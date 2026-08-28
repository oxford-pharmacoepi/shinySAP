# Cohort operations: a plain cohort's logic as data ----------------------------
#
# A denominator cohort has always been typed -- its "logic" IS the generator's
# arguments -- which is why cohort_r_code() can turn one into a call. A plain
# cohort (target, outcome, comparator, censor) was the opposite: three textareas
# of sentences, which is why cohort_r_code() returns NULL for one.
#
# The blocker was never that CohortConstructor lacks the verbs. It is that prose
# does not carry the facts a call needs. Two exit criteria -- "1825 days after
# index" and "End of continuous observation" -- do not say whether the cohort
# ends at the EARLIER or the LATER of them, and no parser recovers a fact nobody
# wrote down. Typed operations force that choice at authoring time, the same way
# study_problems() forces a minimum cell count rather than guessing one.
#
# So a cohort may carry `operations`: an ORDERED list of typed steps, each one
# CohortConstructor verb. Order is the meaning -- requiring first entry before
# and after a date restriction give different cohorts -- so it is a list, not an
# object.
#
#   {"op": "concept_cohort", "codelist": "cs_follicular_lymphoma"}
#   {"op": "require_first_entry"}
#   {"op": "pad_cohort_end", "days": 1825}
#
# `codelist` may name ONE codelist or SEVERAL:
#
#   {"op": "concept_cohort", "codelist": ["cs_follicular_lymphoma", "cs_multiple_myeloma"]}
#
# conceptCohort() creates one cohort per codelist entry, so several named
# together are several cohorts in ONE table -- which is what lets one estimator
# call cover every outcome sharing an exit rule, instead of one call each.
#
# Free text is NOT replaced and is never auto-converted: entry_events /
# inclusion_criteria / exit_criteria stay exactly as they are, and a cohort
# carrying operations renders from them instead. Converting sentences into
# operations behind an author's back would be the guess this design exists to
# avoid.
#
# Signatures these emit against (verified against the package reference):
#
#   conceptCohort(cdm, conceptSet, name, exit, overlap, table, typeConceptId,
#     useRecordsBeforeObservation, useSourceFields, subsetCohort, subsetCohortId)
#   requireIsFirstEntry(cohort, cohortId, indexDate, name)
#   requireDemographics(cohort, cohortId, indexDate, ageRange, sex,
#     minPriorObservation, minFutureObservation, atFirst, name)
#   requireInDateRange(cohort, dateRange, cohortId, indexDate, atFirst, name)
#   exitAtObservationEnd(cohort, cohortId, limitToCurrentPeriod, name)
#   exitAtDeath(cohort, cohortId, requireDeath, name)
#   padCohortEnd(cohort, days, collapse, requireFullContribution, cohortId, name)
#
# Depends on `%||%` (utils.R) and cohort_table_name() (sap_code.R). No Shiny.

# The registry -----------------------------------------------------------------
#
# Same shape as analysis_registry.R and cohort_kinds.R: a partial registry keyed
# by op, and an "unknown op" answer that reports rather than guesses.
#
# prose     function(o) -> one English line for the document
# code      function(o, ctx) -> the R call, or NULL when the op emits none
# validate  function(o, ctx) -> character() of problems
# entry     TRUE for an op that CREATES the cohort rather than transforming one
# package   which library the emitted call comes from, so the generated script
#           can load exactly what it uses. Declared per op rather than assumed:
#           an op added later that reaches for a different package says so here
#           and the script header follows, with nothing else to remember.
# assigns   WHAT the entry op's call returns, and so where it has to be assigned:
#           "table" for a call returning a cohort table (cdm$<name> <- ...), the
#           case for every CohortConstructor verb, or "cdm" for one returning a
#           cdm reference with the table already attached (cdm <- ...). The same
#           split cohort_r_code() documents between conceptCohort() and
#           generateDenominatorCohortSet(): getting it wrong does not fail
#           loudly, it silently leaves the table unattached and every estimate
#           pointing at it dies at the data partner.
COHORT_OP_TEMPLATES <- list()
cohort_op_registry_env <- environment()

register_cohort_op <- function(op, prose, code = function(o, ctx) NULL,
                               validate = function(o, ctx) character(0),
                               entry = FALSE, package = "CohortConstructor",
                               assigns = "table") {
  cohort_op_registry_env$COHORT_OP_TEMPLATES[[op]] <- list(
    prose = prose, code = code, validate = validate,
    entry = entry, package = package, assigns = assigns
  )
}

cohort_op_type <- function(o) {
  v <- as.character(o$op %||% "")[1]
  if (is.na(v)) "" else trimws(v)
}

cohort_op_template <- function(op) COHORT_OP_TEMPLATES[[op]]

# Rendering helpers ------------------------------------------------------------

# A pipeline step on one line when it fits. r_call() always breaks a call across
# lines, which reads well for a ten-argument generator and badly for
# `padCohortEnd(days = 1825)` sitting indented under a pipe.
r_call_inline <- function(fn, args, width = 76) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  one <- sprintf("%s(%s)", fn, paste(sprintf("%s = %s", names(args), unlist(args)),
                                     collapse = ", "))
  if (nchar(one) <= width) one else r_call(fn, args)
}

# Field readers ----------------------------------------------------------------
#
# An operation arrives from JSON, so every field is untyped until read. These
# return NULL for absent rather than a default: an op that omitted a field left
# the package's own default in force, exactly as an omitted argument does in
# cohort_r_code().

op_num <- function(o, key) {
  v <- suppressWarnings(as.numeric(o[[key]] %||% NA))
  if (length(v) != 1 || is.na(v)) NULL else v
}

op_chr <- function(o, key) {
  v <- as.character(o[[key]] %||% NA)[1]
  if (is.na(v) || !nzchar(trimws(v))) NULL else trimws(v)
}

# The same field read as a VECTOR, for a key that may name one thing or several.
#
# JSON carries both shapes naturally -- "cs_x" and ["cs_x", "cs_y"] -- and a
# field that grew a plural reading must still read every SAP written before it
# did. Returns NULL for absent, exactly as op_chr() does, so an omitted field
# still leaves the package default in force.
op_chr_vec <- function(o, key) {
  v <- trimws(as.character(unlist(o[[key]] %||% character(0))))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) NULL else v
}

# A [lower, upper] pair, or a list of them. Indexed with [[ ]] and never
# unlist()ed, for the reason in cohort_kinds.R: a null bound would collapse and
# the pair would silently become length 1.
op_pairs <- function(o, key) {
  v <- o[[key]]
  if (!length(v)) return(NULL)
  # A bare pair ([0, 17]) and a list of pairs ([[0, 17], [18, 150]]) are both
  # accepted, because both read naturally in JSON and ageRange takes a list.
  if (!is.list(v[[1]]) && length(v) == 2) v <- list(v)
  v
}

# The operations ---------------------------------------------------------------

# The one op that CREATES a cohort. Its codelist is a name on the Codelists tab,
# not codes: the concept set lives there once and every cohort citing it refers
# to the same object, which is also what the generated script assigns.
register_cohort_op(
  "concept_cohort",
  entry = TRUE,
  prose = function(o) sprintf(
    "Entry: a record of any concept in [%s]%s%s.",
    paste(op_chr_vec(o, "codelist") %||% "?", collapse = ", "),
    if (!is.null(op_chr(o, "subset_cohort"))) {
      sprintf(", among people in the %s cohort", op_chr(o, "subset_cohort"))
    } else "",
    if (identical(op_chr(o, "exit"), "event_start_date")) {
      ", with the episode ending on the record's start date"
    } else ""),
  code = function(o, ctx) {
    cl <- op_chr_vec(o, "codelist")
    r_call("conceptCohort", list(
      cdm        = "cdm",
      # The R name(s) the concept set was assigned under, not a string: the
      # script builds the codelist object above and hands it in here. Several
      # named together become one codelist -- see concept_set_arg().
      conceptSet = if (is.null(cl)) NULL else concept_set_arg(cl),
      name       = r_string(ctx$table),
      # Only when the author chose the non-default. Where the episode ENDS is
      # what a later pad_cohort_end counts from, so the two decide together
      # whether "N days after index" means what it says.
      exit       = if (identical(op_chr(o, "exit"), "event_start_date")) {
        r_string("event_start_date")
      } else NULL,
      # Build the cohort only among people already in another cohort. A study
      # looking for outcomes AFTER an exposure has no use for the outcome in
      # everyone else, and restricting at creation is far cheaper than creating
      # the whole thing and intersecting afterwards. Named by cohort, resolved to
      # its table name, exactly as a denominator references its target.
      subsetCohort = {
        sub <- op_chr(o, "subset_cohort")
        if (is.null(sub)) NULL else {
          t <- cohort_table_name(sub)
          if (is.na(t)) NULL else r_string(t)
        }
      }
    ))
  },
  validate = function(o, ctx) {
    cl <- op_chr_vec(o, "codelist")
    if (is.null(cl)) {
      return("An entry operation must name the codelist its concepts come from.")
    }
    # Every name is reported, not just the first: an entry citing four codelists
    # with two of them missing should say which two.
    if (length(ctx$codelists)) {
      missing <- cl[!cl %in% ctx$codelists]
      if (length(missing)) {
        return(sprintf("Entry cites [%s], which is not on the Codelists tab.",
                       paste(missing, collapse = ", ")))
      }
    }
    # One cohort per codelist is the whole point of naming several, so two
    # entries under the same name would collide in the generated cohort set.
    if (anyDuplicated(cl)) {
      return(sprintf("Entry names [%s] more than once.",
                     paste(unique(cl[duplicated(cl)]), collapse = ", ")))
    }
    ex <- op_chr(o, "exit")
    if (!is.null(ex) && !ex %in% c("event_end_date", "event_start_date")) {
      return('Entry `exit` must be "event_end_date" or "event_start_date".')
    }
    sub <- op_chr(o, "subset_cohort")
    if (!is.null(sub) && length(ctx$cohorts) && !sub %in% ctx$cohorts) {
      return(sprintf("Restricted to the cohort '%s', which this SAP does not define.", sub))
    }
    character(0)
  }
)

register_cohort_op(
  "require_first_entry",
  prose = function(o) "Keep only each person's first entry ever.",
  code = function(o, ctx) "requireIsFirstEntry()"
)

register_cohort_op(
  "require_demographics",
  prose = function(o) {
    parts <- character(0)
    ages <- op_pairs(o, "age_range")
    if (!is.null(ages)) {
      parts <- c(parts, sprintf("aged %s", paste(vapply(ages, function(a)
        sprintf("%s to %s", a[[1]], if (length(a) >= 2) a[[2]] else "any"),
        character(1)), collapse = " or ")))
    }
    sex <- op_chr(o, "sex")
    if (!is.null(sex) && !identical(sex, "Both")) parts <- c(parts, tolower(sex))
    prior <- op_num(o, "min_prior_observation")
    if (!is.null(prior)) {
      parts <- c(parts, sprintf("with at least %s days of prior observation",
                                format(prior)))
    }
    if (!length(parts)) return("Require demographics (none specified).")
    sprintf("Require people to be %s at index.", paste(parts, collapse = ", "))
  },
  code = function(o, ctx) {
    ages <- op_pairs(o, "age_range")
    r_call_inline("requireDemographics", list(
      ageRange            = if (is.null(ages)) NULL else r_bound_list(ages, always_list = TRUE),
      sex                 = r_chr_vec(op_chr(o, "sex")),
      minPriorObservation = r_num_vec(op_num(o, "min_prior_observation"))
    ))
  },
  validate = function(o, ctx) {
    if (is.null(op_pairs(o, "age_range")) && is.null(op_chr(o, "sex")) &&
        is.null(op_num(o, "min_prior_observation"))) {
      return(paste("A demographics requirement that sets nothing does nothing;",
                   "give an age range, a sex, or a prior observation minimum."))
    }
    sex <- op_chr(o, "sex")
    if (!is.null(sex) && !sex %in% COHORT_SEXES) {
      return(sprintf("Sex must be one of %s.", paste(COHORT_SEXES, collapse = ", ")))
    }
    character(0)
  }
)

register_cohort_op(
  "require_in_date_range",
  prose = function(o) sprintf(
    "Require the index date to fall between %s and %s.",
    op_chr(o, "start") %||% "the start of available data",
    op_chr(o, "end") %||% "the end of available data"),
  code = function(o, ctx) {
    # The argument is one two-element vector whose missing bound is NA, exactly
    # like cohortDateRange -- so it reuses the same renderer.
    r_call_inline("requireInDateRange", list(
      dateRange = r_date_range(list(op_chr(o, "start"), op_chr(o, "end")))))
  },
  validate = function(o, ctx) {
    start <- op_chr(o, "start")
    end   <- op_chr(o, "end")
    if (is.null(start) && is.null(end)) {
      return("A date range requirement needs at least one of a start or an end.")
    }
    bad <- Filter(function(d) is.na(suppressWarnings(as.Date(d, optional = TRUE))),
                  Filter(Negate(is.null), list(start, end)))
    if (length(bad)) {
      return(sprintf("'%s' is not a date the package can read (use YYYY-MM-DD).", bad[[1]]))
    }
    if (!is.null(start) && !is.null(end) && as.Date(start) > as.Date(end)) {
      return(sprintf("The date range starts (%s) after it ends (%s).", start, end))
    }
    character(0)
  }
)

register_cohort_op(
  "exit_at_observation_end",
  prose = function(o) "Exit at the end of continuous observation.",
  code = function(o, ctx) "exitAtObservationEnd()"
)

register_cohort_op(
  "exit_at_death",
  prose = function(o) "Exit at death.",
  code = function(o, ctx) "exitAtDeath()"
)

# The op that answers the "earliest of" question the free-text form could not.
# padCohortEnd() documents that "if the days added means that cohort end would be
# after observation period end date, then observation period end date will be
# used for cohort exit" -- so the clamp is the function's own behaviour, not
# something the generated script has to arrange, and the plan can say so.
#
# It adds days to the cohort END, not to entry. "1825 days after index" is
# therefore this op PLUS an entry that exits on the record's start date
# (conceptCohort(exit = "event_start_date")), and the prose says "extend the exit
# date" rather than "after entry" so the two are not confused for one another.
register_cohort_op(
  "pad_cohort_end",
  prose = function(o) sprintf(
    paste("Extend the exit date by %s days, or to the end of continuous",
          "observation, whichever comes first."),
    format(op_num(o, "days") %||% NA)),
  code = function(o, ctx) r_call_inline("padCohortEnd", list(days = r_num_vec(op_num(o, "days")))),
  validate = function(o, ctx) {
    d <- op_num(o, "days")
    if (is.null(d)) return("An exit-after-days operation must say how many days.")
    if (d < 0) return("Days after entry cannot be negative.")
    character(0)
  }
)

# Several cohort definitions gathered into ONE table -----------------------------
#
# The op that lets one estimator call cover what would otherwise be several.
#
# estimatePrevalence()'s `outcomeTable` takes ONE table -- passing a vector fails
# with "You can only read one table of a cdm_reference" -- but a table may hold
# many cohorts, and the estimator runs across all of them in a single call,
# labelling each in the result's group_level. That is already why C1-001's six
# cancers are one analysis rather than six: conceptCohort() puts one cohort per
# codelist in one table.
#
# What it could NOT do was combine definitions built by different pipelines --
# two outcome families defined from different codelists, say, that one estimate
# should cover. omopgenerics::bind() joins their tables, renumbering cohort ids
# in argument order, so one call covers them all.
#
# The constraint that decides what may merge: the constituents' COHORT NAMES
# must be disjoint. bind() aborts on a duplicated cohort_name -- and even if it
# did not, the estimator labels its results by cohort_name, so two definitions
# under one name would be indistinguishable in the output. conceptCohort() names
# one cohort per codelist, AFTER the codelist, which is why definitions that
# differ only in exit (C1-001's 5-year / 2-year / complete prevalence trio) do
# not merge: same codelists, same names. bound_cohort_problems() reports the
# collision, since no single op can see across tables.
#
# Verified against omopgenerics 1.4.1: bind() on cohort tables returns a CDM
# REFERENCE with the combined table attached (hence assigns = "cdm"), the ids are
# renumbered in argument order, and one estimatePeriodPrevalence() over the
# result reports each constituent cohort separately.
register_cohort_op(
  "bind_cohorts",
  entry   = TRUE,
  assigns = "cdm",
  # Namespaced rather than library()'d, exactly as suppression_r_code() writes
  # omopgenerics::suppress(): the script needs no import for one call.
  package = NULL,
  prose = function(o) {
    nms <- sprintf("'%s'", op_chr_vec(o, "cohorts") %||% "?")
    listed <- if (length(nms) < 2) nms else paste(
      paste(nms[-length(nms)], collapse = ", "), "and", nms[[length(nms)]])
    sprintf("Entry: every cohort of %s, combined into one table so one estimate covers them all.",
            listed)
  },
  code = function(o, ctx) {
    nms <- op_chr_vec(o, "cohorts") %||% character(0)
    tbls <- vapply(nms, function(n) {
      t <- cohort_table_name(n)
      if (is.na(t)) "cohort" else t
    }, character(1), USE.NAMES = FALSE)
    args <- c(sprintf("cdm$%s", tbls), sprintf("name = %s", r_string(ctx$table)))
    one <- sprintf("omopgenerics::bind(%s)", paste(args, collapse = ", "))
    if (nchar(one) <= 88) return(one)
    sprintf("omopgenerics::bind(\n%s\n)", paste(sprintf("  %s", args), collapse = ",\n"))
  },
  validate = function(o, ctx) {
    nms <- op_chr_vec(o, "cohorts")
    if (is.null(nms)) {
      return("A bind operation must name the cohorts it combines.")
    }
    # One cohort bound to nothing is a copy of that cohort under another name --
    # legal R, but never what an author meant to write down.
    if (length(nms) < 2) {
      return("A bind operation combines two or more cohorts; naming one only copies it.")
    }
    if (anyDuplicated(nms)) {
      return(sprintf("Binds '%s' more than once.",
                     paste(unique(nms[duplicated(nms)]), collapse = "', '")))
    }
    # Binding the cohort into itself: the table would be read as an argument to
    # the call that creates it.
    if (any(vapply(nms, function(n) identical(cohort_table_name(n), ctx$table),
                   logical(1)))) {
      return("A bind operation cannot include the cohort it defines.")
    }
    if (length(ctx$cohorts)) {
      missing <- nms[!nms %in% ctx$cohorts]
      if (length(missing)) {
        return(sprintf("Binds '%s', which this SAP does not define.",
                       paste(missing, collapse = "', '")))
      }
    }
    character(0)
  }
)

# The escape hatch, and the reason the vocabulary can stay small. An operation
# the registry does not cover is written out as free text and emitted as a TODO
# comment -- visible in the script, never silently dropped. The same answer the
# "Other" analysis type gives.
register_cohort_op(
  "custom",
  package = NULL,
  prose = function(o) op_chr(o, "text") %||% "Custom step (not described).",
  code = function(o, ctx) NULL,
  validate = function(o, ctx) {
    if (is.null(op_chr(o, "text"))) "A custom step must describe what it does."
    else character(0)
  }
)

# Concept sets in the generated script -----------------------------------------

# The R variable a codelist is assigned to. Derived from the codelist's own name
# so the script reads as the SAP does, and deterministic so the assignment above
# and the reference below cannot disagree.
concept_set_var <- function(name) {
  v <- cohort_table_name(name)
  if (is.na(v)) "concept_set" else v
}

# The conceptSet= argument for one entry operation, which may name SEVERAL
# codelists.
#
# A codelist renders as a NAMED LIST of concept ids (see concept_set_r_code), so
# c() of several is one codelist carrying one entry per element -- and
# conceptCohort() creates ONE COHORT PER ENTRY in a single table. That is the
# whole reason an entry may name more than one: six cancers sharing an exit rule
# are six cohorts in one table, not six tables, and the estimators then take the
# lot in a single call.
concept_set_arg <- function(names) {
  vars <- vapply(names, concept_set_var, character(1), USE.NAMES = FALSE)
  if (length(vars) == 1) vars else sprintf("c(%s)", paste(vars, collapse = ", "))
}

# One codelist as the assignment the conceptCohort(conceptSet =) call below
# needs -- whose path the script cannot know, because the SAP names a codelist
# without carrying its codes. Emitted as runnable code with a placeholder path
# rather than as a comment: the entry call references the variable either way,
# and an unfilled placeholder fails loudly at the importCodelist() call, with
# this comment as the trail back to the plan, instead of at a bare reference
# nothing explains.
#
# A PLACEHOLDER, deliberately not the study export's by-category path: the
# document's appendix is read before any study directory exists -- laying out
# that directory is OmopStudyBuilder's job, and the export names the real path
# there (see codelist_csv_path() in sap_study_export.R, which the preview's
# render session does not load).
concept_set_r_code <- function(cl) {
  nm  <- as.character(cl$name %||% "")[1]
  var <- concept_set_var(nm)
  sprintf(paste0(
    "# TODO: point `path` at the CSV (concept_id, concept_name) holding the\n",
    "#   codes for '%s' -- the SAP names it but does not carry them.\n",
    "%s <- omopgenerics::importCodelist(path = \"<path/to/%s.csv>\", type = \"csv\")"),
    nm, var, var)
}

# Rendering --------------------------------------------------------------------

# A cohort's operations as the pipeline that builds it, or NULL when it carries
# none (which is every cohort authored as free text -- see the header).
#
# The entry op seeds the pipeline and the rest are piped onto it, which is how
# CohortConstructor is written: verbs take a cohort and return one. Assigned into
# `cdm$<table>` so the table the estimators point at is the one this created.
cohort_operations_code <- function(ch, codelists = list(), cohorts = character(0)) {
  ops <- ch$operations %||% list()
  if (!length(ops)) return(NULL)
  tbl <- cohort_table_name(ch$name)
  ctx <- list(table = if (is.na(tbl)) "cohort" else tbl,
              codelists = codelists, cohorts = cohorts)

  rendered <- lapply(ops, function(o) {
    tmpl <- cohort_op_template(cohort_op_type(o))
    if (is.null(tmpl)) {
      return(list(code = NULL, note = sprintf(
        "No cohort operation is registered as '%s'.", cohort_op_type(o))))
    }
    note <- if (identical(cohort_op_type(o), "custom")) op_chr(o, "text") else NULL
    list(code = tmpl$code(o, ctx), note = note, entry = isTRUE(tmpl$entry),
         assigns = tmpl$assigns %||% "table")
  })

  # An operation with no call becomes a comment in place, so a step the author
  # asked for is never silently missing from the script.
  as_line <- function(r) {
    if (!is.null(r$code)) return(r$code)
    sprintf("# TODO: %s", r$note %||% "this step has no generated call.")
  }

  first  <- rendered[[1]]
  head_line <- as_line(first)
  # An entry op whose call returns a CDM REFERENCE rather than a cohort table
  # (bind_cohorts) cannot be the head of a `cdm$x <- ... |> ...` pipeline: the
  # table is attached by the call itself. It becomes its own statement, and any
  # steps after it pipe from the attached table instead.
  if (identical(first$assigns, "cdm")) {
    out <- sprintf("cdm <- %s", head_line)
    if (length(rendered) > 1) {
      steps <- vapply(rendered[-1], function(r) {
        sprintf("  %s", gsub("\n", "\n  ", as_line(r), fixed = TRUE))
      }, character(1))
      out <- paste0(out, sprintf("\ncdm$%s <- cdm$%s |>\n%s",
                                 ctx$table, ctx$table, paste(steps, collapse = " |>\n")))
    }
    return(out)
  }
  # Without an entry op there is no cohort to pipe onto: the script says so
  # rather than emitting a pipeline that starts from nothing.
  if (!isTRUE(first$entry)) {
    head_line <- paste0(
      "# TODO: this cohort has no entry operation, so nothing creates it.\n",
      head_line)
  }
  # A step that did not fit on one line is a multi-line call, and every line of
  # it sits inside the pipeline -- so the continuations are indented too. Without
  # this the closing paren of a wrapped requireDemographics() lands in column 1,
  # under the pipe rather than inside it.
  rest <- vapply(rendered[-1], function(r) {
    sprintf("  %s", gsub("\n", "\n  ", as_line(r), fixed = TRUE))
  }, character(1))

  body <- if (length(rest)) {
    paste0(head_line, " |>\n", paste(rest, collapse = " |>\n"))
  } else {
    head_line
  }
  sprintf("cdm$%s <- %s", ctx$table, body)
}

# The cohort NAMES a built table will hold, as far as the plan can know them.
#
# conceptCohort() names one cohort per codelist, after the codelist; a bound
# table holds the union of its constituents'. Anything else -- a prose cohort, an
# entry the registry cannot see through -- returns NULL: "unknown", which is a
# different answer from "none", so a collision is only ever reported between
# tables whose names are actually known.
cohort_set_names <- function(co, cohorts, nms, seen = character(0)) {
  ops <- co$operations %||% list()
  if (!length(ops)) return(NULL)
  entry <- ops[[1]]
  if (identical(cohort_op_type(entry), "concept_cohort")) {
    return(op_chr_vec(entry, "codelist"))
  }
  if (identical(cohort_op_type(entry), "bind_cohorts")) {
    nm <- as.character(co$name %||% "")[1]
    if (nm %in% seen) return(NULL)  # a bind cycle; the order check reports it
    parts <- lapply(op_chr_vec(entry, "cohorts") %||% character(0), function(ref) {
      j <- match(ref, nms)
      if (is.na(j)) return(NULL)
      cohort_set_names(cohorts[[j]], cohorts, nms, c(seen, nm))
    })
    # One unknown constituent makes the whole table unknown: reporting a
    # collision on a partial listing could accuse the wrong pair.
    if (any(vapply(parts, is.null, logical(1)))) return(NULL)
    return(unlist(parts))
  }
  NULL
}

# What a bind can get wrong that its own validate() cannot see -------------------
#
# An operation validates against a context of NAMES, so it can tell that a bound
# cohort exists. It cannot tell whether that cohort is built, or built BEFORE it
# -- both of which are properties of the whole cohort list, and both of which end
# the same way: `cdm$<table>` read as an argument to a call, at a data partner,
# for a table nothing has created yet.
#
# Order matters because the script emits cohorts in the order the SAP lists them.
# Reordering silently would be the guess this design exists to avoid, so the plan
# is asked to say it in the order it means.
#
# The fourth check is the one your own bind() call would make for you, too late
# and too far away: constituents whose tables hold the SAME cohort name cannot
# combine. omopgenerics::bind() aborts on a duplicated cohort_name, and even if
# it did not, the estimator labels results by cohort_name, so the estimates from
# two same-named definitions could never be told apart.
bound_cohort_problems <- function(cohorts) {
  cohorts <- cohorts %||% list()
  if (!length(cohorts)) return(list())
  nms <- vapply(cohorts, function(co) as.character(co$name %||% "")[1], character(1))
  # Built = something in the generated script creates the table.
  built <- vapply(cohorts, function(co) {
    !is.null(cohort_operations_code(co)) || !is.null(cohort_r_code(co))
  }, logical(1))
  denom <- vapply(cohorts, function(co) is_denominator_kind(co$kind), logical(1))

  found <- list()
  for (i in seq_along(cohorts)) {
    ops <- Filter(function(o) identical(cohort_op_type(o), "bind_cohorts"),
                  cohorts[[i]]$operations %||% list())
    if (!length(ops)) next
    refs <- unique(unlist(lapply(ops, function(o) op_chr_vec(o, "cohorts") %||% character(0))))
    msgs <- character(0)
    for (ref in refs) {
      j <- match(ref, nms)
      if (is.na(j)) next            # the op's own validate() reports this one
      if (denom[[j]]) {
        msgs <- c(msgs, sprintf(paste(
          "Binds '%s', which is a generated denominator cohort set. A denominator",
          "is the population an estimate runs ON, never one of its outcomes."), ref))
      } else if (!built[[j]]) {
        msgs <- c(msgs, sprintf(paste(
          "Binds '%s', which the generated script never creates -- it is a plain",
          "cohort with no typed operations, so there is no table to bind."), ref))
      } else if (j > i) {
        msgs <- c(msgs, sprintf(paste(
          "Binds '%s', which this SAP defines AFTER it. The script builds cohorts",
          "in the order they are listed, so move '%s' above this one."), ref, ref))
      }
    }
    # Constituents that are otherwise fine but hold the same cohort name. Only
    # tables whose names are KNOWN take part: an unresolvable or unknowable
    # constituent is either reported above or beyond what a plan can check.
    inner <- list()
    for (ref in refs) {
      j <- match(ref, nms)
      if (is.na(j) || denom[[j]] || !built[[j]]) next
      v <- cohort_set_names(cohorts[[j]], cohorts, nms)
      if (!is.null(v)) inner[[ref]] <- v
    }
    if (length(inner) > 1) {
      dup <- unlist(inner, use.names = FALSE)
      dup <- unique(dup[duplicated(dup)])
      holders <- names(inner)[vapply(inner, function(v) any(v %in% dup), logical(1))]
      if (length(dup) && length(holders) > 1) {
        quoted <- sprintf("'%s'", holders)
        listed <- paste(paste(quoted[-length(quoted)], collapse = ", "),
                        "and", quoted[[length(quoted)]])
        msgs <- c(msgs, sprintf(paste(
          "Binds %s, whose tables each hold a cohort named [%s]. bind() refuses",
          "a duplicated cohort name -- and the estimates it labels would be",
          "indistinguishable anyway. Definitions built from the same codelists",
          "stay separate analyses."),
          listed, paste(dup, collapse = ", ")))
      }
    }
    if (length(msgs)) {
      found[[length(found) + 1]] <- list(name = nms[[i]] %||% "Untitled cohort",
                                         messages = msgs)
    }
  }
  found
}

# The codelists a cohort's typed entries actually enter on.
#
# A codelist cited only in free text is documentation, not an input: it earns a
# mention in the document and no assignment in the code. Both the standalone
# script and the study directory need this same answer, which is why it lives
# here rather than being worked out again at each call site.
cited_codelist_names <- function(cohorts) {
  refs <- unlist(lapply(cohorts %||% list(), function(co) {
    lapply(Filter(function(o) identical(cohort_op_type(o), "concept_cohort"),
                  co$operations %||% list()),
           function(o) op_chr_vec(o, "codelist") %||% character(0))
  }))
  refs <- as.character(refs %||% character(0))
  unique(refs[nzchar(refs)])
}

# The packages a cohort's operations actually call, so the script header loads
# what it uses and nothing more. An op that emits no call contributes none.
cohort_operations_packages <- function(ch) {
  ops <- ch$operations %||% list()
  if (!length(ops)) return(character(0))
  pkgs <- unlist(lapply(ops, function(o) {
    tmpl <- cohort_op_template(cohort_op_type(o))
    if (is.null(tmpl) || is.null(tmpl$code(o, list(table = "x", codelists = NULL)))) {
      return(NULL)
    }
    tmpl$package
  }))
  unique(as.character(pkgs %||% character(0)))
}

# The same operations as prose, for the document.
#
# THE DIRECTION MATTERS: the readable description is generated FROM the typed
# operations, not parsed out of a written one. That is the whole inversion --
# denominator_facts() in the preview already does it for denominators, and this
# is the same move for plain cohorts. One source, so the sentences a reviewer
# reads and the code the study runs cannot describe different cohorts.
cohort_operations_prose <- function(ch) {
  ops <- ch$operations %||% list()
  if (!length(ops)) return(character(0))
  vapply(ops, function(o) {
    tmpl <- cohort_op_template(cohort_op_type(o))
    if (is.null(tmpl)) {
      return(sprintf("Unrecognised step '%s'.", cohort_op_type(o)))
    }
    as.character(tmpl$prose(o))[1]
  }, character(1))
}

# Everything wrong with one cohort's operations. Shaped like the other
# validators: character() of messages, warn-not-block.
cohort_operations_problems <- function(ch, codelist_names = character(0),
                                       cohort_names = character(0)) {
  ops <- ch$operations %||% list()
  if (!length(ops)) return(character(0))
  ctx  <- list(table = cohort_table_name(ch$name), codelists = codelist_names,
               cohorts = cohort_names)
  errs <- character(0)

  for (i in seq_along(ops)) {
    o    <- ops[[i]]
    type <- cohort_op_type(o)
    tmpl <- cohort_op_template(type)
    if (is.null(tmpl)) {
      errs <- c(errs, sprintf(
        "Step %d is '%s', which is not a cohort operation this app knows.", i, type))
      next
    }
    found <- tryCatch(as.character(tmpl$validate(o, ctx)),
                      error = function(e) paste("Could not validate step", i))
    if (length(found)) errs <- c(errs, sprintf("Step %d: %s", i, found))
  }

  # An entry op is what creates the cohort, so exactly one belongs at the front.
  entries <- which(vapply(ops, function(o) {
    tmpl <- cohort_op_template(cohort_op_type(o))
    !is.null(tmpl) && isTRUE(tmpl$entry)
  }, logical(1)))
  if (!length(entries)) {
    errs <- c(errs, "These operations never create the cohort; the first step should be an entry.")
  } else {
    if (length(entries) > 1) {
      errs <- c(errs, sprintf(
        "Steps %s each create the cohort; only the first step can.",
        paste(entries, collapse = " and ")))
    } else if (entries[[1]] != 1) {
      errs <- c(errs, sprintf(
        "Step %d creates the cohort, so it has to come first.", entries[[1]]))
    }
  }
  errs
}
