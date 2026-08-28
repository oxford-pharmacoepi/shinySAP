# Generated R code: a SAP item -> the IncidencePrevalence call it describes -----
#
# The JSON already names its keys after the estimators' and generators' arguments
# (see the header of analysis_type_incidence.R). That was always the point: a SAP
# should be executable, not merely readable. This file closes the last step --
# rendering those keys as the call itself, so the document a reviewer signs off
# and the code the study runs are the same thing.
#
# Signatures these emit against (verified against the package reference):
#
#   generateDenominatorCohortSet(cdm, name, cohortDateRange, ageGroup, sex,
#     daysPriorObservation, requirementInteractions)
#
#   generateTargetDenominatorCohortSet(cdm, name, targetCohortTable,
#     targetCohortId, cohortDateRange, timeAtRisk, ageGroup, sex,
#     daysPriorObservation, requirementsAtEntry, requirementInteractions)
#
#   estimatePointPrevalence(cdm, denominatorTable, outcomeTable,
#     denominatorCohortId, outcomeCohortId, interval, timePoint, strata,
#     includeOverallStrata)
#
#   estimatePeriodPrevalence(cdm, denominatorTable, outcomeTable,
#     denominatorCohortId, outcomeCohortId, interval, completeDatabaseIntervals,
#     fullContribution, level, strata, includeOverallStrata)
#
#   estimateIncidence(cdm, denominatorTable, outcomeTable, censorTable,
#     denominatorCohortId, outcomeCohortId, censorCohortId, interval,
#     completeDatabaseIntervals, outcomeWashout, repeatedEvents, strata,
#     includeOverallStrata)
#
# Arguments are emitted IN SIGNATURE ORDER, and an argument the author never
# decided is OMITTED rather than filled with the package default. That is the same
# rule the cards follow (see cohort_kinds.R): writing `sex = "Both"` for an
# untouched field would put a decision in the code that nobody made. The omission
# is visible -- the call simply falls back to the documented default -- and the
# cohort validators already flag an empty ageGroup / sex / daysPriorObservation as
# a problem, so a gap here is never silent.
#
# Depends only on base R, `%||%` (utils.R) and the cohort kind helpers
# (cohort_kinds.R). No Shiny: the preview document sources this directly.

# R literals -------------------------------------------------------------------

r_string <- function(x) sprintf('"%s"', gsub('"', '\\\\"', as.character(x)))

# Whole numbers print as 0 and 150, not 0.0 or 1.5e+02. Inf survives as Inf --
# the package's own open upper bound -- and JSON null arrives as NULL, which the
# pair helpers below resolve before this ever sees it.
r_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || is.na(x)) return("NA")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

r_logical <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"

# A character vector: one value bare, several wrapped in c(). NULL for empty, so
# r_call() drops the argument entirely.
r_chr_vec <- function(x) {
  v <- as.character(unlist(x %||% character(0)))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NULL)
  if (length(v) == 1) return(r_string(v))
  sprintf("c(%s)", paste(vapply(v, r_string, character(1)), collapse = ", "))
}

r_num_vec <- function(x) {
  v <- suppressWarnings(as.numeric(unlist(x %||% numeric(0))))
  v <- v[!is.na(v)]
  if (!length(v)) return(NULL)
  if (length(v) == 1) return(r_number(v))
  sprintf("c(%s)", paste(vapply(v, r_number, character(1)), collapse = ", "))
}

# One [lower, upper] pair as c(lo, hi). Indexed with [[ ]] and never unlist()ed:
# an unbounded upper bound travels as JSON null, and unlist() would collapse it
# so the pair silently became length 1 (the warning in cohort_kinds.R).
r_bounds <- function(pair) {
  if (!length(pair)) return(NULL)
  lo <- suppressWarnings(as.numeric(pair[[1]]))
  hi <- bound_upper(pair)
  sprintf("c(%s, %s)", r_number(lo), r_number(hi))
}

# ageGroup and timeAtRisk are both lists of pairs. One pair renders bare -- which
# is how both signatures write their own defaults, c(0, Inf) and list(c(0, 150))
# -- and several as a list().
r_bound_list <- function(pairs, always_list = FALSE) {
  pairs <- pairs %||% list()
  rendered <- Filter(Negate(is.null), lapply(pairs, r_bounds))
  if (!length(rendered)) return(NULL)
  if (length(rendered) == 1 && !always_list) return(rendered[[1]])
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

# cohortDateRange takes two dates in one argument, and a missing bound is NA --
# which is what the signature's own default, as.Date(c(NA, NA)), is made of.
r_date_range <- function(dr) {
  if (!length(dr)) return(NULL)
  bound <- function(i) {
    v <- date_bound(dr, i)
    if (is.null(v)) "NA" else r_string(v)
  }
  lo <- bound(1)
  hi <- bound(2)
  if (lo == "NA" && hi == "NA") return(NULL)   # the default; nothing was decided
  sprintf("as.Date(c(%s, %s))", lo, hi)
}

# strata is a list of variable GROUPS: list("sex", c("age_group", "sex")) means
# one stratification by sex and another by their cross. The nesting is the whole
# meaning, so a group is only unwrapped when it really holds one variable.
r_strata <- function(groups) {
  groups <- groups %||% list()
  rendered <- Filter(Negate(is.null), lapply(groups, r_chr_vec))
  if (!length(rendered)) return(NULL)
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

# Split "a, c(0, 9), c(10, 19)" on its TOP-LEVEL commas only, so a nested c()
# pair is never torn in half.
split_top_level <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0
  out   <- character(0)
  cur   <- character(0)
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- depth - 1
    if (ch == "," && depth == 0) {
      out <- c(out, paste(cur, collapse = ""))
      cur <- character(0)
    } else {
      cur <- c(cur, ch)
    }
  }
  trimws(c(out, paste(cur, collapse = "")))
}

# A long list() argument -- fourteen age groups, say -- broken across lines and
# aligned under its opening paren, so the emitted script stays readable at the
# width a Word page and an R script both assume.
r_wrap_value <- function(value, prefix, width = 92) {
  # list() and c() wrap the same way -- only the opening token differs, and its
  # width is what the continuation lines line up under. c() earns this because an
  # entry naming six codelists renders as one c() of six variable names.
  head <- if (grepl("^list\\(", value)) "list(" else if (grepl("^c\\(", value)) "c(" else NULL
  if (is.null(head) || nchar(prefix) + nchar(value) <= width) return(value)
  inner <- substr(value, nchar(head) + 1L, nchar(value) - 1L)
  parts <- split_top_level(inner)
  if (length(parts) < 2) return(value)
  pad <- strrep(" ", nchar(prefix) + nchar(head))
  wrapped <- character(0)
  line    <- head
  # Only the FIRST line sits after the "  argName = " prefix; every continuation
  # line already carries that width as leading pad, so counting the prefix again
  # would wrap them far too early.
  first <- TRUE
  for (i in seq_along(parts)) {
    piece <- paste0(parts[[i]], if (i < length(parts)) "," else "")
    open  <- grepl("\\($", line)                    # nothing on this line yet
    used  <- nchar(line) + if (first) nchar(prefix) else 0L
    sep   <- if (open) "" else " "
    if (!open && used + nchar(sep) + nchar(piece) > width) {
      wrapped <- c(wrapped, line)
      line    <- paste0(pad, piece)
      first   <- FALSE
    } else {
      line <- paste0(line, sep, piece)
    }
  }
  paste0(paste(c(wrapped, line), collapse = "\n"), ")")
}

# fn(\n  arg = value,\n  ...\n). Arguments that rendered NULL -- undecided, so
# left to the package default -- drop out here.
r_call <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  pad <- max(nchar(names(args)))
  lines <- vapply(seq_along(args), function(i) {
    prefix <- sprintf("  %-*s = ", pad, names(args)[[i]])
    paste0(prefix, r_wrap_value(args[[i]], prefix))
  }, character(1))
  paste0(fn, "(\n", paste(lines, collapse = ",\n"), "\n)")
}

# Cohort display name -> the CDM table name the calls refer to ------------------
#
# Every cohort reference in the JSON is a display name ("General population
# denominator"), because that is what the author picked from. The package wants a
# table name, so the two are bridged here rather than by asking the author to
# maintain a second name by hand. Deterministic, so the name a generator call
# creates is the same string the estimator call points at.
cohort_table_name <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")[1]))
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) return(NA_character_)
  if (grepl("^[0-9]", s)) s <- paste0("c_", s)
  # 63 is Postgres's identifier limit, the tightest of the databases the CDM
  # runs on. Two long names can still truncate onto each other, so the collision
  # is reported rather than silently generated -- see table_name_collisions().
  substr(s, 1, 63)
}

# Data sources as the generated script's own guard -------------------------------
#
# `data_sources` is the SAP-level counterpart of `cdm`: WHICH databases a cohort
# is built in and an estimate is run against. It is the one field on an analysis
# that reached the document and stopped there -- the script ran every estimate at
# every data partner, so a plan restricting an analysis to two of five databases
# generated code that ignored the restriction. That is the dangerous direction:
# the plan a reviewer signed said one thing and the code exported another.
#
# A DARWIN study package ships ONE script to every partner and each runs it
# against their own CDM, so the restriction has to be a run-time test rather than
# a build-time one -- hence omopgenerics::cdmName(cdm), which returns the name the
# cdm reference was created with. Namespaced rather than library()'d, the same
# way suppression_r_code() writes omopgenerics::suppress().
#
# THE NAMES MUST LINE UP. The guard compares this SAP's source keys against
# whatever codeToRun.R passed as cdmName, and nothing here can check that -- so
# study_export_notes() says so whenever a guard is emitted.

# The source keys a SAP names, exactly as the pickers offer them: the short key,
# falling back to the full name (see names_r in mod_cdm_sources.R). The two must
# agree or an item's data_sources could never match the list it chose from.
sap_source_keys <- function(sap) {
  vapply(sap$cdm_sources %||% list(), function(s) {
    key <- trimws(as.character(s$source_key %||% "")[1])
    if (!is.na(key) && nzchar(key)) key else trimws(as.character(s$name %||% "")[1])
  }, character(1))
}

item_sources <- function(x) {
  v <- trimws(as.character(unlist(x$data_sources %||% character(0))))
  unique(v[!is.na(v) & nzchar(v)])
}

# TRUE when an item names a PROPER SUBSET of the study's sources -- the only case
# a guard says anything. An item that runs everywhere, or that names no sources at
# all (the author never said, which this app reads as no restriction), is emitted
# bare: wrapping every call in a test that is always true would bury the two that
# are not.
is_source_restricted <- function(item, all_sources) {
  keys <- item_sources(item)
  if (!length(keys) || !length(all_sources)) return(FALSE)
  !setequal(keys, all_sources)
}

indent_block <- function(code) paste0("  ", gsub("\n", "\n  ", code))

# Always c(), even for one source. r_chr_vec() drops the c() at length 1, which
# is right for interval = "years" and wrong here: `%in% c("SIDIAP")` reads as the
# membership test it is, and stays diff-stable when a second database is added.
r_chr_c <- function(x) sprintf("c(%s)", paste(vapply(
  as.character(unlist(x)), r_string, character(1)), collapse = ", "))

# A SIDE-EFFECTING block -- a cohort, a denominator set -- under its guard. Not
# creating the table at a partner the plan excludes is the whole point, so there
# is nothing to put in an else.
source_guard <- function(code, item, all_sources) {
  if (is.null(code) || !is_source_restricted(item, all_sources)) return(code)
  sprintf("if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n}",
          r_chr_c(item_sources(item)), indent_block(code))
}

# An ESTIMATE under its guard, which needs the other branch.
#
# The estimates are bound together at the end (suppression_r_code), so every
# variable the script binds has to EXIST at every partner -- a bare `if` would
# leave it undefined wherever the guard is false and take down bind() with
# "object not found", turning a restriction the plan states into a study that
# crashes at three partners out of five. omopgenerics::emptySummarisedResult() is
# a valid empty summarised_result, so it binds with the rest and contributes no
# rows: the analysis simply produced nothing there, which is what the plan says.
source_guard_estimate <- function(var, code, item, all_sources) {
  if (!is_source_restricted(item, all_sources)) {
    return(sprintf("%s <- %s", var, code))
  }
  sprintf(paste0("%s <- if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n} else {\n",
                 "  # Not run here: this SAP restricts the analysis to the databases above.\n",
                 "  omopgenerics::emptySummarisedResult()\n}"),
          var, r_chr_c(item_sources(item)), indent_block(code))
}

# Two cohorts whose names collapse to ONE table name. The generated script would
# then create a table twice and every estimate on the first would silently read
# the second. Shaped like the other entries in problems_r (see cohort_kinds.R).
table_name_collisions <- function(cohorts) {
  nms <- vapply(cohorts, function(co) as.character(co$name %||% "")[1], character(1))
  nms <- nms[!is.na(nms) & nzchar(nms)]
  tbl <- vapply(nms, function(n) cohort_table_name(n) %||% "", character(1))
  keep <- nzchar(tbl)
  nms <- nms[keep]
  tbl <- tbl[keep]
  lapply(unique(tbl[duplicated(tbl)]), function(t) list(
    name = t,
    messages = sprintf(
      paste("These cohorts all map onto the table name '%s' in the generated code:",
            "%s. Rename one so each cohort gets its own table."),
      t, paste(sprintf("'%s'", nms[tbl == t]), collapse = ", "))
  ))
}

# Cohorts an estimator points at that the generated script never creates.
#
# An estimator call names a cohort TABLE, and something has to have created it. A
# denominator cohort creates one (generateDenominatorCohortSet); so does a plain
# cohort carrying typed operations (conceptCohort and the verbs after it). A
# plain cohort with NEITHER -- prose alone -- creates nothing, which cohort_r_code()
# documents as legitimate: such a cohort is instantiated outside
# IncidencePrevalence, by hand or by a study's own script.
#
# Legitimate, but not free. The script still emits the estimator call, so run as
# it stands it fails at the data partner with "<table> does not exist in the
# cdm_reference object" -- and nothing anywhere said so. Every other gap in the
# generated code is reported: a colliding table name above, an uncarried
# codelist as a TODO, an unregistered operation as a comment in place. This one
# was the exception, and it is the one that stops the script dead.
#
# A reference to a cohort NOBODY DEFINED lands here too. It is a different
# mistake with the same ending -- a call against a table the script never creates
# -- and no other check reports it.
#
# Grouped by the missing cohort rather than by the analysis, because that is
# where the fix goes: nine analyses sharing one uninstantiated outcome are one
# problem, not nine.
#
# Analyses whose type maps onto no estimator (the "Other" template) are skipped:
# they emit no call, so they cannot point at a table that is missing.
uninstantiated_cohort_problems <- function(cohorts, analyses) {
  cohorts  <- cohorts %||% list()
  analyses <- analyses %||% list()

  nms <- vapply(cohorts, function(co) as.character(co$name %||% "")[1], character(1))
  ok  <- !is.na(nms) & nzchar(nms)
  # NULL from BOTH renderers is the whole test. The extra arguments
  # cohort_operations_code() takes only shape the code it emits, never whether
  # there is any, so the defaults are safe here.
  made <- vapply(cohorts, function(co) {
    !is.null(cohort_operations_code(co)) || !is.null(cohort_r_code(co))
  }, logical(1))
  defined <- nms[ok]
  created <- nms[ok & made]

  # cohort name -> the analyses naming it, for the analyses that emit a call.
  refs <- list()
  for (a in analyses) {
    if (is.na(analysis_estimator(a$analysis_type))) next
    p <- a$parameters %||% list()
    for (id in as.character(analysis_template(a$analysis_type)$pickers$cohorts %||% character(0))) {
      nm <- as.character(p[[id]] %||% "")[1]
      if (is.na(nm)) next
      nm <- trimws(nm)
      # An empty picker is "not chosen yet", which is the template's own problem
      # to report, not a missing table.
      if (!nzchar(nm) || nm %in% created) next
      refs[[nm]] <- c(refs[[nm]], as.character(a$name %||% "(unnamed analysis)")[1])
    }
  }

  naming <- function(x) {
    x <- unique(x)
    if (length(x) <= 4) return(paste(sprintf("'%s'", x), collapse = ", "))
    sprintf("%s, and %d more", paste(sprintf("'%s'", x[1:4]), collapse = ", "), length(x) - 4)
  }

  lapply(names(refs), function(nm) {
    tbl <- cohort_table_name(nm)
    tbl <- if (is.na(tbl)) nm else tbl
    list(name = nm, messages = if (nm %in% defined) {
      sprintf(paste("%s is named by %s, but the generated script never creates it:",
                    "it is a plain cohort with no typed operations, so nothing",
                    "instantiates the table '%s'. Give it operations, or create",
                    "'%s' yourself before the script runs."),
              sprintf("'%s'", nm), naming(refs[[nm]]), tbl, tbl)
    } else {
      sprintf(paste("%s is named by %s, but this SAP defines no cohort by that",
                    "name, so the script never creates the table '%s'."),
              sprintf("'%s'", nm), naming(refs[[nm]]), tbl)
    })
  })
}

# Plan fields carried into the RESULT ------------------------------------------
#
# An analysis's four non-`parameters` fields -- name, role, objectives,
# data_sources -- are not arguments to any DARWIN function, because none of them
# is a computational choice: the primary analysis and its sensitivity analyses
# call the same estimator with the same arguments, and what separates them is
# which one the study's conclusion may rest on. That is a decision the PLAN makes.
#
# data_sources became a guard (see source_guard above). The other three reached
# the script only as a comment, which is fine for someone reading the code and
# useless to everything downstream: the exported results are what a study report
# is written from, and a results file that cannot say which of nine prevalence
# estimates is the primary one pushes the same "it is only in prose" problem one
# step further along.
#
# omopgenerics settings are the documented home for this. A summarised_result may
# carry extra settings columns beyond the required ones, and they survive bind(),
# suppress() and the export/import round trip -- verified against omopgenerics
# 1.4.1 and IncidencePrevalence 1.2.1, including that the tag reaches the CSV
# that actually leaves the data partner.
SAP_TAG_HELPER <- paste(
  "# The SAP fields that are not estimator arguments, carried into the result so",
  "# the exported settings say which analysis produced it and which one the",
  "# study concludes from. An empty result is left alone: it has no settings rows",
  "# to label, and omopgenerics drops all-NA settings columns anyway.",
  "sapTag <- function(result, analysis, role = NA_character_,",
  "                   objectives = NA_character_) {",
  "  if (nrow(result) == 0) return(result)",
  "  s <- omopgenerics::settings(result)",
  "  s$sap_analysis   <- analysis",
  "  s$sap_role       <- role",
  "  s$sap_objectives <- objectives",
  "  omopgenerics::newSummarisedResult(result, settings = s)",
  "}",
  sep = "\n")

# Which objectives an analysis answers, as the numbers the document uses. Shared
# with objective_labels() (sap_study_export.R) so the comment above a call and the
# label inside the result cannot number them differently.
objective_indices <- function(sap, a) {
  objs <- sap$study$objectives %||% list()
  ids  <- vapply(objs, function(o) objective_id(o) %||% "", character(1))
  named <- as.character(unlist(a$objectives %||% list()))
  which(ids %in% named)
}

# The one line that labels an estimate. `analysis` is always passed -- an unnamed
# analysis is a gap the author can see in the results rather than one that
# silently loses its label. role and objectives are passed only when stated, so
# the helper's own NA defaults stand for "the plan never said".
sap_tag_call <- function(var, a, sap) {
  nm <- trimws(as.character(a$name %||% "")[1])
  if (is.na(nm) || !nzchar(nm)) nm <- "(unnamed analysis)"
  role <- canonical_analysis_role(a$role)
  objs <- objective_indices(sap, a)
  args <- c(var,
            sprintf("analysis = %s", r_string(nm)),
            if (nzchar(role)) sprintf("role = %s", r_string(role)),
            if (length(objs)) sprintf("objectives = %s",
                                      r_string(paste(objs, collapse = ", "))))
  one <- sprintf("%s <- sapTag(%s)", var, paste(args, collapse = ", "))
  if (nchar(one) <= 92) return(one)
  # An analysis name is a whole sentence, so this wraps more often than not.
  # Two-space indent rather than aligning under the paren: the left-hand side is
  # often results[["a_long_analysis_slug"]], and aligning to it would push every
  # argument off the right of the page.
  sprintf("%s <- sapTag(\n%s\n)", var,
          paste(sprintf("  %s", args), collapse = ",\n"))
}

# An estimate that runs at a data partner where its cohort is never built.
#
# The same failure as uninstantiated_cohort_problems() -- "<table> does not exist
# in the cdm_reference object" -- but reachable only per DATABASE, and only since
# data_sources became a guard in the generated script rather than documentation.
# A cohort restricted to SIDIAP and an analysis running on all five is a plan
# that reads fine and dies at four partners: the cohort block is guarded out
# there, the estimate is not, and it points at a table nothing created.
#
# Reported per analysis, naming the databases and the cohort, because that is the
# pair the author has to reconcile -- widen the cohort or narrow the analysis.
#
# An analysis or cohort naming NO sources is unrestricted (the author never said),
# so it can never be the narrower of the two and raises nothing.
cohort_source_coverage_problems <- function(cohorts, analyses, all_sources) {
  cohorts     <- cohorts %||% list()
  analyses    <- analyses %||% list()
  all_sources <- as.character(all_sources %||% character(0))
  if (!length(all_sources)) return(list())

  index <- stats::setNames(cohorts, vapply(cohorts, function(co)
    as.character(co$name %||% "")[1], character(1)))
  # Where an item actually runs: what it names, or everywhere when it names
  # nothing.
  runs_in <- function(x) {
    keys <- item_sources(x)
    if (!length(keys)) all_sources else keys
  }

  found <- list()
  for (a in analyses) {
    if (is.na(analysis_estimator(a$analysis_type))) next
    a_sources <- runs_in(a)
    p <- a$parameters %||% list()
    msgs <- character(0)
    for (id in as.character(analysis_template(a$analysis_type)$pickers$cohorts %||% character(0))) {
      nm <- trimws(as.character(p[[id]] %||% "")[1])
      if (is.na(nm) || !nzchar(nm)) next
      co <- index[[nm]]
      # A cohort this SAP does not define is uninstantiated_cohort_problems()'s
      # to report; saying it twice in different words helps nobody.
      if (is.null(co)) next
      gap <- setdiff(a_sources, runs_in(co))
      if (length(gap)) {
        msgs <- c(msgs, sprintf(paste(
          "Runs on %s, where the cohort '%s' is not built -- the script guards",
          "that cohort to its own data sources, so the estimate would point at a",
          "table that does not exist there."),
          paste(gap, collapse = ", "), nm))
      }
    }
    if (length(msgs)) {
      found[[length(found) + 1]] <- list(
        name = as.character(a$name %||% "(unnamed analysis)")[1], messages = msgs)
    }
  }
  found
}

# Cohorts ----------------------------------------------------------------------

# The generator call a denominator cohort describes, or NULL for a plain cohort.
#
# NULL is not a gap: a plain cohort (target, outcome, comparator, censor) is
# instantiated OUTSIDE IncidencePrevalence -- from its codelist and entry
# criteria, by CohortConstructor or a study's own script -- and the package only
# ever points at the table it produced. The preview says so rather than inventing
# a call the package does not have.
cohort_r_code <- function(co) {
  kind <- canonical_cohort_kind(co$kind)
  if (!is_denominator_kind(kind)) return(NULL)
  target <- identical(kind, "target_denominator")

  args <- list(cdm = "cdm", name = r_string(cohort_table_name(co$name) %||% "denominator"))
  if (target) {
    tgt <- cohort_table_name(co$targetCohortTable)
    args$targetCohortTable <- if (is.na(tgt)) NULL else r_string(tgt)
  }
  args$cohortDateRange <- r_date_range(co[["cohortDateRange"]])
  # Each interval "creates one set of denominator cohorts", so several stay a
  # list; the signature's own default is the bare pair c(0, Inf).
  if (target) args$timeAtRisk <- r_bound_list(co$timeAtRisk)
  # ageGroup is always a list, even at length one: its default is list(c(0, 150)).
  args$ageGroup             <- r_bound_list(co$ageGroup, always_list = TRUE)
  args$sex                  <- r_chr_vec(co$sex)
  args$daysPriorObservation <- r_num_vec(co$daysPriorObservation)
  # The two rules are always collected as a definite TRUE/FALSE (isTRUE on the
  # checkbox), so unlike the fields above they are never "undecided" -- and
  # requirementInteractions changes how many cohorts the call generates, so
  # leaving it implicit is exactly what made the old preview ambiguous.
  if (target) args$requirementsAtEntry <- r_logical(co$requirementsAtEntry)
  args$requirementInteractions <- r_logical(co$requirementInteractions)

  r_call(
    if (target) "generateTargetDenominatorCohortSet" else "generateDenominatorCohortSet",
    args
  )
}

# Analyses ---------------------------------------------------------------------

# The estimator a serialised analysis_type names, or NA for one that maps onto no
# IncidencePrevalence function (the "Other" template, or free text).
analysis_estimator <- function(type) {
  type <- as.character(type %||% "")[1]
  if (is.na(type) || !nzchar(type)) return(NA_character_)
  estimators <- c("estimatePointPrevalence", "estimatePeriodPrevalence", "estimateIncidence")
  if (type %in% estimators) return(type)
  # The Incidence template has no serialised_type, so a saved analysis names the
  # registry key. Prevalence is deliberately absent here: it DOES serialise an
  # estimator, and the key alone would not say point or period -- guessing one
  # would put a choice in the code the author never made.
  if (identical(type, "Incidence")) return("estimateIncidence")
  NA_character_
}

# The estimator call an analysis describes, or NULL when its type maps onto none.
#
# `cohorts` is the SAP's cohort list, used only to resolve a picked display name
# to the table name the generator call above created -- so the two code blocks in
# the document refer to the same table.
analysis_r_code <- function(a, cohorts = list()) {
  fn <- analysis_estimator(a$analysis_type)
  if (is.na(fn)) return(NULL)
  p <- a$parameters %||% list()

  tbl <- function(nm) {
    t <- cohort_table_name(nm)
    if (is.na(t)) NULL else r_string(t)
  }
  # NULL means "every cohort in the set", which is the estimators' own default,
  # so an unrestricted analysis simply omits the argument. A restricted one must
  # say so: dropping these is what made a SAP run on 2 of 8 denominator cohorts
  # read exactly like one run on all 8.
  ids <- function(x) r_num_vec(x)

  args <- list(cdm = "cdm",
               denominatorTable = tbl(p$denominatorTable),
               outcomeTable     = tbl(p$outcomeTable))

  if (identical(fn, "estimateIncidence")) {
    args$censorTable         <- tbl(p$censorTable)
    args$denominatorCohortId <- ids(p$denominatorCohortId)
    args$outcomeCohortId     <- ids(p$outcomeCohortId)
    args$censorCohortId      <- ids(p$censorCohortId)
    args$interval            <- r_chr_vec(p$interval)
    args$completeDatabaseIntervals <- r_logical(p$completeDatabaseIntervals)
    # washout_days(), not r_num_vec(): under na = "null" an unbounded washout
    # serialises as [null], which would read as empty here and silently drop the
    # argument -- turning the author's explicit "unbounded" into the default.
    # NULL back from it means genuinely unset, and that one is left implicit.
    washout <- washout_days(p$outcomeWashout)
    args$outcomeWashout <- if (is.null(washout)) NULL else r_number(washout)
    args$repeatedEvents <- r_logical(p$repeatedEvents)
  } else {
    args$denominatorCohortId <- ids(p$denominatorCohortId)
    args$outcomeCohortId     <- ids(p$outcomeCohortId)
    args$interval            <- r_chr_vec(p[["interval"]])
    if (identical(fn, "estimatePointPrevalence")) {
      args$timePoint <- r_chr_vec(p$timePoint)
    } else {
      args$completeDatabaseIntervals <- r_logical(p$completeDatabaseIntervals)
      args$fullContribution          <- r_logical(p$fullContribution)
      args$level                     <- r_chr_vec(p$level)
    }
  }

  args$strata <- r_strata(p$strata)
  # Inert without strata -- the estimators then return only the overall estimate
  # -- so it is stated only where it does something.
  if (!is.null(args$strata)) args$includeOverallStrata <- r_logical(p$includeOverallStrata)

  r_call(fn, args)
}

# A unique R variable name per analysis, so the estimates can be bound and
# suppressed together at the end. Derived from the analysis name for readability;
# the index keeps it unique when two analyses share a name (or have none), which
# a variable name -- unlike a cohort table name -- can silently tolerate.
estimate_var_names <- function(analyses) {
  vapply(seq_along(analyses), function(i) {
    slug <- cohort_table_name(analyses[[i]]$name)
    if (is.na(slug)) return(sprintf("estimate_%d", i))
    # Analysis names are sentences ("Follicular lymphoma - 5-year partial point
    # prevalence - 365d prior observation"), so unlike a table name -- which must
    # stay recognisable as the thing it creates -- a variable name is trimmed to
    # a readable stem. The index carries the uniqueness.
    slug <- sub("_+$", "", substr(slug, 1, 32))
    if (!nzchar(slug)) sprintf("estimate_%d", i) else sprintf("%s_%d", slug, i)
  }, character(1))
}

# The whole SAP as its ordered, labelled script BLOCKS: every denominator cohort
# set generated first, then every estimate, then the one suppression step that
# governs what may leave the data partner. Running the plan means running the
# blocks in this order, which is why the order is a property of the list rather
# than of whoever renders it.
#
# This is the single source both forms of the generated code come from:
# sap_r_script() joins the blocks into one runnable listing, and the document's
# appendix renders the same blocks under their own headings so every analysis is
# reachable from the table of contents. Neither form can therefore show code the
# other does not, or show it in a different order.
#
# One entry per block:
#   group  the section it belongs to
#   title  the cohort or analysis it came from, NA for a whole-plan step
#   code   the R, or NULL when the SAP asks for something with no package call
#   note   why, when code is NULL -- so an unmappable analysis is still listed
#          rather than silently dropped
#   pkgs   the libraries this block's calls come from, so sap_r_script() can head
#          the script with exactly the ones it uses. Carried per block rather
#          than assumed for the whole script: a SAP with no analyses must not
#          load IncidencePrevalence, and a new op or analysis type brings its own
#          declaration with it (see the two registries).
# The libraries a set of blocks actually calls, in the order they are first used.
# Derived, never listed: a SAP that generates no cohorts must not tell its reader
# to load CohortConstructor, and a plan is not a place to carry a dependency it
# does not have.
sap_script_packages <- function(secs) {
  unique(unlist(lapply(secs, function(s) as.character(s$pkgs %||% character(0)))))
}

sap_script_sections <- function(sap) {
  cohorts   <- sap$cohorts %||% list()
  analyses  <- sap$proposed_analyses %||% list()
  codelists <- sap$codelists %||% list()
  # The blocks accumulate in an ENVIRONMENT rather than through `<<-`.
  #
  # add() has to reach out of its own frame to append, and super-assignment does
  # that by walking the scope chain until it finds the name -- so a typo binds
  # something in the global environment instead of failing. An environment is
  # the same mutation said out loud: `acc` is passed by reference, so a plain
  # `<-` into it is unambiguous about what is being changed and where it lives.
  acc <- new.env(parent = emptyenv())
  acc$out <- list()
  add <- function(group, title, code = NULL, note = NULL, pkgs = character(0)) {
    acc$out <- c(acc$out, list(list(group = group, title = title, code = code,
                                    note = note, pkgs = pkgs)))
  }

  # Dependency order, which is why the groups run in this sequence: a concept set
  # is built before the cohort that enters on it, a plain cohort before the
  # denominator generated from it, and every cohort before the estimates that
  # point at it.
  #
  # Only codelists a cohort's operations actually cite are emitted; see
  # cited_codelist_names() for why free-text citations do not count.
  cited <- cited_codelist_names(cohorts)
  for (cl in codelists) {
    nm <- as.character(cl$name %||% "")[1]
    if (!nm %in% cited) next
    add("Concept sets", nm, code = concept_set_r_code(cl))
  }

  all_sources <- sap_source_keys(sap)

  for (co in cohorts) {
    code <- cohort_operations_code(
      co,
      vapply(codelists, function(cl) as.character(cl$name %||% "")[1], character(1)),
      vapply(cohorts, function(x) as.character(x$name %||% "")[1], character(1)))
    if (!is.null(code)) {
      add("Cohorts", as.character(co$name %||% ""),
          code = source_guard(code, co, all_sources),
          pkgs = cohort_operations_packages(co))
    }
  }

  for (co in cohorts) {
    code <- cohort_r_code(co)
    # NULL is a plain cohort. One with typed operations was emitted above; one
    # without is instantiated outside IncidencePrevalence, so there is no call to
    # show and inventing a heading for it would imply otherwise.
    if (!is.null(code)) {
      # `cdm <- `, not `cdm$name <- `: the two packages return different things
      # and so take different idioms. generateDenominatorCohortSet() returns A CDM
      # REFERENCE with the new table attached, so its result must go back into
      # `cdm`; conceptCohort() returns a cohort table, which is why the cohort
      # pipelines above assign into `cdm$<table>`. Emitted as a bare call until
      # 0.4.21, which discarded the result -- the denominator was never attached,
      # and every estimate pointing at it would have failed at run time.
      add("Denominator cohort sets", as.character(co$name %||% ""),
          code = source_guard(sprintf("cdm <- %s", code), co, all_sources),
          pkgs = "IncidencePrevalence")
    }
  }

  vars   <- estimate_var_names(analyses)
  bound  <- character(0)
  tagged <- FALSE
  for (i in seq_along(analyses)) {
    a    <- analyses[[i]]
    nm   <- as.character(a$name %||% "")
    code <- analysis_r_code(a, cohorts)
    if (is.null(code)) {
      add("Estimates", nm, note = sprintf(
        "No IncidencePrevalence function maps onto analysis type '%s'.",
        as.character(a$analysis_type %||% "")))
    } else {
      # Assigned here, not in analysis_r_code(): the variable exists so the
      # estimates can be bound and suppressed together at the end, which is a
      # fact about the script as a whole rather than about the call.
      bound <- c(bound, vars[[i]])
      tmpl  <- analysis_template(canonical_analysis_type(a$analysis_type))
      # The helper is defined once, immediately before the first estimate that
      # uses it, so a plan with no estimates carries no dead function.
      if (!tagged) {
        add("Estimates", NA_character_, code = SAP_TAG_HELPER)
        tagged <- TRUE
      }
      add("Estimates", nm,
          code = paste(source_guard_estimate(vars[[i]], code, a, all_sources),
                       sap_tag_call(vars[[i]], a, sap), sep = "\n"),
          pkgs = as.character(tmpl$package %||% character(0)))
    }
  }

  supp <- suppression_r_code(sap$study$min_cell_count, bound)
  # No title: suppression is one step over every estimate above, not a step
  # belonging to any one of them.
  # No package: suppression_r_code() writes omopgenerics::suppress() namespaced,
  # so the script needs no library() for it.
  if (!is.null(supp)) add("Result suppression", NA_character_, code = supp)

  # The header is a BLOCK like any other, prepended once the blocks below it are
  # known, because what it loads is derived from what they call. Emitting it only
  # in sap_r_script() would have made the flat script and the appendix disagree,
  # which is the drift sap_script_sections() exists to make impossible.
  pkgs <- sap_script_packages(acc$out)
  if (length(pkgs)) {
    acc$out <- c(list(list(group = "Libraries", title = NA_character_,
                           code  = paste(sprintf("library(%s)", pkgs), collapse = "\n"),
                           note  = NULL, pkgs = character(0))),
                 acc$out)
  }

  acc$out
}

# The blocks above as one runnable script, each under its group banner and its
# own name. The executable form of the document.
sap_r_script <- function(sap) {
  secs <- sap_script_sections(sap)
  if (!length(secs)) return("")
  out   <- character(0)
  group <- NULL
  for (s in secs) {
    if (!identical(s$group, group)) {
      group <- s$group
      out   <- c(out, sprintf("# --- %s ---", group))
    }
    label <- if (is.na(s$title)) character(0) else sprintf("# %s", s$title)
    # The note is indented under its own analysis's name, so a block with no
    # call still reads as that analysis rather than as a stray remark.
    body  <- if (is.null(s$code)) sprintf("#   %s", s$note) else s$code
    out   <- c(out, paste(c(label, body), collapse = "\n"))
  }
  paste(out, collapse = "\n\n")
}

# The study-level minimum cell count as the call that applies it.
#
# NULL when the plan states no threshold: a SAP that never said what may be
# exported must not have a number invented for it here, and its absence is
# already visible in the study table. `vars` are the estimates to combine --
# binding one object is pointless, so a single estimate is suppressed directly.
suppression_r_code <- function(min_cell_count, vars) {
  n <- suppressWarnings(as.numeric(min_cell_count %||% NA))
  if (length(n) != 1 || is.na(n) || !length(vars)) return(NULL)
  combine <- if (length(vars) == 1) {
    vars[[1]]
  } else {
    sprintf("omopgenerics::bind(\n%s\n)",
            paste(sprintf("  %s", vars), collapse = ",\n"))
  }
  # No section banner here: the block's own group heading supplies it, in both
  # the flat script and the appendix.
  paste0(
    "# Applied once, to every result above, before anything leaves the data partner.\n",
    sprintf("results <- %s\n", combine),
    sprintf("results <- omopgenerics::suppress(results, minCellCount = %s)", r_number(n))
  )
}
