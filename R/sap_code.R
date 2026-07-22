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
  if (nchar(prefix) + nchar(value) <= width || !grepl("^list\\(", value)) return(value)
  inner <- substr(value, 6L, nchar(value) - 1L)
  parts <- split_top_level(inner)
  if (length(parts) < 2) return(value)
  pad <- strrep(" ", nchar(prefix) + 5L)
  wrapped <- character(0)
  line    <- "list("
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
  # Pre-0.4.0 files name the registry key rather than the estimator. Prevalence
  # is deliberately absent: the key alone does not say point or period, and
  # guessing one would put a choice in the code the author never made.
  aliases <- c("Incidence" = "estimateIncidence", "Incidence rate" = "estimateIncidence")
  if (type %in% names(aliases)) return(unname(aliases[[type]]))
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
sap_script_sections <- function(sap) {
  cohorts  <- sap$cohorts %||% list()
  analyses <- sap$proposed_analyses %||% list()
  out <- list()
  add <- function(group, title, code = NULL, note = NULL) {
    out[[length(out) + 1]] <<- list(group = group, title = title, code = code, note = note)
  }

  for (co in cohorts) {
    code <- cohort_r_code(co)
    # NULL is a plain cohort, instantiated outside IncidencePrevalence: there is
    # no call to show, and inventing a heading for one would imply otherwise.
    if (!is.null(code)) {
      add("Denominator cohort sets", as.character(co$name %||% ""), code = code)
    }
  }

  vars  <- estimate_var_names(analyses)
  bound <- character(0)
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
      add("Estimates", nm, code = sprintf("%s <- %s", vars[[i]], code))
    }
  }

  supp <- suppression_r_code(sap$study$min_cell_count, bound)
  # No title: suppression is one step over every estimate above, not a step
  # belonging to any one of them.
  if (!is.null(supp)) add("Result suppression", NA_character_, code = supp)

  out
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
