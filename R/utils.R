# Small helpers shared across the app --------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

# A textarea holds one item per line; JSON holds an array.
split_lines <- function(x) {
  if (is.null(x)) return(character(0))
  v <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  v[nzchar(v)]
}

join_lines <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = "\n")
}

# I() keeps jsonlite from unboxing a length-1 vector, so these fields are
# always arrays in the JSON regardless of how many entries the user typed.
as_array <- function(x) I(as.character(x))

# Same, for a field that must stay numeric -- an age group or a time-at-risk
# bound. Under na = "null" jsonlite writes Inf as null, so c(0, Inf) becomes
# [0, null]: an unbounded upper bound. Reading it back gives list(0, NULL), so
# index such a pair with [[1]]/[[2]] and never unlist() it, or the null collapses
# and a two-element pair silently becomes one.
as_num_array <- function(x) I(as.numeric(x))

# Blank text becomes null rather than "" so consumers can test for absence. A
# cleared dateInput sends NA rather than "", and lands here too -- same answer.
blank_to_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) NA_character_ else x
}

# A dateInput's `value`: a Date, or NULL to start the field genuinely blank.
#
# These fields used to be free-text, so an older file may hold something a
# calendar cannot show ("Q1 2024", "unknown"). dateInput() would warn and render
# blank anyway; doing the coercion here makes that explicit and keeps the console
# quiet. A date the picker cannot represent is not one this app can capture.
as_date_value <- function(x) {
  x <- trimws(as.character(x %||% "")[1])
  if (is.na(x) || !nzchar(x)) return(NULL)
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (is.na(d)) NULL else d
}

# A dateInput that genuinely starts blank -- which shiny::dateInput() will not do.
#
# Its JS binding substitutes TODAY whenever the field has no `data-initial-date`:
#
#   let date = $input.data("initial-date");
#   if (date === void 0 || date === null) date = ...new Date();   -- shiny.js
#
# Blank is a *meaningful* value here: an unset cohort date range means "the whole
# observation period", and an unset data lock point means the author has not said.
# A silently pre-filled today would decide both on their behalf -- the same trap
# the outcome washout's empty first choice exists to avoid. An empty string is
# neither undefined nor null, so the picker starts empty and stays that way.
date_input <- function(inputId, label, value = NULL, ...) {
  value <- as_date_value(value)
  di <- dateInput(inputId, label, value = value, width = "100%", ...)
  if (!is.null(value)) return(di)
  htmltools::tagQuery(di)$find("input")$addAttrs("data-initial-date" = "")$allTags()
}

slugify <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) "sap" else substr(x, 1, 60)
}

# Builds the value-lookup used by the *_item_ui() functions when restoring a
# previously saved SAP: pf("cdm_table") returns the stored value or a default.
prefiller <- function(prefill) {
  function(key, default = "") {
    v <- prefill[[key]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) default else v
  }
}

# First non-empty key, so a renamed section can still read older SAP files.
coalesce_key <- function(x, ...) {
  for (key in c(...)) {
    v <- x[[key]]
    if (!is.null(v) && length(v) > 0) return(v)
  }
  list()
}

sap_json <- function(sap) {
  jsonlite::toJSON(sap, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
}

save_sap <- function(sap, dir = "output") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, sprintf(
    "sap-%s-%s.json",
    slugify(sap$study$title),
    format(Sys.time(), "%Y%m%d-%H%M%S")
  ))
  writeLines(sap_json(sap), path)
  path
}

read_sap <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

# Cross-section migrations, run once on a loaded SAP before any section sees it.
#
# A template's flatten() can only rewrite its own analysis, so anything that has
# to move *between* sections has to happen here -- and before cohorts$load(), or
# the cohort cards are already built by the time the analysis is read.
#
# Before 0.3.2 an analysis named a plain target cohort as its denominator and
# carried its own time at risk. Neither is how IncidencePrevalence works: a
# denominator is a cohort *set* produced by generateTargetDenominatorCohortSet(),
# and timeAtRisk is one of that generator's arguments.
#
# So the missing denominator is synthesised here -- one per (target cohort, time
# at risk) pair, since that is exactly what one generator call produces -- and the
# analysis is repointed at it. Doing it in the template's flatten() is not
# possible: an analysis can only rewrite itself, not add a cohort.
#
# The old {start_offset_days, start_anchor, end_offset_days, end_anchor} becomes
# a single [[start, end]] interval. The anchors are dropped: the API has nowhere
# to put them, because both bounds are relative to target cohort entry.
migrate_sap <- function(sap) {
  analyses <- coalesce_key(sap, "proposed_analyses", "analyses")
  cohorts  <- lapply(sap$cohorts %||% list(), migrate_cohort)
  if (!length(analyses)) {
    sap$cohorts <- cohorts
    return(sap)
  }

  index_of <- function(nm) match(as.character(nm), vapply(
    cohorts, function(x) as.character(x$name %||% ""), character(1)))

  for (k in seq_along(analyses)) {
    a     <- analyses[[k]]
    flat  <- is.null(a$parameters)
    p     <- if (flat) a else a$parameters
    # Pre-0.3.0 the generic form called the denominator `target_cohort`.
    nm    <- p$denominator_cohort %||% p$target_cohort
    if (is.null(nm) || !nzchar(as.character(nm))) next

    i <- index_of(nm)
    # Names a cohort nobody defined, or one that is already a denominator: leave it.
    if (is.na(i) || is_denominator_kind(cohorts[[i]]$kind)) next

    tar <- migrate_time_at_risk(p$time_at_risk)
    den <- synthetic_denominator_name(as.character(nm), tar)

    if (is.na(index_of(den))) {
      cohorts[[length(cohorts) + 1]] <- list(
        name                  = den,
        kind                  = "target_denominator",
        description           = sprintf(
          "Denominator generated from '%s'. Added when this SAP was read: before 0.3.2 the time at risk lived on the analysis.", nm),
        target_cohort         = as.character(nm),
        time_at_risk          = tar,
        requirements_at_entry = TRUE
      )
    }

    p$denominator_cohort <- den
    p$time_at_risk       <- NULL
    if (flat) analyses[[k]] <- p else analyses[[k]]$parameters <- p
  }

  sap$cohorts <- cohorts
  # coalesce_key() may have read these from the pre-0.2.0 `analyses` key.
  sap$proposed_analyses <- analyses
  sap$analyses <- NULL
  sap
}

# One generator call per (target, time at risk), so the name has to carry both.
synthetic_denominator_name <- function(target, tar) {
  win <- paste(format_bound_list(tar), collapse = "; ")
  sprintf("%s denominator (%s days)", target, win)
}

# The anchored shape -> a list holding one [start, end] interval. Already-migrated
# cohorts hold a list of pairs, which has no $start_offset_days and is left alone.
migrate_time_at_risk <- function(tar) {
  if (is.null(tar)) return(list(as_num_array(c(0, Inf))))   # the API's default
  if (is.list(tar) && !is.null(tar$start_offset_days)) {
    return(list(as_num_array(c(
      as.numeric(tar$start_offset_days %||% 0),
      as.numeric(tar$end_offset_days   %||% Inf)
    ))))
  }
  tar
}

migrate_cohort <- function(ch) {
  ch$kind <- canonical_cohort_kind(ch$kind %||% ch$role)
  ch$role <- NULL

  if (!is.null(ch$time_at_risk)) ch$time_at_risk <- migrate_time_at_risk(ch$time_at_risk)

  # Age groups were free text ("18-64"); the API wants numeric pairs.
  ages <- ch$age_groups
  if (length(ages) && all(vapply(ages, function(a) length(a) == 1 && is.character(a[[1]]),
                                 logical(1)))) {
    ch$age_groups <- parse_bound_list(join_lines(ages))
  }

  # Neither generator takes a washout: it is estimateIncidence(outcomeWashout =),
  # which the Incidence analysis now captures. There is nowhere to move a cohort
  # washout to, so it is dropped rather than quietly misapplied.
  ch$washout_days <- NULL

  # 0.3.1 called the cohort date range the "study period".
  if (is.null(ch$cohort_date_range_start)) ch$cohort_date_range_start <- ch$study_period_start
  if (is.null(ch$cohort_date_range_end))   ch$cohort_date_range_end   <- ch$study_period_end
  ch$study_period_start <- NULL
  ch$study_period_end   <- NULL

  # daysPriorObservation may be a vector; the old field was a single number.
  if (is.null(ch$days_prior_observation) && !is.null(ch$prior_observation_days)) {
    ch$days_prior_observation <- as_num_array(ch$prior_observation_days)
  }
  ch$prior_observation_days <- NULL

  ch
}
