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

# Blank text becomes null rather than "" so consumers can test for absence.
blank_to_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) NA_character_ else x
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
# 0.3.0 moved time at risk from each analysis onto the cohort it runs on, so two
# analyses sharing a denominator can no longer disagree about it. An older file
# holds it on the analysis; copy it across or it is silently lost.
migrate_sap <- function(sap) {
  analyses <- coalesce_key(sap, "proposed_analyses", "analyses")
  cohorts  <- sap$cohorts %||% list()
  if (!length(analyses) || !length(cohorts)) return(sap)

  cohort_names <- vapply(cohorts, function(x) as.character(x$name %||% ""), character(1))

  for (a in analyses) {
    p   <- if (is.null(a$parameters)) a else a$parameters
    tar <- p$time_at_risk
    if (is.null(tar)) next
    # Pre-0.3.0 the generic form called the denominator `target_cohort`.
    nm <- p$denominator_cohort %||% p$target_cohort
    if (is.null(nm) || !nzchar(as.character(nm))) next
    i <- match(as.character(nm), cohort_names)
    # An analysis may name a cohort that was never defined; nothing to move onto.
    if (is.na(i)) next
    # First analysis to claim the cohort wins. Two analyses on one denominator
    # disagreeing about time at risk is exactly what this move exists to stop,
    # and there is no way to pick a winner here -- validate() will flag it.
    if (is.null(cohorts[[i]]$time_at_risk)) cohorts[[i]]$time_at_risk <- tar
  }

  sap$cohorts <- cohorts
  sap
}
