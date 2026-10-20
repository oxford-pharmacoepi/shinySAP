# SAP validation ---------------------------------------------------------------
#
# Two kinds of check:
#   * Structural: checkSap() validates required fields, value types, unknown
#     fields, duplicate ids, and broken references against the schema.
#   * Semantic: constraints the schema cannot express -- a denominator slot must
#     name a denominator cohort; strata must be columns the denominator carries.
#
# sapProblems() combines both, so callers (an LLM, a pipeline, the Shiny app)
# get one list without launching the UI.

# The upper age bound IncidencePrevalence generators accept.
AGE_MAX <- 150L

#' Format a list of numeric bound pairs as human-readable labels
#'
#' @param pairs A list of length-2 numeric vectors, e.g. `list(c(0, 17), c(18, 150))`.
#' @param open The label for an open upper bound (default `"Inf"`).
#' @return A character vector, one element per pair.
#' @noRd
format_bound_list <- function(pairs, open = "Inf") {
  if (!length(pairs)) return(character(0))
  vapply(pairs, function(p) {
    p <- as.numeric(unlist(p))
    hi <- if (length(p) >= 2 && is.finite(p[[2]])) format(p[[2]]) else as.character(open)
    paste0(format(p[[1]]), ", ", hi)
  }, character(1))
}

# "cohorts[3].parameters.codelist_id" -> list(name = <item label>, field = <field path>)
problem_location <- function(sap, path) {
  path <- as.character(path %||% "")
  m <- regmatches(path, regexec("^([a-z_]+)\\[(\\d+)\\](?:\\.(.*))?$", path, perl = TRUE))[[1]]
  if (length(m) == 4 && nzchar(m[[1]])) {
    collection <- m[[2]]
    index <- as.integer(m[[3]])
    item <- sap[[collection]][[index]]
    object <- sapSchemaVocabulary()$collections[[collection]]
    noun <- gsub("_", " ", object %||% collection)
    nm <- as.character(item$name %||% "")
    label <- if (nzchar(nm)) nm else sprintf("Untitled %s", noun)
    if (!is.null(item$id)) label <- sprintf("%s (%s)", label, item$id)
    return(list(name = label, field = m[[4]]))
  }
  if (identical(path, "") || identical(path, "study") || startsWith(path, "study.")) {
    return(list(name = "Study", field = sub("^study\\.?", "", path)))
  }
  list(name = tools::toTitleCase(gsub("_", " ", sub("\\..*$", "", path))),
       field = sub("^[a-z_]+\\.?", "", path))
}

# Regroup structural problems (from checkSap()) by item name for display.
format_check_problems <- function(problems, sap) {
  if (!length(problems)) return(list())
  grouped <- list()
  for (p in problems) {
    loc <- problem_location(sap, p$path)
    msg <- if (nzchar(loc$field)) sprintf("%s: %s", loc$field, p$message) else p$message
    grouped[[loc$name]] <- c(grouped[[loc$name]], msg)
  }
  lapply(names(grouped), function(nm) list(name = nm, messages = unique(grouped[[nm]])))
}

# The columns generateDenominatorCohortSet() attaches, and so the only columns
# IncidencePrevalence estimators can stratify by.
STRATA_VARIABLES <- c("age_group", "sex")

date_range_problems <- function(range) {
  if (is.null(range) || length(range) < 2) return(character(0))
  range <- as.Date(range)
  if (anyNA(range) || range[[1]] <= range[[2]]) return(character(0))
  sprintf("The date range starts (%s) after it ends (%s).", range[[1]], range[[2]])
}

age_group_problems <- function(groups) {
  errs <- character(0)
  for (g in groups %||% list()) {
    g <- as.numeric(unlist(g))
    if (length(g) == 2 && !anyNA(g) && g[[2]] > AGE_MAX) {
      errs <- c(errs, sprintf("Age group '%s' must lie within 0 and %d.",
                              format_bound_list(list(g), open = AGE_MAX), AGE_MAX))
    }
  }
  errs
}

cohort_strata_variables <- function(cohort) {
  if (is.null(cohort) || !sapIsDenominatorType(cohort$type)) return(character(0))
  STRATA_VARIABLES
}

validate_strata_against <- function(groups, cohort) {
  if (!length(groups) || is.null(cohort)) return(character(0))
  errs <- character(0)
  declared <- cohort_strata_variables(cohort)
  p <- cohort$parameters %||% list()
  sex <- as.character(p$sex %||% "Both")
  n_ages <- length(p$age_group %||% list())
  for (v in unique(as.character(unlist(groups)))) {
    if (!v %in% declared) {
      errs <- c(errs, sprintf(
        "Cannot stratify by '%s': the denominator cohort does not carry that column (it has %s).",
        v, if (length(declared)) paste(declared, collapse = ", ") else "none"))
      next
    }
    if (identical(v, "sex") && !identical(sex, "Both")) {
      errs <- c(errs, sprintf(
        "Cannot stratify by sex: the denominator cohort is restricted to %s.", sex))
    }
    if (identical(v, "age_group") && n_ages < 2) {
      errs <- c(errs, paste("Cannot stratify by age_group:",
                            "the denominator cohort defines fewer than two age groups."))
    }
  }
  unique(errs)
}

# An analysis restricted to named data sources where a cohort it uses is never
# built: the generated guard would run the estimator against a missing table.
source_coverage_problems <- function(analysis, sap) {
  own <- chr_vec(analysis$data_source_id)
  if (!length(own)) return(character(0))
  errs <- character(0)
  p <- analysis$parameters %||% list()
  for (key in grep("_cohort_id$", names(p), value = TRUE)) {
    for (cid in chr_vec(p[[key]])) {
      co <- sapItemById(sap$cohorts, cid)
      theirs <- chr_vec(co$data_source_id)
      if (is.null(co) || !length(theirs)) next
      missing <- setdiff(own, theirs)
      if (length(missing)) {
        errs <- c(errs, sprintf("Runs on %s, where cohort '%s' is not built.",
                                paste(data_source_names(sap, missing), collapse = ", "),
                                sapItemLabel(co, "cohort")))
      }
    }
  }
  unique(errs)
}

semantic_problems <- function(sap) {
  acc <- new.env(parent = emptyenv())
  acc$found <- list()
  add <- function(name, messages) {
    messages <- as.character(messages)
    if (!length(messages)) return(invisible(NULL))
    acc$found <- c(acc$found, list(list(name = name, messages = messages)))
  }
  cohorts <- sap$cohorts %||% list()

  for (m in sap$data_source_modifications %||% list()) {
    add(sapItemLabel(m, "modification"), date_range_problems(m$parameters$date_range))
  }

  for (co in cohorts) {
    p <- co$parameters %||% list()
    errs <- character(0)
    if (identical(co$type, "denominator") || identical(co$type, "target_denominator")) {
      errs <- c(errs, age_group_problems(p$age_group))
    }
    if (identical(co$type, "target_denominator")) {
      errs <- c(errs, date_range_problems(p$target_cohort_date_range))
      for (tid in chr_vec(p$target_cohort_id)) {
        if (identical(tid, co$id)) {
          errs <- c(errs, "A target denominator cannot be built from itself.")
        }
        t <- sapItemById(cohorts, tid)
        if (!is.null(t) && sapIsDenominatorType(t$type)) {
          errs <- c(errs, sprintf(
            "'%s' is a denominator; the target cohort must be a plain cohort.",
            sapItemLabel(t, "cohort")))
        }
      }
      for (w in p$time_at_risk %||% list()) {
        if (as.numeric(w[[1]]) < 0) {
          errs <- c(errs, sprintf("Time at risk '%s' must start at day 0 or later.",
                                  format_bound_list(list(w))))
        }
      }
    }
    add(sapItemLabel(co, "cohort"), errs)
  }
  add("Cohorts", sapTableNameCollisions(cohorts))

  used <- unlist(lapply(cohorts, function(co) {
    if (identical(co$type, "concept_cohort")) chr_vec(co$parameters$codelist_id) else character(0)
  }))
  idle <- Filter(function(cl) !as.character(cl$id %||% "") %in% used, sap$codelists %||% list())
  add("Codelists", vapply(idle,
    function(cl) sprintf("No cohort uses '%s'.", sapItemLabel(cl, "codelist")), character(1)))

  for (a in sap$analyses %||% list()) {
    p <- a$parameters %||% list()
    errs <- character(0)
    type <- as.character(a$type %||% "")
    if (type %in% c("incidence", "point_prevalence", "period_prevalence")) {
      d <- sapItemById(cohorts, p$denominator_cohort_id)
      if (!is.null(d) && !sapIsDenominatorType(d$type)) {
        errs <- c(errs, sprintf(
          "'%s' is not a denominator cohort; the denominator must be a denominator or target denominator.",
          sapItemLabel(d, "cohort")))
      }
      for (slot in c("outcome_cohort_id", "censor_cohort_id")) {
        ch <- sapItemById(cohorts, p[[slot]])
        if (!is.null(ch) && sapIsDenominatorType(ch$type)) {
          errs <- c(errs, sprintf(
            "%s '%s' is a generated denominator; it must be a plain cohort.",
            gsub("_", " ", sub("_cohort_id$", "", slot)), sapItemLabel(ch, "cohort")))
        }
      }
      errs <- c(errs, validate_strata_against(p$strata, d))
    }
    if (type %in% c("single_event_survival", "competing_risk_survival")) {
      for (slot in c("target_cohort_id", "outcome_cohort_id", "competing_outcome_cohort_id")) {
        ch <- sapItemById(cohorts, p[[slot]])
        if (!is.null(ch) && sapIsDenominatorType(ch$type)) {
          errs <- c(errs, sprintf(
            "%s '%s' is a generated denominator; survival runs on plain cohorts.",
            gsub("_", " ", sub("_cohort_id$", "", slot)), sapItemLabel(ch, "cohort")))
        }
      }
      if (!is.null(p[["censor_on_date"]]) && !is.null(p[["censor_on_date_variable"]])) {
        errs <- c(errs, paste("Both a censoring date and a censoring date column are set;",
                              "CohortSurvival takes one or the other."))
      }
    }
    errs <- c(errs, source_coverage_problems(a, sap))
    add(sapItemLabel(a, "analysis"), errs)
  }
  acc$found
}

#' Check a SAP for structural and semantic problems
#'
#' Combines the structural checks from [checkSap()] with semantic validation
#' (cohort type constraints, strata validity, data source coverage). Useful
#' for programmatic SAP construction — for example by an LLM — where the
#' Shiny UI is not available to surface warnings.
#'
#' @param sap A SAP object from [readSap()] or [createSap()].
#' @return A list of problems. Each element is a list with:
#'   \describe{
#'     \item{`name`}{The item or section the problem belongs to.}
#'     \item{`messages`}{A character vector of problem descriptions.}
#'   }
#'   An empty list means no problems were found.
#' @export
sapProblems <- function(sap) {
  c(format_check_problems(checkSap(sap), sap),
    semantic_problems(sap))
}
