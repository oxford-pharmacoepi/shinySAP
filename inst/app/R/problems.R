# Problems shown on the Review tab ---------------------------------------------
#
# Two sources, one shape -- list(name = <item>, messages = <character>):
#
#   * STRUCTURAL problems come from shinySAP::checkSap(): missing required
#     fields, wrong value types, unknown fields, duplicate ids, references to ids
#     nothing defines. format_check_problems() only regroups them by item.
#   * SEMANTIC problems are the handful of checks the schema cannot express --
#     that a denominator slot names a denominator, that strata are columns the
#     denominator carries -- and live in semantic_problems().
#
# Warn-not-block, as ever: a SAP is written incrementally and half-finished is
# its normal state. No Shiny here, so the tests and the preview can call these.

# "cohorts[3].parameters.codelist_id" -> the cohort's display name and the field.
problem_location <- function(sap, path) {
  path <- as.character(path %||% "")
  m <- regmatches(path, regexec("^([a-z_]+)\\[(\\d+)\\](?:\\.(.*))?$", path, perl = TRUE))[[1]]
  if (length(m) == 4 && nzchar(m[[1]])) {
    collection <- m[[2]]
    index <- as.integer(m[[3]])
    item <- sap[[collection]][[index]]
    object <- shinySAP::sapSchemaVocabulary()$collections[[collection]]
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

# Lookups ------------------------------------------------------------------------

sap_item_by_id <- function(items, id) {
  id <- as.character(id %||% "")[1]
  if (is.na(id) || !nzchar(id)) return(NULL)
  for (item in items %||% list()) if (identical(as.character(item$id %||% ""), id)) return(item)
  NULL
}

item_label <- function(item, noun) {
  nm <- as.character(item$name %||% "")
  if (nzchar(nm)) nm else sprintf("Untitled %s", noun)
}

is_denominator_type <- function(type) {
  as.character(type %||% "") %in% c("denominator", "target_denominator")
}

# Semantic checks ---------------------------------------------------------------

semantic_problems <- function(sap) {
  acc <- new.env(parent = emptyenv())
  acc$found <- list()
  add <- function(name, messages) {
    messages <- as.character(messages)
    if (!length(messages)) return(invisible(NULL))
    acc$found <- c(acc$found, list(list(name = name, messages = messages)))
  }
  cohorts <- sap$cohorts %||% list()

  # Data source modifications: a date range that runs backwards.
  for (m in sap$data_source_modifications %||% list()) {
    add(item_label(m, "modification"), date_range_problems(m$parameters$date_range))
  }

  # Cohorts.
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
        t <- sap_item_by_id(cohorts, tid)
        if (!is.null(t) && is_denominator_type(t$type)) {
          errs <- c(errs, sprintf("'%s' is a denominator; the target cohort must be a plain cohort.",
                                  item_label(t, "cohort")))
        }
      }
      for (w in p$time_at_risk %||% list()) {
        if (as.numeric(w[[1]]) < 0) {
          errs <- c(errs, sprintf("Time at risk '%s' must start at day 0 or later.",
                                  format_bound_list(list(w))))
        }
      }
    }
    add(item_label(co, "cohort"), errs)
  }
  add("Cohorts", table_name_collisions(cohorts))

  # Codelists nobody uses: a nudge, not an error.
  used <- unlist(lapply(cohorts, function(co) {
    if (identical(co$type, "concept_cohort")) chr_vec(co$parameters$codelist_id) else character(0)
  }))
  idle <- Filter(function(cl) !as.character(cl$id %||% "") %in% used, sap$codelists %||% list())
  add("Codelists", vapply(idle, function(cl) sprintf("No cohort uses '%s'.", item_label(cl, "codelist")),
                          character(1)))

  # Analyses.
  for (a in sap$analyses %||% list()) {
    p <- a$parameters %||% list()
    errs <- character(0)
    type <- as.character(a$type %||% "")
    if (type %in% c("incidence", "point_prevalence", "period_prevalence")) {
      d <- sap_item_by_id(cohorts, p$denominator_cohort_id)
      if (!is.null(d) && !is_denominator_type(d$type)) {
        errs <- c(errs, sprintf(
          "'%s' is not a denominator cohort; the denominator must be a denominator or target denominator.",
          item_label(d, "cohort")))
      }
      for (slot in c("outcome_cohort_id", "censor_cohort_id")) {
        ch <- sap_item_by_id(cohorts, p[[slot]])
        if (!is.null(ch) && is_denominator_type(ch$type)) {
          errs <- c(errs, sprintf("%s '%s' is a generated denominator; it must be a plain cohort.",
                                  gsub("_", " ", sub("_cohort_id$", "", slot)), item_label(ch, "cohort")))
        }
      }
      errs <- c(errs, validate_strata_against(p$strata, d))
    }
    if (type %in% c("single_event_survival", "competing_risk_survival")) {
      for (slot in c("target_cohort_id", "outcome_cohort_id", "competing_outcome_cohort_id")) {
        ch <- sap_item_by_id(cohorts, p[[slot]])
        if (!is.null(ch) && is_denominator_type(ch$type)) {
          errs <- c(errs, sprintf("%s '%s' is a generated denominator; survival runs on plain cohorts.",
                                  gsub("_", " ", sub("_cohort_id$", "", slot)), item_label(ch, "cohort")))
        }
      }
      if (!is.null(p[["censor_on_date"]]) && !is.null(p[["censor_on_date_variable"]])) {
        errs <- c(errs, paste("Both a censoring date and a censoring date column are set;",
                              "CohortSurvival takes one or the other."))
      }
    }
    errs <- c(errs, source_coverage_problems(a, sap))
    add(item_label(a, "analysis"), errs)
  }
  acc$found
}

date_range_problems <- function(range) {
  if (is.null(range) || length(range) < 2) return(character(0))
  range <- as.Date(range)
  if (anyNA(range) || range[[1]] <= range[[2]]) return(character(0))
  sprintf("The date range starts (%s) after it ends (%s).", range[[1]], range[[2]])
}

# The generator accepts ages within 0..150; the schema only checks the pair is
# ordered and finite.
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

# The columns generateDenominatorCohortSet() puts on the denominator table, and
# so the only things the IncidencePrevalence estimators can stratify by.
STRATA_VARIABLES <- c("age_group", "sex")

cohort_strata_variables <- function(cohort) {
  if (is.null(cohort) || !is_denominator_type(cohort$type)) return(character(0))
  STRATA_VARIABLES
}

# Two things can be wrong with a stratification: the variable is not a column
# on the denominator at all, or the column exists but the cohort has already
# collapsed it -- a male-only cohort has no sex left to vary, a single age band
# no age. `groups` is the SAP shape: a list of character vectors.
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
      errs <- c(errs, sprintf("Cannot stratify by sex: the denominator cohort is restricted to %s.", sex))
    }
    if (identical(v, "age_group") && n_ages < 2) {
      errs <- c(errs, "Cannot stratify by age_group: the denominator cohort defines fewer than two age groups.")
    }
  }
  unique(errs)
}

# An analysis restricted to data sources where a cohort it uses is never built:
# the generated script guards each on omopgenerics::cdmName(cdm), so the estimate
# would run against a table that does not exist there.
source_coverage_problems <- function(analysis, sap) {
  own <- chr_vec(analysis$data_source_id)
  if (!length(own)) return(character(0))
  errs <- character(0)
  p <- analysis$parameters %||% list()
  for (key in grep("_cohort_id$", names(p), value = TRUE)) {
    for (cid in chr_vec(p[[key]])) {
      co <- sap_item_by_id(sap$cohorts, cid)
      theirs <- chr_vec(co$data_source_id)
      if (is.null(co) || !length(theirs)) next
      missing <- setdiff(own, theirs)
      if (length(missing)) {
        errs <- c(errs, sprintf("Runs on %s, where cohort '%s' is not built.",
                                paste(data_source_names(sap, missing), collapse = ", "),
                                item_label(co, "cohort")))
      }
    }
  }
  unique(errs)
}
