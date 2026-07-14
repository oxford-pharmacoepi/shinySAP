#!/usr/bin/env Rscript
# Checks the JSON contract the app promises: list fields are always arrays
# (even with one entry), blank text is null, a saved SAP reads back, and every
# analysis template round-trips through collect() -> JSON -> flatten().

library(jsonlite)
library(shiny)
library(bslib)

# utils.R and dynamic_items.R come first here, unlike under Shiny's loadSupport():
# the templates call %||% and entity_picker() when they run, and the tests run
# them. The templates are then globbed exactly as the app finds them, so a new
# analysis_type_*.R file is covered by the checks below without touching this
# file.
source("R/utils.R")
source("R/dynamic_items.R")
source("R/analysis_registry.R")
for (f in sort(list.files("R", pattern = "^analysis_type_.*\\.R$", full.names = TRUE))) source(f)

failures <- 0
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n", sep = "")
  if (!isTRUE(ok)) failures <<- failures + 1
}

sap <- list(
  sap_schema_version = "0.3.0",
  generated_at = "2026-07-09T00:00:00+0000",
  study = list(
    title = "Metformin and lactic acidosis",
    acronym = blank_to_na("   "),
    authors = as_array("A. Researcher"),          # single author
    version = "1.0",
    date = "2026-07-09",
    background = blank_to_na(""),
    objectives = as_array(split_lines("Estimate incidence\n\n  Characterise users  "))
  ),
  cdm_sources = list(list(
    name = "CPRD GOLD",
    source_key = "cprd",
    data_type = "Primary care records",
    population_size = 12000000,
    cdm_version = "5.4",
    country = blank_to_na("")
  )),
  cdm_changes = list(),
  cohorts = list(list(
    name = "Metformin new users",
    role = "Target",
    cohort_id = 1001,
    entry_events = as_array(split_lines("First metformin dispensation")),
    inclusion_criteria = as_array(character(0)),
    washout_days = NA
  )),
  # No analysis_type: an analysis can be saved without one, and it is exactly the
  # input that breaks a resolver that indexes the registry directly.
  proposed_analyses = list(list(
    name = "Incidence",
    data_sources = as_array("CPRD GOLD"),
    parameters = list(
      outcome_cohort = "Lactic acidosis",
      time_at_risk = list(start_offset_days = 1, start_anchor = "cohort start")
    )
  ))
)

txt <- as.character(sap_json(sap))
back <- fromJSON(txt, simplifyVector = FALSE)

check("blank text serialises to null", is.null(back$study$acronym))
check("single author stays an array", length(back$study$authors) == 1 && is.list(back$study$authors))
check("objectives split and trimmed", identical(unlist(back$study$objectives),
                                                c("Estimate incidence", "Characterise users")))
check("empty section is an array", grepl('"cdm_changes": \\[\\]', txt))
check("empty criteria list is an array", grepl('"inclusion_criteria": \\[\\]', txt))
check("NA numeric serialises to null", is.null(back$cohorts[[1]]$washout_days))
check("scalars are unboxed", identical(back$study$title, "Metformin and lactic acidosis"))
check("nested time_at_risk survives",
      back$proposed_analyses[[1]]$parameters$time_at_risk$start_offset_days == 1)
check("cdm_sources serialises", identical(back$cdm_sources[[1]]$source_key, "cprd"))
check("single data_source stays an array",
      is.list(back$proposed_analyses[[1]]$data_sources) &&
        length(back$proposed_analyses[[1]]$data_sources) == 1)

# Section rename: 0.2.0 reads proposed_analyses, but must still load 0.1.0 files.
check("coalesce_key prefers the new name",
      identical(coalesce_key(list(proposed_analyses = list("new"), analyses = list("old")),
                             "proposed_analyses", "analyses"), list("new")))
check("coalesce_key falls back to the old name",
      identical(coalesce_key(list(analyses = list("old")), "proposed_analyses", "analyses"),
                list("old")))
check("coalesce_key on an empty new key falls back",
      identical(coalesce_key(list(proposed_analyses = list(), analyses = list("old")),
                             "proposed_analyses", "analyses"), list("old")))
check("coalesce_key with neither key gives an empty list",
      identical(coalesce_key(list(), "proposed_analyses", "analyses"), list()))

check("slugify", identical(slugify("Metformin & Lactic Acidosis!"), "metformin-lactic-acidosis"))
check("slugify empty falls back", identical(slugify(""), "sap"))
check("join_lines round-trips split_lines",
      identical(split_lines(join_lines(list("a", "b"))), c("a", "b")))

pf <- prefiller(list(name = "x", missing = NA))
check("prefiller returns value", identical(pf("name"), "x"))
check("prefiller default on NA", identical(pf("missing", "d"), "d"))
check("prefiller default on absent", identical(pf("nope", "d"), "d"))

tmp <- file.path(tempdir(), "sap-out")
path <- save_sap(sap, tmp)
check("save_sap writes a file", file.exists(path))
check("filename is slugged", grepl("^sap-metformin-and-lactic-acidosis-\\d{8}-\\d{6}\\.json$", basename(path)))
check("saved file reads back", identical(read_sap(path)$study$title, sap$study$title))
unlink(tmp, recursive = TRUE)

# Analysis type resolution ----------------------------------------------------
#
# analysis_template() is called on whatever a saved file happens to hold, and
# load() has already cleared the section by then -- so nothing here may error.

check("resolver: NULL falls back", identical(canonical_analysis_type(NULL), ANALYSIS_TYPES[1]))
check("resolver: NA falls back", identical(canonical_analysis_type(NA), ANALYSIS_TYPES[1]))
check("resolver: empty string falls back", identical(canonical_analysis_type(""), ANALYSIS_TYPES[1]))
check("resolver: a renamed type is aliased",
      identical(canonical_analysis_type("Incidence rate"), "Incidence"))
check("resolver: serialised estimator names resolve to the Prevalence template",
      identical(canonical_analysis_type("estimatePointPrevalence"), "Prevalence") &&
        identical(canonical_analysis_type("estimatePeriodPrevalence"), "Prevalence"))
check("resolver: a known type is left alone",
      identical(canonical_analysis_type("Prevalence"), "Prevalence"))

check("template: NULL resolves without error",
      identical(analysis_template(NULL), ANALYSIS_TEMPLATES[["Other"]]))
check("template: a renamed type gets the Incidence template",
      identical(analysis_template("Incidence rate"), ANALYSIS_TEMPLATES[["Incidence"]]))
check("template: an unknown type falls back to Other",
      identical(analysis_template("Bayesian hierarchical model"), ANALYSIS_TEMPLATES[["Other"]]))
check("template: a type with no entry yet falls back to Other",
      identical(analysis_template("Case-control"), ANALYSIS_TEMPLATES[["Other"]]))

check("Other is always present, it is the fallback",
      "Other" %in% names(ANALYSIS_TEMPLATES))
check("every template key is an offered analysis type",
      all(names(ANALYSIS_TEMPLATES) %in% ANALYSIS_TYPES))

# Template input ids ----------------------------------------------------------

picker_ids <- function(tmpl) unlist(tmpl$pickers, use.names = FALSE) %||% character(0)
all_pickers <- unique(unlist(lapply(ANALYSIS_TEMPLATES, picker_ids)))

for (type in names(ANALYSIS_TEMPLATES)) {
  tmpl <- ANALYSIS_TEMPLATES[[type]]
  ids  <- template_field_ids(tmpl)     # also a smoke test that ui() runs at all
  pk   <- picker_ids(tmpl)

  check(sprintf("[%s] ui() renders at least one input", type), length(ids) > 0)
  check(sprintf("[%s] no id collides with a common field", type),
        length(intersect(ids, RESERVED_INPUT_IDS)) == 0)
  check(sprintf("[%s] every declared picker is actually rendered", type), all(pk %in% ids))
  # An id that sync_pickers() owns in one template must not be a plain input in
  # another: after a type switch the stale selectize string would be handed to
  # whatever widget the new template built for that id.
  check(sprintf("[%s] no id is a picker elsewhere but a plain input here", type),
        length(setdiff(intersect(ids, all_pickers), pk)) == 0)
}

# Template round-trips: collect() -> JSON -> flatten() -> prefiller() ----------

round_trip <- function(tmpl, input) {
  params <- tmpl$collect(input)
  rt     <- fromJSON(as.character(sap_json(params)), simplifyVector = FALSE)
  list(json = rt, pf = prefiller(tmpl$flatten(rt)))
}

inc <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(
  denominator_cohort = "Metformin new users",
  outcome_cohort     = "Lactic acidosis",
  denominator_unit   = "person-years",
  rate_multiplier    = 1000,
  repeated_events    = FALSE,
  calendar_intervals = "2015-2019\n2020-2024",
  tar_start_offset   = 1, tar_start_anchor = "cohort start",
  tar_end_offset     = 0, tar_end_anchor   = "cohort end",
  stratifications    = "Sex",
  sensitivity_analyses = ""
))
check("incidence: time_at_risk nests", inc$json$time_at_risk$start_offset_days == 1)
check("incidence: rate multiplier survives", inc$json$rate_multiplier == 1000)
check("incidence: an unticked checkbox is false, not null", identical(inc$json$repeated_events, FALSE))
check("incidence: an empty textarea is an array", identical(inc$json$sensitivity_analyses, list()))
# == not identical(): a whole number comes back from JSON as an integer, and
# numericInput() is happy with either.
check("incidence: flatten feeds the time-at-risk inputs", inc$pf("tar_start_offset") == 1)
check("incidence: flatten feeds the time-at-risk anchors",
      identical(inc$pf("tar_end_anchor"), "cohort end"))
check("incidence: prefiller recovers a picker value",
      identical(inc$pf("denominator_cohort"), "Metformin new users"))
check("incidence: prefiller recovers a textarea",
      identical(unlist(inc$pf("calendar_intervals")), c("2015-2019", "2020-2024")))

prev <- round_trip(ANALYSIS_TEMPLATES[["Prevalence"]], list(
  denominatorTable          = "Metformin new users",
  outcomeTable              = "Lactic acidosis",
  prevalence_type           = "Period prevalence",
  period_interval           = "overall",
  point_interval            = "months",   # stale value on the hidden point select
  timePoint                 = "start",    # ditto
  fullContribution          = TRUE,
  completeDatabaseIntervals = TRUE,
  level                     = "person",
  includeOverallStrata      = TRUE,
  strata                    = "Sex\n10-year age bands"
))
check("prevalence: emits no time_at_risk", is.null(prev$json$time_at_risk))
check("prevalence: strata split into an array",
      identical(unlist(prev$json$strata), c("Sex", "10-year age bands")))
check("prevalence: sensitivity_analyses is gone",
      !"sensitivity_analyses" %in% names(prev$json))
check("prevalence: includeOverallStrata serialises when strata exist",
      identical(prev$json$includeOverallStrata, TRUE))
check("prevalence: period reads the period interval select", identical(prev$json$interval, "overall"))
check("prevalence: a ticked checkbox is true", identical(prev$json$fullContribution, TRUE))
check("prevalence: estimation level survives", identical(prev$json$level, "person"))
check("prevalence: point-only timePoint is not serialised for a period analysis",
      !"timePoint" %in% names(prev$json))
check("prevalence: prevalence_type is not among the parameters",
      !"prevalence_type" %in% names(prev$json))

# The point/period split serialises as the analysis_type, one level above the
# parameters, and flatten() recovers the select from it on load.
check("prevalence: serialised_type names the period estimator",
      identical(ANALYSIS_TEMPLATES[["Prevalence"]]$serialised_type(
        list(prevalence_type = "Period prevalence")), "estimatePeriodPrevalence"))
check("prevalence: serialised_type defaults to the point estimator",
      identical(ANALYSIS_TEMPLATES[["Prevalence"]]$serialised_type(list()),
                "estimatePointPrevalence"))
check("prevalence: flatten recovers the type select from the estimator name",
      identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
        list(analysis_type = "estimatePeriodPrevalence")))("prevalence_type"),
        "Period prevalence"))
check("prevalence: an older file's own prevalence_type parameter wins",
      identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
        list(analysis_type = "Prevalence", prevalence_type = "Period prevalence")
      ))("prevalence_type"), "Period prevalence"))
check("prevalence: flatten feeds the period interval select",
      identical(prev$pf("period_interval"), "overall"))
check("prevalence: 'overall' never reaches the point select, it is not a choice there",
      identical(prev$pf("point_interval", "years"), "years"))

prev_pt <- round_trip(ANALYSIS_TEMPLATES[["Prevalence"]], list(
  denominatorTable          = "Metformin new users",
  denominatorCohortId       = c("2", "3"),   # selectize reports IDs as strings
  outcomeTable              = "Lactic acidosis",
  outcomeCohortId           = "7",
  prevalence_type           = "Point prevalence",
  point_interval            = "months",
  period_interval           = "overall",  # stale value on the hidden period select
  timePoint                 = "middle",
  fullContribution          = TRUE,       # stale period values, must not serialise
  completeDatabaseIntervals = TRUE,
  level                     = "person",
  includeOverallStrata      = TRUE,
  strata                    = ""
))
check("point prevalence: reads the point interval select", identical(prev_pt$json$interval, "months"))
check("point prevalence: an empty strata textarea is an array",
      identical(prev_pt$json$strata, list()))
check("point prevalence: includeOverallStrata stays true without strata",
      identical(prev_pt$json$includeOverallStrata, TRUE))
check("prevalence: includeOverallStrata defaults true before the checkbox reports",
      identical(ANALYSIS_TEMPLATES[["Prevalence"]]$collect(list())$includeOverallStrata, TRUE))
check("point prevalence: keys follow estimatePointPrevalence()'s signature order",
      identical(names(prev_pt$json),
                c("denominatorTable", "outcomeTable", "denominatorCohortId",
                  "outcomeCohortId", "interval", "timePoint",
                  "strata", "includeOverallStrata")))
check("period prevalence: keys follow estimatePeriodPrevalence()'s signature order",
      identical(names(prev$json),
                c("denominatorTable", "outcomeTable", "denominatorCohortId",
                  "outcomeCohortId", "interval", "completeDatabaseIntervals",
                  "fullContribution", "level", "strata", "includeOverallStrata")))
check("point prevalence: timePoint survives", identical(prev_pt$json$timePoint, "middle"))
check("point prevalence: period-only fields are not serialised",
      !any(c("fullContribution", "completeDatabaseIntervals", "level") %in% names(prev_pt$json)))
check("point prevalence: flatten feeds both interval selects",
      identical(prev_pt$pf("point_interval"), "months") &&
        identical(prev_pt$pf("period_interval"), "months"))
# ==, not identical(): whole numbers come back from JSON as integers.
check("prevalence: denominatorCohortId serialises as a numeric array",
      all(unlist(prev_pt$json$denominatorCohortId) == c(2, 3)))
check("prevalence: a single outcomeCohortId still serialises as an array",
      grepl('"outcomeCohortId": \\[7\\]', as.character(sap_json(
        ANALYSIS_TEMPLATES[["Prevalence"]]$collect(list(outcomeCohortId = "7"))))))
check("prevalence: no cohort set means the ID fields are null (= all)",
      is.null(prev$json$denominatorCohortId) && is.null(prev$json$outcomeCohortId))
check("prevalence: prefiller recovers the cohort IDs",
      all(unlist(prev_pt$pf("denominatorCohortId")) == c(2, 3)) &&
        unlist(prev_pt$pf("outcomeCohortId")) == 7)

# Cohort sets: the IDs offered for a denominator are its own plus those of
# every cohort naming it as parent ---------------------------------------

set_cohorts <- list(
  list(name = "Adults",       cohort_id = 1,  parent_cohort = NA),
  list(name = "Adults 18-39", cohort_id = 2,  parent_cohort = "Adults"),
  list(name = "Adults 40-64", cohort_id = 3,  parent_cohort = "Adults"),
  list(name = "Children",     cohort_id = 9,  parent_cohort = NA),
  list(name = "No ID yet",    cohort_id = NA, parent_cohort = "Adults")
)
check("subcohorts: a set is the parent's ID plus its children's",
      identical(unname(subcohort_choices("Adults", set_cohorts)), c(1, 2, 3)))
check("subcohorts: IDs are labelled with their cohort names",
      identical(names(subcohort_choices("Adults", set_cohorts)),
                c("Adults (1)", "Adults 18-39 (2)", "Adults 40-64 (3)")))
check("subcohorts: a cohort with no children is not a set",
      length(subcohort_choices("Children", set_cohorts)) < 2)
check("subcohorts: a blank or absent parent detects nothing",
      length(subcohort_choices("", set_cohorts)) == 0 &&
        length(subcohort_choices(NULL, set_cohorts)) == 0)
check("subcohorts: a child without an ID contributes nothing",
      !anyNA(subcohort_choices("Adults", set_cohorts)))

# Prevalence keys were snake_case before they were aligned with the
# IncidencePrevalence argument names; a file saved under those names must land
# on the renamed inputs.
snake_prev <- prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(list(
  denominator_cohort          = "Metformin new users",
  outcome_cohort              = "Lactic acidosis",
  time_point                  = "middle",
  full_contribution           = TRUE,
  complete_database_intervals = FALSE,
  include_overall_strata      = FALSE,
  stratifications             = list("Sex")
)))
check("snake_case prevalence: cohorts migrate",
      identical(snake_prev("denominatorTable"), "Metformin new users") &&
        identical(snake_prev("outcomeTable"), "Lactic acidosis"))
check("snake_case prevalence: timePoint migrates", identical(snake_prev("timePoint"), "middle"))
check("snake_case prevalence: checkboxes migrate without flipping",
      isTRUE(snake_prev("fullContribution")) &&
        identical(snake_prev("completeDatabaseIntervals"), FALSE) &&
        identical(snake_prev("includeOverallStrata"), FALSE))
check("snake_case prevalence: stratifications migrate to strata",
      identical(unlist(snake_prev("strata")), "Sex"))

# Loading a pre-0.3.0 prevalence analysis: a day count that names a calendar
# unit becomes the interval; free-text time_points have had no home since
# sensitivity_analyses was dropped and no longer survive.
legacy_prev <- prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(list(
  target_cohort        = "Metformin new users",
  prevalence_type      = "Point prevalence",
  interval_length_days = 30,
  time_points          = list("2020-01-01", "2021-01-01")
)))
check("legacy prevalence: target_cohort migrates to the denominator",
      identical(legacy_prev("denominatorTable"), "Metformin new users"))
check("legacy prevalence: a 30-day interval length maps to months",
      identical(legacy_prev("point_interval"), "months"))
check("legacy prevalence: an unmappable interval length falls back to the default",
      identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
        list(interval_length_days = 45)))("point_interval", "years"), "years"))

# Loading a pre-0.3.0 analysis: flat top-level keys, the old type name, and the
# generic form's `target_cohort` where the template now wants a denominator.
legacy <- list(
  name = "Legacy incidence", analysis_type = "Incidence rate",
  target_cohort = "Metformin new users", outcome_cohort = "Lactic acidosis",
  time_at_risk = list(start_offset_days = 7, start_anchor = "cohort start"),
  stratifications = list("Sex")
)
legacy_tmpl <- analysis_template(legacy$analysis_type)
legacy_pf   <- prefiller(legacy_tmpl$flatten(legacy))   # no `parameters` -> read flat
check("legacy: the old type name resolves to the Incidence template",
      identical(legacy_tmpl, ANALYSIS_TEMPLATES[["Incidence"]]))
check("legacy: time_at_risk flattens onto the inputs", legacy_pf("tar_start_offset") == 7)
check("legacy: target_cohort migrates to the denominator",
      identical(legacy_pf("denominator_cohort"), "Metformin new users"))
check("legacy: outcome_cohort survives", identical(legacy_pf("outcome_cohort"), "Lactic acidosis"))
check("migration never overwrites a denominator the file already has",
      identical(
        prefiller(ANALYSIS_TEMPLATES[["Incidence"]]$flatten(
          list(denominator_cohort = "Real denominator", target_cohort = "Stale target")
        ))("denominator_cohort"),
        "Real denominator"))

# Nothing a template collects may collide with a common key, or c() in load()
# would silently prefer the common one.
for (type in names(ANALYSIS_TEMPLATES)) {
  keys <- names(ANALYSIS_TEMPLATES[[type]]$collect(list()))
  check(sprintf("[%s] no collected key collides with a common field", type),
        length(intersect(keys, ANALYSIS_COMMON_FIELDS)) == 0)
}

cat("\n", if (failures == 0) "All checks passed.\n" else sprintf("%d check(s) failed.\n", failures), sep = "")
quit(status = if (failures == 0) 0 else 1)
