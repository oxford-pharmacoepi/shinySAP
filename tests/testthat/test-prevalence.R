# The Prevalence template: period and point round trips, cohort sets, and the
# migrations from the snake_case and pre-0.3.0 shapes.

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
  include_overall_strata    = TRUE,
  strata                    = c("sex", "sex, age_group")
))

test_that("prevalence: emits no time_at_risk", expect_null(prev$json$time_at_risk))
test_that("prevalence: strata are a list of variable groups",
          expect_identical(lapply(prev$json$strata, unlist), list("sex", c("sex", "age_group"))))
test_that("prevalence: sensitivity_analyses is gone",
          expect_false("sensitivity_analyses" %in% names(prev$json)))
test_that("prevalence: includeOverallStrata serialises camelCase from the shared checkbox",
          expect_identical(prev$json$includeOverallStrata, TRUE))
test_that("prevalence: period reads the period interval select",
          expect_identical(prev$json$interval, "overall"))
test_that("prevalence: a ticked checkbox is true",
          expect_identical(prev$json$fullContribution, TRUE))
test_that("prevalence: estimation level survives", expect_identical(prev$json$level, "person"))
test_that("prevalence: point-only timePoint is not serialised for a period analysis",
          expect_false("timePoint" %in% names(prev$json)))
test_that("prevalence: prevalence_type is not among the parameters",
          expect_false("prevalence_type" %in% names(prev$json)))

# The point/period split serialises as the analysis_type, one level above the
# parameters, and flatten() recovers the select from it on load.
test_that("prevalence: serialised_type names the period estimator",
          expect_identical(ANALYSIS_TEMPLATES[["Prevalence"]]$serialised_type(
            list(prevalence_type = "Period prevalence")), "estimatePeriodPrevalence"))
test_that("prevalence: serialised_type defaults to the point estimator",
          expect_identical(ANALYSIS_TEMPLATES[["Prevalence"]]$serialised_type(list()),
                           "estimatePointPrevalence"))
test_that("prevalence: flatten recovers the type select from the estimator name",
          expect_identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
            list(analysis_type = "estimatePeriodPrevalence")))("prevalence_type"),
            "Period prevalence"))
test_that("prevalence: an older file's own prevalence_type parameter wins",
          expect_identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
            list(analysis_type = "Prevalence", prevalence_type = "Period prevalence")
          ))("prevalence_type"), "Period prevalence"))
test_that("prevalence: flatten feeds the period interval select",
          expect_identical(prev$pf("period_interval"), "overall"))
test_that("prevalence: 'overall' never reaches the point select, it is not a choice there",
          expect_identical(prev$pf("point_interval", "years"), "years"))

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
  include_overall_strata    = TRUE
))

test_that("point prevalence: reads the point interval select",
          expect_identical(prev_pt$json$interval, "months"))
test_that("point prevalence: no strata is an empty list, the estimators' default",
          expect_identical(prev_pt$json$strata, list()))
test_that("point prevalence: includeOverallStrata stays true without strata",
          expect_identical(prev_pt$json$includeOverallStrata, TRUE))
test_that("prevalence: includeOverallStrata defaults true before the checkbox reports",
          expect_identical(ANALYSIS_TEMPLATES[["Prevalence"]]$collect(list())$includeOverallStrata,
                           TRUE))
test_that("point prevalence: keys follow estimatePointPrevalence()'s signature order",
          expect_identical(names(prev_pt$json),
                           c("denominatorTable", "outcomeTable", "denominatorCohortId",
                             "outcomeCohortId", "interval", "timePoint",
                             "strata", "includeOverallStrata")))
test_that("period prevalence: keys follow estimatePeriodPrevalence()'s signature order",
          expect_identical(names(prev$json),
                           c("denominatorTable", "outcomeTable", "denominatorCohortId",
                             "outcomeCohortId", "interval", "completeDatabaseIntervals",
                             "fullContribution", "level", "strata", "includeOverallStrata")))
test_that("point prevalence: timePoint survives", expect_identical(prev_pt$json$timePoint, "middle"))
test_that("point prevalence: period-only fields are not serialised",
          expect_false(any(c("fullContribution", "completeDatabaseIntervals", "level") %in%
                             names(prev_pt$json))))
test_that("point prevalence: flatten feeds both interval selects", {
  expect_identical(prev_pt$pf("point_interval"), "months")
  expect_identical(prev_pt$pf("period_interval"), "months")
})
# ==, not identical(): whole numbers come back from JSON as integers.
test_that("prevalence: denominatorCohortId serialises as a numeric array",
          expect_true(all(unlist(prev_pt$json$denominatorCohortId) == c(2, 3))))
test_that("prevalence: a single outcomeCohortId still serialises as an array",
          expect_match(as.character(sap_json(
            ANALYSIS_TEMPLATES[["Prevalence"]]$collect(list(outcomeCohortId = "7")))),
            '"outcomeCohortId": \\[7\\]'))
test_that("prevalence: no cohort set means the ID fields are null (= all)", {
  expect_null(prev$json$denominatorCohortId)
  expect_null(prev$json$outcomeCohortId)
})
test_that("prevalence: prefiller recovers the cohort IDs", {
  expect_true(all(unlist(prev_pt$pf("denominatorCohortId")) == c(2, 3)))
  expect_true(unlist(prev_pt$pf("outcomeCohortId")) == 7)
})

# Cohort sets: the IDs offered for a denominator are its own plus those of
# every cohort naming it as parent ---------------------------------------

set_cohorts <- list(
  list(name = "Adults",       cohort_id = 1,  parent_cohort = NA),
  list(name = "Adults 18-39", cohort_id = 2,  parent_cohort = "Adults"),
  list(name = "Adults 40-64", cohort_id = 3,  parent_cohort = "Adults"),
  list(name = "Children",     cohort_id = 9,  parent_cohort = NA),
  list(name = "No ID yet",    cohort_id = NA, parent_cohort = "Adults")
)

test_that("subcohorts: a set is the parent's ID plus its children's",
          expect_identical(unname(subcohort_choices("Adults", set_cohorts)), c(1, 2, 3)))
test_that("subcohorts: IDs are labelled with their cohort names",
          expect_identical(names(subcohort_choices("Adults", set_cohorts)),
                           c("Adults (1)", "Adults 18-39 (2)", "Adults 40-64 (3)")))
test_that("subcohorts: a cohort with no children is not a set",
          expect_lt(length(subcohort_choices("Children", set_cohorts)), 2))
test_that("subcohorts: a blank or absent parent detects nothing", {
  expect_length(subcohort_choices("", set_cohorts), 0)
  expect_length(subcohort_choices(NULL, set_cohorts), 0)
})
test_that("subcohorts: a child without an ID contributes nothing",
          expect_false(anyNA(subcohort_choices("Adults", set_cohorts))))

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

test_that("snake_case prevalence: cohorts migrate", {
  expect_identical(snake_prev("denominatorTable"), "Metformin new users")
  expect_identical(snake_prev("outcomeTable"), "Lactic acidosis")
})
test_that("snake_case prevalence: timePoint migrates",
          expect_identical(snake_prev("timePoint"), "middle"))
test_that("snake_case prevalence: checkboxes migrate without flipping", {
  expect_true(snake_prev("fullContribution"))
  expect_identical(snake_prev("completeDatabaseIntervals"), FALSE)
  expect_identical(snake_prev("include_overall_strata"), FALSE)
})
test_that("snake_case prevalence: stratifications migrate to strata tokens",
          expect_identical(snake_prev("strata"), "Sex"))
test_that("textarea-era prevalence: flat strata lines migrate to strata tokens",
          expect_identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
            list(strata = list("Sex", "10-year age bands"))))("strata"),
            c("Sex", "10-year age bands")))

# Loading a pre-0.3.0 prevalence analysis: a day count that names a calendar
# unit becomes the interval; free-text time_points have had no home since
# sensitivity_analyses was dropped and no longer survive.
legacy_prev <- prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(list(
  target_cohort        = "Metformin new users",
  prevalence_type      = "Point prevalence",
  interval_length_days = 30,
  time_points          = list("2020-01-01", "2021-01-01")
)))

test_that("legacy prevalence: target_cohort migrates to the denominator",
          expect_identical(legacy_prev("denominatorTable"), "Metformin new users"))
test_that("legacy prevalence: a 30-day interval length maps to months",
          expect_identical(legacy_prev("point_interval"), "months"))
test_that("legacy prevalence: an unmappable interval length falls back to the default",
          expect_identical(prefiller(ANALYSIS_TEMPLATES[["Prevalence"]]$flatten(
            list(interval_length_days = 45)))("point_interval", "years"), "years"))
