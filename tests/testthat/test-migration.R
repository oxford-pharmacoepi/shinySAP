# Loading older files: flat pre-0.3.0 analyses, and the migrate_sap() repairs
# that have to happen before any section builds its cards.

# A pre-0.3.0 analysis: flat top-level keys, the old type name, and the generic
# form's `target_cohort` where the template now wants a denominator.
legacy <- list(
  name = "Legacy incidence", analysis_type = "Incidence rate",
  target_cohort = "Metformin new users", outcome_cohort = "Lactic acidosis",
  time_at_risk = list(start_offset_days = 7, start_anchor = "cohort start"),
  stratifications = list("Sex")
)
legacy_tmpl <- analysis_template(legacy$analysis_type)
legacy_pf   <- prefiller(legacy_tmpl$flatten(legacy))   # no `parameters` -> read flat

test_that("legacy: the old type name resolves to the Incidence template",
          expect_identical(legacy_tmpl, ANALYSIS_TEMPLATES[["Incidence"]]))
test_that("legacy: target_cohort migrates to the denominator",
          expect_identical(legacy_pf("denominatorTable"), "Metformin new users"))
test_that("legacy: outcome_cohort survives",
          expect_identical(legacy_pf("outcomeTable"), "Lactic acidosis"))
test_that("migration never overwrites a denominator the file already has",
          expect_identical(
            prefiller(ANALYSIS_TEMPLATES[["Incidence"]]$flatten(
              list(denominator_cohort = "Real denominator", target_cohort = "Stale target")
            ))("denominatorTable"),
            "Real denominator"))

# Before 0.3.2 an analysis named a plain target cohort as its denominator and
# carried its own time at risk. IncidencePrevalence has no such object: the
# denominator is a cohort set generated FROM the target, with timeAtRisk as one of
# the generator's arguments. migrate_sap() synthesises the missing denominator --
# a template cannot, because an analysis can only rewrite itself, not add a cohort.
old_sap <- list(
  cohorts = list(
    list(name = "Metformin new users", role = "Target",
         entry_events = list("First metformin dispensation"), concept_set = "cs_metformin"),
    list(name = "Lactic acidosis", role = "Outcome")
  ),
  proposed_analyses = list(list(
    name = "Legacy incidence", analysis_type = "Incidence rate",
    target_cohort = "Metformin new users", outcome_cohort = "Lactic acidosis",
    time_at_risk = list(start_offset_days = 7, start_anchor = "cohort start",
                        end_offset_days = 30, end_anchor = "cohort end")
  ))
)
migrated <- migrate_sap(old_sap)
den <- migrated$cohorts[[3]]

test_that("migrate_sap: an old Target stays a PLAIN cohort, keeping its definition", {
  expect_identical(migrated$cohorts[[1]]$kind, "target")
  expect_identical(unlist(migrated$cohorts[[1]]$entry_events), "First metformin dispensation")
})
test_that("migrate_sap: the missing target denominator is synthesised", {
  expect_identical(den$kind, "target_denominator")
  expect_identical(den$targetCohortTable, "Metformin new users")
})
test_that("migrate_sap: the anchored time at risk becomes one [start, end] interval",
          expect_identical(as.numeric(den$timeAtRisk[[1]]), c(7, 30)))
test_that("migrate_sap: the analysis is repointed at the synthesised denominator",
          expect_identical(migrated$proposed_analyses[[1]]$denominator_cohort, den$name))
test_that("migrate_sap: the analysis no longer carries a time at risk",
          expect_null(migrated$proposed_analyses[[1]]$time_at_risk))
test_that("migrate_sap: unrelated cohorts are untouched", {
  expect_identical(migrated$cohorts[[2]]$kind, "outcome")
  expect_null(migrated$cohorts[[2]]$time_at_risk)
})

# Two analyses on the same target and the same window are one generator call, so
# they must share one denominator -- but a different window is a different call.
two <- migrate_sap(list(
  cohorts = list(list(name = "T", role = "Target")),
  proposed_analyses = list(
    list(name = "A", target_cohort = "T",
         time_at_risk = list(start_offset_days = 0, end_offset_days = 30)),
    list(name = "B", target_cohort = "T",
         time_at_risk = list(start_offset_days = 0, end_offset_days = 30)),
    list(name = "C", target_cohort = "T",
         time_at_risk = list(start_offset_days = 31, end_offset_days = 60))
  )
))

test_that("migrate_sap: analyses sharing a target and a window share one denominator",
          expect_identical(two$proposed_analyses[[1]]$denominator_cohort,
                           two$proposed_analyses[[2]]$denominator_cohort))
test_that("migrate_sap: a different window gets its own denominator",
          expect_false(identical(two$proposed_analyses[[1]]$denominator_cohort,
                                 two$proposed_analyses[[3]]$denominator_cohort)))
test_that("migrate_sap: one target, two windows, two synthesised denominators",
          expect_length(two$cohorts, 3))

test_that("migrate_sap: an analysis naming an undefined cohort is a no-op, not an error",
          expect_length(migrate_sap(list(
            cohorts = list(list(name = "C", role = "Outcome")),
            proposed_analyses = list(list(denominator_cohort = "Nope",
                                          time_at_risk = list(start_offset_days = 7)))
          ))$cohorts, 1))
test_that("migrate_sap: an analysis already on a real denominator is left alone",
          expect_identical(migrate_sap(list(
            cohorts = list(list(name = "D", kind = "denominator")),
            proposed_analyses = list(list(denominator_cohort = "D"))
          ))$proposed_analyses[[1]]$denominator_cohort, "D"))

# Neither generator takes a washout, and there is nowhere to move a cohort one to.
test_that("migrate_sap: an old cohort washout is dropped, not misapplied",
          expect_null(migrate_sap(list(
            cohorts = list(list(name = "D", kind = "denominator", washout_days = 365))
          ))$cohorts[[1]]$washout_days))
test_that("migrate_sap: old free-text age groups become numeric pairs",
          expect_identical(as.numeric(migrate_sap(list(cohorts = list(list(
            name = "D", kind = "denominator", age_groups = list("18-64", "65+")
          ))))$cohorts[[1]]$ageGroup[[1]]), c(18, 64)))

# 0.4.2 renamed every denominator key onto the generator's own argument names, and
# folded the two date keys into the single cohortDateRange pair the argument takes.
test_that("migrate_sap: the old study period becomes one cohortDateRange pair", {
  ch <- migrate_sap(list(cohorts = list(list(
    name = "D", kind = "denominator",
    study_period_start = "2015-01-01", study_period_end = "2024-12-31"
  ))))$cohorts[[1]]
  expect_identical(as.character(ch$cohortDateRange), c("2015-01-01", "2024-12-31"))
  expect_null(ch$study_period_start)
})
test_that("migrate_sap: the 0.3.2 split date keys also fold into the pair", {
  ch <- migrate_sap(list(cohorts = list(list(
    name = "D", kind = "denominator", cohort_date_range_start = "2015-01-01"
  ))))$cohorts[[1]]
  expect_identical(date_bound(ch$cohortDateRange, 1), "2015-01-01")
  expect_null(date_bound(ch$cohortDateRange, 2))
  expect_null(ch[["cohort_date_range_start"]])
})
test_that("migrate_sap: old snake_case denominator keys are renamed onto the arguments", {
  ch <- migrate_sap(list(cohorts = list(list(
    name = "TD", kind = "target_denominator",
    target_cohort = "T", time_at_risk = list(c(0, 30)), requirements_at_entry = FALSE,
    age_groups = list(c(18, 64)), days_prior_observation = 365,
    requirement_interactions = FALSE
  ))))$cohorts[[1]]
  expect_identical(ch$targetCohortTable, "T")
  expect_identical(as.numeric(ch$timeAtRisk[[1]]), c(0, 30))
  expect_false(ch$requirementsAtEntry)
  expect_identical(as.numeric(ch$ageGroup[[1]]), c(18, 64))
  expect_identical(as.numeric(ch$daysPriorObservation), 365)
  expect_false(ch$requirementInteractions)
  # The old names must be gone, or `$` partial matching could still find them.
  for (old in c("target_cohort", "time_at_risk", "requirements_at_entry",
                "age_groups", "days_prior_observation", "requirement_interactions")) {
    expect_null(ch[[old]], info = old)
  }
})
