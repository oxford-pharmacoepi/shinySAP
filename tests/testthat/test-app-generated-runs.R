skip_without_app()
skip_if_not_installed("IncidencePrevalence")
skip_if_not_installed("duckdb")
skip_if_not_installed("CDMConnector")

# The generated denominator and incidence code RUN against IncidencePrevalence's
# mock database. The mock ships an `outcome` cohort table; a `target`-type cohort
# named "outcome" refers to it by table name, exactly as the generated code does.
test_that("generated denominator and incidence code run on the mock cdm", {
  cdm <- IncidencePrevalence::mockIncidencePrevalence(sampleSize = 500)
  sap <- createSap(
    newSapStudy("MOCK", "Mock run"),
    cohorts = list(
      newSapCohort("coh_1", "outcome", character(), "target", list()),
      newSapCohort("coh_2", "denominator", character(), "denominator",
                   list(age_group = list(c(0, 150)), sex = "Both", days_prior_observation = 0,
                        requirement_interactions = TRUE))),
    analyses = list(
      newSapAnalysis("an_1", "incidence", character(), "incidence",
                     list(denominator_cohort_id = "coh_2", outcome_cohort_id = "coh_1",
                          interval = "years", washout = 0, repeated_events = FALSE)))
  )
  secs <- app("sap_script_sections")(sap)
  code <- vapply(Filter(function(s) !is.null(s$code) && s$group != "Libraries", secs),
                 function(s) s$code, character(1))
  env <- new.env(parent = globalenv())
  env$cdm <- cdm
  suppressMessages(library(IncidencePrevalence))
  for (block in code) eval(parse(text = block), envir = env)
  expect_s3_class(env$incidence_1, "summarised_result")
  expect_s3_class(env$results, "summarised_result")
  expect_gt(nrow(env$results), 0)
  CDMConnector::cdmDisconnect(cdm)
})
