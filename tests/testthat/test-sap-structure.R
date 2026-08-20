test_that("schema accessors expose the current version", {
  expect_identical(currentSapSchemaVersion(), "0.1.0")
  expect_identical(schemaTypes("analysis"), c("incidence", "other"))
  expect_true(all(c("study", "data_sources", "analyses") %in%
                  schemaObjectNames("sap")))
  expect_true("parameters.denominator_cohort_key" %in%
              schemaFields("analysis", "incidence")$path)
})

test_that("constructors and createSap use snake_case SAP fields", {
  description <- structure(list(), class = "data_source_description")
  content <- structure(list(), class = "codelist")

  study <- newSapStudy("P4-002", "My study", authors = c("A", "B"))
  source <- newSapDataSource("cprd_gold", description)
  codelist <- newSapCodelist("acetaminophen", "codelist", content)
  cohort <- newSapCohort(
    "acetaminophen_cohort", "cprd_gold", "concept_cohort",
    list(codelist_key = "acetaminophen", exit = "event_start_date",
         overlap = "extend")
  )
  analysis <- newSapAnalysis(
    "analysis_1", "cprd_gold", "incidence",
    list(denominator_cohort_key = "acetaminophen_cohort",
         outcome_cohort_key = NULL)
  )

  sap <- createSap(
    study,
    dataSources = list(source),
    codelists = list(codelist),
    cohorts = list(cohort),
    analyses = list(analysis)
  )

  expect_s3_class(sap, "sap")
  expect_named(
    sap,
    c("study", "data_sources", "data_source_modifications", "codelists",
      "cohorts", "analyses", "sap_schema_version", "generated_at")
  )
  expect_length(checkSap(sap), 0)
})

test_that("a type with no parameters is valid", {
  description <- structure(list(), class = "data_source_description")
  sap <- createSap(
    newSapStudy("P4-002", "My study"),
    dataSources = list(newSapDataSource("cprd_gold", description)),
    analyses = list(newSapAnalysis("analysis_1", "cprd_gold", "other"))
  )

  expect_length(checkSap(sap), 0)
  expect_identical(sap$analyses[[1]]$parameters, list())
})

test_that("whole-SAP checks catch duplicate keys and missing references", {
  description <- structure(list(), class = "data_source_description")
  sap <- createSap(
    newSapStudy("P4-002", "My study"),
    dataSources = list(newSapDataSource("cprd_gold", description)),
    codelists = list(newSapCodelist(
      "acetaminophen", "codelist", structure(list(), class = "codelist")
    )),
    cohorts = list(newSapCohort(
      "analysis_cohort", "cprd_gold", "concept_cohort",
      list(codelist_key = "acetaminophen", exit = "event_start_date",
           overlap = "extend")
    )),
    analyses = list(newSapAnalysis(
      "analysis_1", "cprd_gold", "incidence",
      list(denominator_cohort_key = "analysis_cohort")
    ))
  )
  sap$analyses[[1]]$parameters$denominator_cohort_key <- "missing_cohort"
  problems <- checkSap(sap)
  expect_true(any(purrr::map_lgl(problems, function(x) {
    identical(x$code, "missing_reference")
  })))

  duplicate <- sap
  duplicate$data_sources <- c(
    duplicate$data_sources,
    list(newSapDataSource("cprd_gold", description))
  )
  expect_error(validateSap(duplicate), "duplicate_key")
})
