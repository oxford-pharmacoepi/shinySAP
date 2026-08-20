test_that("schema accessors expose the current version", {
  expect_identical(currentSapSchemaVersion(), "0.1.0")
  expect_identical(schemaTypes("analysis"), c("incidence", "other"))
  expect_true(all(c("study", "data_sources", "analyses") %in%
                  schemaObjectNames("sap")))
  expect_true("parameters.denominator_cohort_id" %in%
              schemaFields("analysis", "incidence")$path)
})

test_that("constructors and createSap use snake_case SAP fields", {
  description <- structure(list(), class = "data_source_description")
  content <- structure(list(), class = "codelist")

  study <- newSapStudy("study_001", "My study", authors = c("A", "B"))
  source <- newSapDataSource("ds_001", "CPRD GOLD", description)
  codelist <- newSapCodelist("cl_001", "Acetaminophen", "codelist", content)
  cohort <- newSapCohort(
    "coh_001", "Acetaminophen cohort", "ds_001", "concept_cohort",
    list(codelist_id = "cl_001", exit = "event_start_date",
         overlap = "extend")
  )
  analysis <- newSapAnalysis(
    "an_001", "Incidence analysis", "ds_001", "incidence",
    list(denominator_cohort_id = "coh_001", outcome_cohort_id = NULL)
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
  expect_named(sap$study, c("study_id", "title", "authors", "version", "description"))
  expect_named(sap$data_sources[[1]], c("id", "name", "description"))
  expect_identical(sap$cohorts[[1]]$parameters$codelist_id, "cl_001")
  expect_identical(sap$analyses[[1]]$parameters$denominator_cohort_id, "coh_001")
  expect_length(checkSap(sap), 0)
})

test_that("a type with no parameters is valid", {
  description <- structure(list(), class = "data_source_description")
  sap <- createSap(
    newSapStudy("study_001", "My study"),
    dataSources = list(newSapDataSource("ds_001", "CPRD GOLD", description)),
    analyses = list(newSapAnalysis("an_001", "Other analysis", "ds_001", "other"))
  )

  expect_length(checkSap(sap), 0)
  expect_identical(sap$analyses[[1]]$parameters, list())
})

test_that("whole-SAP checks catch duplicate ids and missing references", {
  description <- structure(list(), class = "data_source_description")
  sap <- createSap(
    newSapStudy("study_001", "My study"),
    dataSources = list(newSapDataSource("ds_001", "CPRD GOLD", description)),
    codelists = list(newSapCodelist(
      "cl_001", "Acetaminophen", "codelist", structure(list(), class = "codelist")
    )),
    cohorts = list(newSapCohort(
      "coh_001", "Analysis cohort", "ds_001", "concept_cohort",
      list(codelist_id = "cl_001", exit = "event_start_date",
           overlap = "extend")
    )),
    analyses = list(newSapAnalysis(
      "an_001", "Incidence analysis", "ds_001", "incidence",
      list(denominator_cohort_id = "coh_001")
    ))
  )
  sap$analyses[[1]]$parameters$denominator_cohort_id <- "missing_cohort"
  problems <- checkSap(sap)
  expect_true(any(purrr::map_lgl(problems, function(x) {
    identical(x$code, "missing_reference")
  })))

  duplicate <- sap
  duplicate$data_sources <- c(
    duplicate$data_sources,
    list(newSapDataSource("ds_001", "CPRD GOLD copy", description))
  )
  expect_error(validateSap(duplicate), "duplicate_id")
})
