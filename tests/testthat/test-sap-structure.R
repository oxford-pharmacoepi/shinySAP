test_that("schema accessors expose the current version", {
  expect_identical(currentSapSchemaVersion(), "0.1.0")
  expect_identical(
    schemaTypes("analysis"),
    c("incidence", "point_prevalence", "period_prevalence", "other")
  )
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

test_that("interval and time point values are checked against their vocabularies", {
  incidence <- function(parameters) {
    createSap(
      newSapStudy("study_001", "My study"),
      dataSources = list(newSapDataSource(
        "ds_001", "CPRD GOLD", structure(list(), class = "data_source_description")
      )),
      cohorts = list(newSapCohort(
        "coh_001", "Denominator", "ds_001", "denominator",
        list(requirement_interactions = TRUE)
      )),
      analyses = list(newSapAnalysis(
        "an_001", "Incidence analysis", "ds_001", "incidence",
        c(list(denominator_cohort_id = "coh_001"), parameters)
      ))
    )
  }
  codes <- function(sap) purrr::map_chr(checkSap(sap), "code")

  expect_length(checkSap(incidence(list(interval = "years"))), 0)
  expect_length(checkSap(incidence(list(interval = "overall"))), 0)

  bad <- incidence(list(interval = "years"))
  bad$analyses[[1]]$parameters$interval <- "fortnights"
  expect_true("invalid_value" %in% codes(bad))

  bad$analyses[[1]]$parameters$interval <- c("years", "months")
  expect_true("invalid_value_type" %in% codes(bad))

  expect_error(incidence(list(interval = "decades")), "choice between")
})

test_that("only incidence may be estimated over the overall interval", {
  expect_silent(validateParameterValue("overall", "time_interval", "interval", "incidence"))
  expect_error(
    validateParameterValue("overall", "time_interval", "interval", "point_prevalence"),
    "choice between"
  )
  expect_silent(validateParameterValue("years", "time_interval", "interval", "point_prevalence"))
})

test_that("a time point is one of start, middle or end", {
  purrr::walk(c("start", "middle", "end"), function(value) {
    expect_silent(validateParameterValue(value, "time_point", "time_point"))
  })
  expect_error(validateParameterValue("midpoint", "time_point", "time_point"), "choice between")
})

test_that("an estimation level is one of person or record", {
  purrr::walk(c("person", "record"), function(value) {
    expect_silent(validateParameterValue(value, "level", "level"))
  })
  expect_error(validateParameterValue("episode", "level", "level"), "choice between")
  expect_error(validateParameterValue(c("person", "record"), "level", "level"), "choice between")
})

test_that("reference checks follow the schema's ref column", {
  description <- structure(list(), class = "data_source_description")
  sap <- createSap(
    newSapStudy("study_001", "My study"),
    dataSources = list(newSapDataSource("ds_001", "CPRD GOLD", description)),
    cohorts = list(
      newSapCohort("coh_001", "Denominator", "ds_001", "denominator",
                   list(requirement_interactions = TRUE)),
      newSapCohort("coh_002", "Target denominator", "ds_001", "target_denominator",
                   list(target_cohort_id = "coh_001", requirements_at_entry = TRUE,
                        requirement_interactions = TRUE))
    ),
    analyses = list(newSapAnalysis(
      "an_001", "Incidence analysis", "ds_001", "incidence",
      list(denominator_cohort_id = "coh_001", censor_cohort_id = "coh_001")
    ))
  )
  expect_length(checkSap(sap), 0)

  # Neither of these was covered by the hand-written list this check replaced.
  sap$cohorts[[2]]$parameters$target_cohort_id <- c("coh_001", "ghost")
  sap$analyses[[1]]$parameters$censor_cohort_id <- "ghost"

  paths <- purrr::map_chr(checkSap(sap), "path")
  expect_true("cohorts[2].parameters.target_cohort_id" %in% paths)
  expect_true("analyses[1].parameters.censor_cohort_id" %in% paths)
})

test_that("every schema reference names a collection that exists", {
  references <- schemaReferences()
  expect_true(all(references$value_type %in% c("id", "id_vector")))
  expect_true(all(references$ref %in% schemaObjectNames("sap")))
})
