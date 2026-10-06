# A SAP exercising every value type, all three codelist content classes and the
# null/Inf conventions the JSON layer documents.
fullSap <- function() {
  ds <- newSapDataSource("ds_1", "CPRD GOLD",
                         structure(list(), class = "data_source_description"))
  cl1 <- newSapCodelist("cl_1", "Diabetes", "codelist",
                        omopgenerics::newCodelist(list(diabetes = c(201826L, 443238L))))
  cl2 <- newSapCodelist("cl_2", "Metformin", "codelist_with_details",
                        omopgenerics::newCodelistWithDetails(list(metformin = dplyr::tibble(
                          concept_id = 1503297L, concept_name = "metformin"))))
  cl3 <- newSapCodelist("cl_3", "Stroke", "concept_set_expression",
                        omopgenerics::newConceptSetExpression(list(stroke = dplyr::tibble(
                          concept_id = 381316L, excluded = FALSE, descendants = TRUE,
                          mapped = FALSE))))
  mod <- newSapDataSourceModification(
    "mod_1", "Trim to study period", "trim_observation_period", "ds_1",
    list(date_range = as.Date(c("2010-01-01", NA))))
  target <- newSapCohort("coh_1", "Diabetes", "ds_1", "concept_cohort",
                         list(codelist_id = "cl_1", exit = "event_end_date",
                              overlap = "merge"))
  denom <- newSapCohort("coh_2", "General population", "ds_1", "denominator",
                        list(age_group = list(c(0, 17), c(18, 150)), sex = "Both",
                             days_prior_observation = 365,
                             requirement_interactions = TRUE))
  tdenom <- newSapCohort("coh_3", "Diabetes denominator", "ds_1", "target_denominator",
                         list(target_cohort_id = "coh_1",
                              target_cohort_date_range = as.Date(c("2010-01-01", "2020-12-31")),
                              time_at_risk = list(c(0, 365), c(366, Inf)),
                              requirements_at_entry = TRUE,
                              requirement_interactions = FALSE))
  inc <- newSapAnalysis("an_1", "Incidence of diabetes", "ds_1", "incidence",
                        list(denominator_cohort_id = "coh_2", outcome_cohort_id = "coh_1",
                             interval = "years", washout = Inf, repeated_events = FALSE,
                             strata = list("sex", c("sex", "age_group")),
                             include_overall_strata = TRUE))
  surv <- newSapAnalysis("an_2", "Survival", "ds_1", "single_event_survival",
                         list(target_cohort_id = "coh_1", outcome_cohort_id = "coh_2",
                              censor_on_date = as.Date("2020-12-31"),
                              follow_up_days = 365, strata = list()))
  other <- newSapAnalysis("an_3", "Something else", character(), "other", list())
  createSap(
    newSapStudy("STUDY-1", "A study", authors = "One Author", description = "About it"),
    dataSources = list(ds), dataSourceModifications = list(mod),
    codelists = list(cl1, cl2, cl3), cohorts = list(target, denom, tdenom),
    analyses = list(inc, surv, other)
  )
}

test_that("a full SAP round-trips through JSON", {
  sap <- fullSap()
  json <- sapToJson(sap)
  back <- sapFromJson(json)

  expect_s3_class(back, "sap")
  expect_length(checkSap(back), 0)
  # Idempotent: serialising the decoded SAP reproduces the same file.
  expect_identical(as.character(sapToJson(back)), as.character(json))

  # Types are restored, not just shapes.
  mod <- getSapComponent(back, "data_source_modifications", "mod_1")
  expect_s3_class(mod$parameters$date_range, "Date")
  expect_true(is.na(mod$parameters$date_range[[2]]))

  tdenom <- getSapComponent(back, "cohorts", "coh_3")
  expect_identical(tdenom$parameters$time_at_risk[[2]], c(366, Inf))
  expect_identical(tdenom$parameters$target_cohort_id, "coh_1")
  expect_s3_class(tdenom$parameters$target_cohort_date_range, "Date")

  inc <- getSapComponent(back, "analyses", "an_1")
  expect_null(inc$parameters$washout)   # Inf is written as absent
  expect_identical(inc$parameters$strata, list("sex", c("sex", "age_group")))
  expect_identical(inc$parameters$interval, "years")

  surv <- getSapComponent(back, "analyses", "an_2")
  expect_identical(surv$parameters$censor_on_date, as.Date("2020-12-31"))
  expect_identical(surv$parameters$follow_up_days, 365)

  expect_identical(back$study$authors, "One Author")
  expect_s3_class(getSapComponent(back, "data_sources", "ds_1")$description,
                  "data_source_description")
})

test_that("codelist content comes back with its omopgenerics class", {
  back <- sapFromJson(sapToJson(fullSap()))
  cl1 <- getSapComponent(back, "codelists", "cl_1")$content
  cl2 <- getSapComponent(back, "codelists", "cl_2")$content
  cl3 <- getSapComponent(back, "codelists", "cl_3")$content
  expect_s3_class(cl1, "codelist")
  expect_identical(cl1$diabetes, c(201826L, 443238L))
  expect_s3_class(cl2, "codelist_with_details")
  expect_identical(cl2$metformin$concept_name, "metformin")
  expect_s3_class(cl3, "concept_set_expression")
  expect_identical(cl3$stroke$concept_id, 381316L)
  expect_true(cl3$stroke$descendants)
})

test_that("JSON conventions: {} parameters, arrays, omitted nulls", {
  json <- as.character(sapToJson(fullSap(), pretty = FALSE))
  expect_match(json, '^\\{"sap_schema_version":"0.1.0","generated_at":')
  # Zero-length vectors are omitted like NULLs; empty parameters print as {}.
  expect_match(json, '"an_3","name":"Something else","type":"other","parameters":\\{\\}')
  expect_match(json, '"authors":\\["One Author"\\]')
  expect_match(json, '"date_range":\\["2010-01-01",null\\]')
  expect_match(json, '"time_at_risk":\\[\\[0,365\\],\\[366,null\\]\\]')
  expect_false(grepl('"washout"', json))
  expect_false(grepl(":null,", gsub('"date_range":\\["2010-01-01",null\\]', "", json)))
})

test_that("empty parameters read back as the constructors build them", {
  built <- newSapAnalysis("an_3", "Something else", "ds_1", "other")
  read <- sapFromJson(sapToJson(fullSap()))
  expect_identical(getSapComponent(read, "analyses", "an_3")$parameters, built$parameters)
  expect_identical(built$parameters, list())
})

test_that("unknown fields survive a read so checkSap can report them", {
  json <- sapToJson(fullSap())
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  x$study$objectives <- list("an objective")
  x$cohorts[[1]]$parameters$bogus <- 1
  back <- sapFromJson(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  codes <- vapply(checkSap(back), function(p) p$code, character(1))
  expect_true("unknown_field" %in% codes)
  expect_true("unknown_parameter" %in% codes)
})

test_that("a file from the retired app schema is refused by version", {
  old <- "{\"sap_schema_version\":\"0.4.30\",\"study\":{\"title\":\"x\"},\"cohorts\":[]}"
  expect_error(sapFromJson(old), "sap_schema_version must be")
  expect_error(sapFromJson("[1,2]"), "JSON object")
})

test_that("writeSap and readSap round-trip through a file", {
  path <- file.path(withr::local_tempdir(), "nested", "sap.json")
  sap <- fullSap()
  sap$generated_at <- "2020-01-01T00:00:00+0000"
  expect_identical(writeSap(sap, path), path)
  expect_true(file.exists(path))
  back <- readSap(path)
  expect_false(identical(back$generated_at, sap$generated_at))   # writeSap() stamps the file
  back$generated_at <- sap$generated_at
  expect_identical(as.character(sapToJson(back)), as.character(sapToJson(sap)))
  expect_identical(readSap(writeSap(sap, path, stamp = FALSE))$generated_at, sap$generated_at)
  expect_error(readSap(file.path(tempdir(), "nope.json")), "not found")
})

test_that("validate = TRUE aborts on an invalid document", {
  x <- jsonlite::fromJSON(sapToJson(fullSap()), simplifyVector = FALSE)
  x$analyses[[1]]$parameters$denominator_cohort_id <- "missing"
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  expect_s3_class(sapFromJson(json), "sap")
  expect_error(sapFromJson(json, validate = TRUE), "missing_reference")
})
