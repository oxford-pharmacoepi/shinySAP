# Draft mode: validate = FALSE builds classed components and documents from
# incomplete input; validate = TRUE keeps the package's contract.

draftSap <- function() createSap(newSapStudy(validate = FALSE), validate = FALSE)

test_that("draft constructors build classed components from nothing but an id", {
  co <- newSapCohort("coh_1", validate = FALSE)
  expect_s3_class(co, "sap_cohort")
  expect_null(co$name)
  expect_null(co$type)
  expect_identical(co$parameters, list())
  expect_named(co, c("id", "name", "data_source_id", "type", "parameters"))

  expect_s3_class(newSapAnalysis("an_1", validate = FALSE), "sap_analysis")
  expect_s3_class(newSapCodelist("cl_1", validate = FALSE), "sap_codelist")
  expect_s3_class(newSapDataSource("ds_1", validate = FALSE), "sap_data_source")
  expect_s3_class(newSapDataSourceModification("mod_1", validate = FALSE), "sap_data_source_modification")
  study <- newSapStudy(validate = FALSE)
  expect_s3_class(study, "sap_study")
  expect_null(study$study_id)
  expect_identical(study$version, "v1.0.0")

  # The id is never optional: a component without one cannot be located.
  expect_error(newSapCohort(NULL, validate = FALSE), "id")
  expect_error(newSapCohort("", validate = FALSE), "id")
})

test_that("a draft with the same content as a validated component is identical", {
  args <- list("coh_1", "Diabetes", "ds_1", "concept_cohort",
               list(codelist_id = "cl_1", exit = "event_end_date", overlap = "merge"))
  expect_identical(do.call(newSapCohort, c(args, validate = FALSE)), do.call(newSapCohort, args))
})

test_that("validate = TRUE still enforces the contract", {
  expect_error(newSapCohort("coh_1"), "name")
  expect_error(newSapCohort("coh_1", "Diabetes", "ds_1", "not_a_type"), "choice")
  expect_error(newSapCohort("coh_1", "Diabetes", "ds_1", "concept_cohort", list(codelist_id = "cl_1")),
               "missing_required_parameter")
  expect_error(newSapStudy(), "studyId")
  expect_error(newSapCodelist("cl_1", "x", "codelist"), "content")
  # description is optional in the schema, so a data source without one is valid.
  expect_s3_class(newSapDataSource("ds_1", "CPRD"), "sap_data_source")
})

test_that("draft parameters are shape-checked, not content-checked", {
  expect_error(newSapCohort("coh_1", parameters = "nope", validate = FALSE), "parameters")
  co <- newSapCohort("coh_1", type = "concept_cohort", parameters = list(bogus = 1), validate = FALSE)
  sap <- addSapComponent(draftSap(), co, validate = FALSE)
  codes <- vapply(checkSap(sap), function(p) p$code, character(1))
  expect_true("unknown_parameter" %in% codes)
})

test_that("drafts enter a document through the CRUD and checkSap reports the gaps", {
  sap <- addSapComponent(draftSap(), newSapCohort("coh_1", validate = FALSE), validate = FALSE)
  expect_s3_class(sap, "sap")
  expect_identical(sapComponentIds(sap, "cohorts"), "coh_1")
  problems <- checkSap(sap)
  paths <- vapply(problems, function(p) p$path, character(1))
  expect_true(all(c("study.study_id", "study.title", "cohorts[1].name", "cohorts[1].type") %in% paths))

  # The same call without validate = FALSE aborts on the first gap.
  expect_error(addSapComponent(draftSap(), newSapCohort("coh_1", validate = FALSE)), "Invalid SAP")

  # Filling the draft in through updateSapComponent() clears its problems.
  sap <- updateStudy(sap, newSapStudy("S1", "A study", validate = FALSE), validate = FALSE)
  sap <- addSapComponent(sap, newSapCodelist("cl_1", "Diabetes", "codelist",
                                         omopgenerics::newCodelist(list(a = 1L))), validate = FALSE)
  sap <- updateSapComponent(sap, newSapCohort("coh_1", "Diabetes", "ds_1", "concept_cohort",
                                              list(codelist_id = "cl_1", exit = "event_end_date",
                                                   overlap = "merge")), validate = FALSE)
  codes <- vapply(checkSap(sap), function(p) p$code, character(1))
  expect_identical(codes, "missing_reference")   # ds_1 is not a data source yet
  sap <- addSapComponent(sap, newSapDataSource("ds_1", "CPRD"), validate = FALSE)
  expect_length(checkSap(sap), 0)
  expect_s3_class(validateSap(sap), "sap")
})

test_that("removing a referenced component is refused when validating, allowed as a draft", {
  sap <- addSapComponent(draftSap(), newSapDataSource("ds_1", "CPRD"), validate = FALSE)
  sap <- addSapComponent(sap, newSapCohort("coh_1", "Target", "ds_1", "target", list()), validate = FALSE)
  expect_error(removeSapComponent(sap, "data_sources", "ds_1"), "missing_reference")
  draft <- removeSapComponent(sap, "data_sources", "ds_1", validate = FALSE)
  expect_length(draft$data_sources, 0)
  codes <- vapply(checkSap(draft), function(p) p$code, character(1))
  expect_true("missing_reference" %in% codes)
})

test_that("a duplicate id is refused even for a draft", {
  sap <- addSapComponent(draftSap(), newSapCohort("coh_1", validate = FALSE), validate = FALSE)
  expect_error(addSapComponent(sap, newSapCohort("coh_1", validate = FALSE), validate = FALSE), "already exists")
  expect_error(addSapComponent(sap, newSapAnalysis("coh_1", validate = FALSE), validate = FALSE), "already exists")
})

test_that("newSapId mints one past the highest id in use anywhere", {
  sap <- draftSap()
  expect_identical(newSapId(sap, "cohorts"), "coh_1")
  expect_identical(newSapId(sap, "data_sources"), "ds_1")
  sap <- addSapComponent(sap, newSapCohort("coh_001", validate = FALSE), validate = FALSE)
  expect_identical(newSapId(sap, "cohorts"), "coh_2")
  expect_identical(newSapId(sap, "cohorts", taken = c("coh_7", "coh_x", "cl_9")), "coh_8")
  # Ids are unique across collections: a data source called coh_5 blocks coh_5.
  sap <- addSapComponent(sap, newSapDataSource("coh_5", validate = FALSE), validate = FALSE)
  expect_identical(newSapId(sap, "cohorts"), "coh_6")
  expect_identical(newSapId(sap, "analyses"), "an_1")
  expect_error(newSapId(sap, "nope"), "collection")
})

test_that("components are classed on every read path", {
  x <- list(study = list(title = "x"), cohorts = list(list(id = "coh_1", name = "A")))
  sap <- newSap(x, validate = FALSE)
  expect_s3_class(sap$study, "sap_study")
  expect_s3_class(sap$cohorts[[1]], "sap_cohort")
  back <- sapFromJson(sapToJson(sap))
  expect_s3_class(back$cohorts[[1]], "sap_cohort")
  expect_s3_class(back$study, "sap_study")
  # Unknown keys survive the classing, and non-lists are left for checkSap().
  y <- newSap(list(study = list(title = "x", objectives = "o"), cohorts = list("junk")), validate = FALSE)
  expect_identical(y$study$objectives, "o")
  expect_identical(y$cohorts[[1]], "junk")
  expect_true("not_an_object" %in% vapply(checkSap(y), function(p) p$code, character(1)))
})
