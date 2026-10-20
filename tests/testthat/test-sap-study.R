test_that("the study directory holds the three scripts the SAP owns", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  files <- shinySAP:::study_files_list(sap)
  expect_setequal(names(files), unlist(shinySAP:::STUDY_PATHS))
  for (f in files) expect_match(f, "Study:   C1-001", fixed = TRUE)
  codelists <- files[["codelist/codelistCreation.R"]]
  expect_match(codelists, "importCodelist(", fixed = TRUE)
  expect_match(codelists, "importConceptSetExpression(", fixed = TRUE)
  expect_match(codelists, "importCodelistWithDetails(", fixed = TRUE)
  cohorts <- files[["cohorts/instantiateCohorts.R"]]
  expect_identical(lengths(regmatches(cohorts, gregexpr("addCohortTableIndex()", cohorts, fixed = TRUE))), 3L)
  expect_false(grepl("library(", cohorts, fixed = TRUE))
  analyses <- files[["analyses/estimates.R"]]
  var <- shinySAP:::estimate_var_names(sap$analyses)[[1]]
  expect_match(analyses, sprintf('results[["%s"]] <- estimatePointPrevalence(', var), fixed = TRUE)
  expect_false(grepl("suppress(", analyses, fixed = TRUE))
  expect_no_error(parse(text = analyses))
})

test_that("writeSapStudy writes the scripts and the codelist files the scripts read", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  dir <- withr::local_tempdir()
  paths <- shinySAP::writeSapStudy(sap, dir)
  expect_true(file.exists(file.path(dir, "codelist", "codelistCreation.R")))
  expect_true(file.exists(file.path(dir, "codelist", "follicular_lymphoma", "follicular_lymphoma.json")))
  back <- omopgenerics::importCodelist(file.path(dir, "codelist", "follicular_lymphoma"), type = "json")
  expect_identical(unclass(back)$follicular_lymphoma, c(4147411L, 4300704L))
  cse <- omopgenerics::importConceptSetExpression(file.path(dir, "codelist", "hairy_cell_leukaemia"), type = "json")
  expect_s3_class(cse, "concept_set_expression")
  expect_true(all(file.exists(paths)))
})

test_that("sapStudyExportNotes warns about guards only when something is restricted", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  expect_match(shinySAP::sapStudyExportNotes(sap), "restricted to named databases")
  minimal <- readSap(test_path("fixtures", "sap-minimal.json"))
  expect_false(grepl("restricted", shinySAP::sapStudyExportNotes(minimal)))
})
