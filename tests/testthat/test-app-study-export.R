skip_without_app()

test_that("the study directory holds the three scripts the SAP owns", {
  sap <- fixture_sap()
  files <- app("study_files")(sap)
  expect_setequal(names(files), unlist(app("STUDY_PATHS")))
  for (f in files) expect_match(f, "Study:   C1-001", fixed = TRUE)
  codelists <- files[["codelist/codelistCreation.R"]]
  expect_match(codelists, "importCodelist(", fixed = TRUE)
  expect_match(codelists, "importConceptSetExpression(", fixed = TRUE)
  expect_match(codelists, "importCodelistWithDetails(", fixed = TRUE)
  cohorts <- files[["cohorts/instantiateCohorts.R"]]
  expect_identical(lengths(regmatches(cohorts, gregexpr("addCohortTableIndex()", cohorts, fixed = TRUE))), 3L)
  expect_false(grepl("library(", cohorts, fixed = TRUE))
  analyses <- files[["analyses/estimates.R"]]
  var <- app("estimate_var_names")(sap$analyses)[[1]]
  expect_match(analyses, sprintf('results[["%s"]] <- estimatePointPrevalence(', var), fixed = TRUE)
  expect_false(grepl("suppress(", analyses, fixed = TRUE))
  expect_no_error(parse(text = analyses))
})

test_that("write_study_files writes the scripts and the codelist files the scripts read", {
  sap <- fixture_sap()
  dir <- withr::local_tempdir()
  paths <- app("write_study_files")(sap, dir)
  expect_true(file.exists(file.path(dir, "codelist", "codelistCreation.R")))
  expect_true(file.exists(file.path(dir, "codelist", "follicular_lymphoma", "follicular_lymphoma.json")))
  back <- omopgenerics::importCodelist(file.path(dir, "codelist", "follicular_lymphoma"), type = "json")
  expect_identical(unclass(back)$follicular_lymphoma, c(4147411L, 4300704L))
  cse <- omopgenerics::importConceptSetExpression(file.path(dir, "codelist", "hairy_cell_leukaemia"), type = "json")
  expect_s3_class(cse, "concept_set_expression")
  expect_true(all(file.exists(paths)))
})

test_that("export notes warn about guards only when something is restricted", {
  sap <- fixture_sap()
  expect_match(app("study_export_notes")(sap), "restricted to named databases")
  minimal <- readSap(test_path("fixtures", "sap-minimal.json"))
  expect_false(grepl("restricted", app("study_export_notes")(minimal)))
})
