skip_without_app()
skip_if_not_installed("knitr")
skip_if_not_installed("flextable")
skip_if_not_installed("officer")

# knitr::knit() alone -- no pandoc -- proves the template's own R runs against a
# SAP in the package shape and reaches every section.
test_that("the preview template knits against the fixture", {
  sap <- fixture_sap()
  out <- withr::local_tempfile(fileext = ".md")
  withr::with_dir(app_dir, knitr::knit("sap_preview.Rmd", output = out, quiet = TRUE,
                                       envir = list2env(list(params = list(sap = sap)), parent = globalenv())))
  txt <- paste(readLines(out), collapse = "\n")
  for (heading in c("# Study information", "# Data sources", "# Codelists", "# Description of cohorts",
                    "# Analyses", "# Appendix: analysis code", "# Appendix: codelist contents")) {
    expect_match(txt, heading, fixed = TRUE)
  }
  expect_match(txt, "estimateSingleEventSurvival(", fixed = TRUE)
  expect_match(txt, "Follicular lymphoma denominator", fixed = TRUE)
  expect_match(txt, "schema 0.1.0", fixed = TRUE)
})

test_that("an empty SAP knits too", {
  sap <- readSap(test_path("fixtures", "sap-minimal.json"))
  out <- withr::local_tempfile(fileext = ".md")
  expect_no_error(withr::with_dir(app_dir, knitr::knit(
    "sap_preview.Rmd", output = out, quiet = TRUE,
    envir = list2env(list(params = list(sap = sap)), parent = globalenv()))))
  expect_match(paste(readLines(out), collapse = "\n"), "_No cohorts defined._", fixed = TRUE)
})
