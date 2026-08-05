# The preview render ------------------------------------------------------------
#
# mod_review.R renders inst/sap_preview.Rmd in a FRESH environment
# (new.env(parent = globalenv())), and the document sources its own subset of
# R/ -- utils, cohort_kinds, analysis_registry, sap_code, cohort_operations --
# via source_app(). Nothing else is loaded there, so a generator that reaches
# into any other file breaks every preview with "could not find function",
# which the rest of the suite cannot see because helper-app.R sources all of
# R/ into one environment. (concept_set_r_code() briefly called
# codelist_csv_path() from sap_study_export.R and every render failed.)
#
# knit() executes exactly the chunks a render would, without needing pandoc.

test_that("the preview knits with only the files it sources itself", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("flextable")
  rmd <- file.path(app_root, "inst", "sap_preview.Rmd")
  sap_file <- file.path(app_root, "output", "sap-c1-001-v1.0.json")
  skip_if(!file.exists(sap_file), "sample SAP not present")

  env <- new.env(parent = globalenv())
  env$params <- list(sap = read_sap(sap_file))
  out <- tempfile(fileext = ".md")
  # knit from inst/ itself, as a render does, so knitr never has to switch
  # between its input directory and testthat's and warn about it.
  withr::local_dir(dirname(rmd))
  expect_no_error(knitr::knit(basename(rmd), output = out, envir = env, quiet = TRUE))
})
