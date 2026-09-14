skip_without_app()

test_that("checkSap problems are regrouped by item with the field as prefix", {
  sap <- fixture_sap()
  x <- unclass(sap)
  x$cohorts[[1]]$name <- NULL
  x$analyses[[1]]$parameters$denominator_cohort_id <- "coh_99"
  x$study$title <- NULL
  x$study$bogus <- "x"
  found <- app("format_check_problems")(checkSap(x), x)
  names <- vapply(found, function(p) p$name, character(1))
  expect_true("Untitled cohort (coh_1)" %in% names)
  expect_true("Study" %in% names)
  expect_true(any(grepl("Point prevalence of follicular lymphoma", names)))
  study <- found[[which(names == "Study")]]$messages
  expect_true(any(grepl("^title: ", study)))
  expect_true(any(grepl("^bogus: ", study)))
  an <- found[[which(grepl("Point prevalence", names))]]$messages
  expect_match(an, "parameters.denominator_cohort_id: .*coh_99")
  expect_identical(app("format_check_problems")(list(), x), list())
})

test_that("a complete fixture raises no problems at all", {
  sap <- fixture_sap()
  expect_length(checkSap(sap), 0)
  expect_length(app("semantic_problems")(sap), 0)
})

semantic <- function(edit) {
  sap <- fixture_sap()
  x <- edit(unclass(sap))
  app("semantic_problems")(x)
}
messages_for <- function(found, name) {
  hit <- Filter(function(p) identical(p$name, name), found)
  unlist(lapply(hit, function(p) p$messages))
}

test_that("a denominator slot must hold a denominator, outcome slots must not", {
  found <- semantic(function(x) {
    x$analyses[[3]]$parameters$denominator_cohort_id <- "coh_1"   # a concept cohort
    x$analyses[[3]]$parameters$outcome_cohort_id <- "coh_4"       # the denominator
    x
  })
  msgs <- messages_for(found, "Incidence of follicular lymphoma")
  expect_true(any(grepl("is not a denominator cohort", msgs)))
  expect_true(any(grepl("outcome 'General population denominator' is a generated denominator", msgs)))
})

test_that("strata must be columns the denominator can still vary", {
  found <- semantic(function(x) {
    x$cohorts[[4]]$parameters$sex <- "Female"
    x$cohorts[[4]]$parameters$age_group <- list(c(0, 150))
    x$analyses[[1]]$parameters$strata <- list("sex", "age_group", "region")
    x
  })
  msgs <- messages_for(found, "Point prevalence of follicular lymphoma")
  expect_true(any(grepl("restricted to Female", msgs)))
  expect_true(any(grepl("fewer than two age groups", msgs)))
  expect_true(any(grepl("does not carry that column", msgs)))
})

test_that("a target denominator cannot be built from itself or another denominator", {
  found <- semantic(function(x) {
    x$cohorts[[5]]$parameters$target_cohort_id <- "coh_5"
    x
  })
  expect_true(any(grepl("cannot be built from itself", messages_for(found, "Follicular lymphoma denominator"))))
  found <- semantic(function(x) {
    x$cohorts[[5]]$parameters$target_cohort_id <- "coh_4"
    x
  })
  expect_true(any(grepl("is a denominator; the target cohort", messages_for(found, "Follicular lymphoma denominator"))))
})

test_that("date ranges, both censor fields, idle codelists and source coverage are reported", {
  found <- semantic(function(x) {
    x$data_source_modifications[[1]]$parameters$date_range <- as.Date(c("2020-01-01", "2010-01-01"))
    x$analyses[[5]]$parameters$censor_on_date <- as.Date("2020-12-31")
    x$cohorts[[3]]$parameters$codelist_id <- "cl_1"     # cl_3 now idle
    x$analyses[[4]]$data_source_id <- c("ds_1", "ds_2", "ds_3")   # coh_1 is built in all, coh_3 too -> none
    x$cohorts[[3]]$data_source_id <- "ds_1"              # death only in CPRD: survival runs elsewhere
    x
  })
  expect_true(any(grepl("starts .* after it ends", messages_for(found, "Restrict to the study period"))))
  expect_true(any(grepl("one or the other", messages_for(found, "Competing risk of death"))))
  expect_true(any(grepl("No cohort uses 'Death'", messages_for(found, "Codelists"))))
  expect_true(any(grepl("Runs on SIDIAP, IPCI, where cohort 'Death' is not built",
                        messages_for(found, "Survival after follicular lymphoma"))))
})

test_that("two cohorts collapsing to one table name are reported", {
  found <- semantic(function(x) {
    x$cohorts[[2]]$name <- "Follicular  Lymphoma!"
    x
  })
  expect_true(any(grepl("follicular_lymphoma", messages_for(found, "Cohorts"))))
})
