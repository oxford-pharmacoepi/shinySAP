test_that("R literal helpers render the package's own idioms", {
  expect_identical(shinySAP:::r_number(150), "150")
  expect_identical(shinySAP:::r_number(Inf), "Inf")
  expect_identical(shinySAP:::r_chr_vec("years"), '"years"')
  expect_identical(shinySAP:::r_chr_vec(c("a", "b")), 'c("a", "b")')
  expect_null(shinySAP:::r_chr_vec(character(0)))
  expect_identical(shinySAP:::r_bound_list(list(c(0, Inf))), "c(0, Inf)")
  expect_identical(shinySAP:::r_bound_list(list(c(0, 17)), always_list = TRUE), "list(c(0, 17))")
  expect_identical(shinySAP:::r_date_range(as.Date(c("2010-01-01", NA))), 'as.Date(c("2010-01-01", NA))')
  expect_null(shinySAP:::r_date_range(as.Date(c(NA, NA))))
  expect_identical(shinySAP:::r_strata(list("sex", c("sex", "age_group"))), 'list("sex", c("sex", "age_group"))')
  expect_identical(shinySAP:::r_call("f", list(a = "1", b = NULL, c = '"x"')), 'f(\n  a = 1,\n  c = "x"\n)')
  expect_identical(shinySAP::cohortTableName("General population (18+)"), "general_population_18")
  expect_identical(shinySAP::cohortTableName("2010 cohort"), "c_2010_cohort")
  expect_true(is.na(shinySAP::cohortTableName("")))
})

test_that("long list arguments wrap under their opening paren", {
  long <- sprintf("list(%s)", paste(sprintf("c(%d, %d)", 0:13 * 5, 0:13 * 5 + 4), collapse = ", "))
  out <- shinySAP:::r_wrap_value(long, "  ageGroup = ")
  expect_true(grepl("\n", out))
  expect_identical(gsub("\\s+", "", out), gsub("\\s+", "", long))
})

test_that("the fixture generates a script that parses and names every estimator", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  script <- shinySAP::sapToScript(sap)
  expect_no_error(parse(text = script))
  expect_match(script, "library(IncidencePrevalence)", fixed = TRUE)
  expect_match(script, "library(CohortConstructor)", fixed = TRUE)
  expect_match(script, "library(CohortSurvival)", fixed = TRUE)
  expect_match(script, paste0("omopgenerics::importCodelist(path = here::here(",
                              "\"codelist/follicular_lymphoma\"), type = \"json\")"), fixed = TRUE)
  expect_match(script, "cdm$follicular_lymphoma <- CohortConstructor::conceptCohort(", fixed = TRUE)
  expect_match(script, "conceptSet = follicular_lymphoma", fixed = TRUE)
  expect_match(script, "cdm <- generateDenominatorCohortSet(", fixed = TRUE)
  expect_match(script, 'targetCohortTable       = "follicular_lymphoma"', fixed = TRUE)
  expect_match(script, "estimatePointPrevalence(", fixed = TRUE)
  expect_match(script, "estimatePeriodPrevalence(", fixed = TRUE)
  # washout = Inf is written as absent (see R/sap_json.R), so the call omits it and
  # the estimator default -- Inf -- applies.
  expect_false(grepl("outcomeWashout", shinySAP:::analysis_r_code(sap, sap$analyses[[3]])$code, fixed = TRUE))
  expect_match(script, "estimateSingleEventSurvival(", fixed = TRUE)
  expect_match(script, 'censorOnDate                = "end_of_study"', fixed = TRUE)
  expect_match(script, "No estimator maps onto analysis type 'other'", fixed = TRUE)
  expect_match(script, "omopgenerics::suppress(results, minCellCount = 5)", fixed = TRUE)
  # The target cohort is instantiated elsewhere: no call, no heading.
  expect_false(grepl("External target", script, fixed = TRUE))
})

test_that("an unbounded washout held in R renders as Inf", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  a <- sap$analyses[[3]]
  a$parameters$washout <- Inf
  expect_match(shinySAP:::analysis_r_code(sap, a)$code, "outcomeWashout += Inf")
})

test_that("an argument the author never decided is omitted", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  a <- sap$analyses[[1]]
  a$parameters$time_point <- NULL
  a$parameters$strata <- NULL
  code <- shinySAP:::analysis_r_code(sap, a)$code
  expect_false(grepl("timePoint", code))
  expect_false(grepl("strata", code))
  expect_false(grepl("includeOverallStrata", code))   # inert without strata
})

test_that("cohort references resolve by id and dangling ids are dropped, not invented", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  a <- sap$analyses[[3]]
  a$parameters$outcome_cohort_id <- "coh_99"
  code <- shinySAP:::analysis_r_code(sap, a)$code
  expect_false(grepl("outcomeTable", code))
  expect_match(code, 'denominatorTable += "general_population_denominator"')
})

test_that("a restricted analysis is guarded on the data source names, an unrestricted one is not", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  secs <- shinySAP::sapScriptSections(sap)
  est <- Filter(function(s) identical(s$group, "Estimates"), secs)
  survival <- Filter(function(s) identical(s$title, "Survival after follicular lymphoma"), est)[[1]]
  expect_match(survival$code, 'omopgenerics::cdmName(cdm) %in% c("CPRD GOLD", "SIDIAP")', fixed = TRUE)
  expect_match(survival$code, "omopgenerics::emptySummarisedResult()", fixed = TRUE)
  incidence <- Filter(function(s) identical(s$title, "Incidence of follicular lymphoma"), est)[[1]]
  expect_false(grepl("cdmName", incidence$code))
  # A concept cohort built everywhere is not guarded either.
  cohort <- Filter(function(s) identical(s$group, "Cohorts"), secs)[[1]]
  expect_false(grepl("cdmName", cohort$code))
})

test_that("sections run in dependency order and libraries are derived", {
  sap <- readSap(test_path("fixtures", "sap-c1-001-v1.0.0.json"))
  groups <- unique(vapply(shinySAP::sapScriptSections(sap), function(s) s$group, character(1)))
  expect_identical(groups, c("Libraries", "Codelists", "Cohorts", "Denominator cohort sets",
                             "Estimates", "Result suppression"))
  minimal <- readSap(test_path("fixtures", "sap-minimal.json"))
  expect_identical(shinySAP::sapToScript(minimal), "")
})

test_that("estimate variable names are readable and unique", {
  vars <- shinySAP:::estimate_var_names(list(list(name = "Same"), list(name = "Same"), list()))
  expect_identical(vars, c("same_1", "same_2", "estimate_3"))
})
