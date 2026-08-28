# Does the generated code RUN? --------------------------------------------------
#
# Every other test here checks the generated script as TEXT: that an argument is
# present, that a guard wraps the right block, that the file parses. Parsing is
# not running, and the gap between them is where the expensive mistakes live --
# a guarded estimate that leaves its variable undefined parses perfectly and then
# takes omopgenerics::bind() down with "object not found" at the data partner,
# which is the one place nobody wants to find out.
#
# So this file executes what the app emits, against the packages it emits for.
#
# SKIPPED unless those packages happen to be installed. This app deliberately
# does not depend on them -- it generates text and never calls IncidencePrevalence
# -- and adding them to renv.lock to satisfy a test would put a heavy dependency
# tree into a Shiny app that has no use for it at run time. Anyone who has them
# (a developer, or CI configured to install them) gets the check; everyone else
# gets a skip rather than a failure.

skip_if_not_installed("IncidencePrevalence")
skip_if_not_installed("omopgenerics")
# mockIncidencePrevalence() builds its CDM in duckdb, so the mock needs it even
# though nothing in this app does.
skip_if_not_installed("duckdb")

# The smallest SAP that exercises the whole estimates path: a generated
# denominator, an estimate on it, and the tag that carries the plan's fields into
# the result. The outcome is a PROSE cohort on purpose -- it generates no code, so
# the mock's own `outcome` table stands in for it and this test stays about the
# analyses file rather than about CohortConstructor.
runnable_sap <- function(...) {
  base <- list(
    study = list(study_code = "RUN", version = "1.0", min_cell_count = 5,
                 objectives = list(list(id = "obj_1", text = "Estimate prevalence"))),
    cdm_sources = list(list(name = "Mock", source_key = "mock")),
    cohorts = list(
      list(name = "Denominator", kind = "denominator", data_sources = list("mock"),
           ageGroup = list(c(0, 150)), sex = list("Both"),
           daysPriorObservation = list(0), requirementInteractions = TRUE),
      list(name = "Outcome", kind = "outcome", data_sources = list("mock"),
           entry_events = list("A record of the condition"))),
    proposed_analyses = list(list(
      name = "Point prevalence", analysis_type = "estimatePointPrevalence",
      role = "primary", objectives = list("obj_1"), data_sources = list("mock"),
      parameters = list(denominatorTable = "Denominator", outcomeTable = "Outcome",
                        interval = "years", timePoint = "start"))))
  utils::modifyList(base, list(...))
}

# Run the analyses file the export would write, under the conditions the harness
# provides: the packages ATTACHED (the generated files carry no library() calls
# on purpose -- codeToRun.R loads them, and a second set would be drift), and the
# two names runStudy.R puts in scope, `cdm` and `results`.
run_analyses_file <- function(sap) {
  withr::local_package("IncidencePrevalence")
  cdm <- IncidencePrevalence::mockIncidencePrevalence(sampleSize = 200)
  env <- new.env(parent = globalenv())
  assign("cdm", cdm, envir = env)
  assign("results", list(), envir = env)
  code <- study_files(sap)[["analyses/incidencePrevalence.R"]]
  eval(parse(text = code), envir = env)
  get("results", envir = env)
}

test_that("the generated analyses file runs against the real packages", {
  results <- run_analyses_file(runnable_sap())
  expect_length(results, 1)
  expect_s3_class(results[[1]], "summarised_result")
  expect_gt(nrow(results[[1]]), 0)
})

# The plan's own fields, read back out of the result the code produced. This is
# the claim the README makes -- that role and objectives reach the exported
# settings -- checked by executing it rather than by grepping the source.
test_that("the estimate comes back carrying the plan's fields", {
  s <- omopgenerics::settings(run_analyses_file(runnable_sap())[[1]])
  expect_true(all(c("sap_analysis", "sap_role", "sap_objectives") %in% names(s)))
  expect_identical(unique(s$sap_role), "primary")
  expect_identical(unique(s$sap_analysis), "Point prevalence")
  expect_identical(unique(s$sap_objectives), "1")
})

# The bug this file exists for. A source-restricted estimate is guarded, and at a
# database the plan excludes the guard is false -- so the variable has to be
# defined anyway, or binding the estimates together fails. Here the guard names a
# database the mock is not, so every estimate takes the else branch.
test_that("a guarded-out estimate still binds", {
  sap <- runnable_sap()
  sap$cdm_sources <- list(list(name = "Mock", source_key = "mock"),
                          list(name = "Other", source_key = "elsewhere"))
  sap$proposed_analyses[[1]]$data_sources <- list("elsewhere")
  results <- run_analyses_file(sap)

  expect_length(results, 1)
  expect_s3_class(results[[1]], "summarised_result")
  expect_identical(nrow(results[[1]]), 0L)          # not run here, as planned
  # The whole point: this is what suppression_r_code() emits over the estimates.
  expect_no_error(omopgenerics::bind(results))
})

# An unrestricted plan must not emit a guard at all, and must still run.
test_that("an unguarded estimate runs and produces rows", {
  results <- run_analyses_file(runnable_sap())
  expect_false(grepl("cdmName", study_files(runnable_sap())[["analyses/incidencePrevalence.R"]],
                     fixed = TRUE))
  expect_gt(nrow(results[[1]]), 0)
})
