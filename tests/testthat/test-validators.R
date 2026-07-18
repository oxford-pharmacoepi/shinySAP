# Template validators: problems a saved analysis reports on the Review tab.

INC <- ANALYSIS_TEMPLATES[["Incidence"]]

# The parameters are flat and camelCase, exactly as estimateIncidence() names them
# and as collect() now emits them. outcomeWashout is a one-element numeric array.
ok_params <- list(denominatorTable = "Metformin denominator",
                  outcomeWashout   = parse_washout("365"),
                  repeatedEvents   = TRUE,
                  strata           = list(list("sex")))

# NOT modifyList(): it recurses into nested lists and merges them, so replacing
# strata with list("Age group") would silently keep list("Sex"). Whole-key
# replacement is what we want. (Same trap that load() avoids.)
with_params <- function(...) {
  p <- ok_params
  new <- list(...)
  p[names(new)] <- new
  p
}
problems <- function(p) INC$validate(p, cohorts_idx)

test_that("validate: a well-formed incidence analysis has no problems",
          expect_length(problems(ok_params), 0))
test_that("validate: an outcome cohort cannot be the denominator",
          expect_true(any(grepl("denominator or target-denominator",
                                problems(with_params(denominatorTable = "Lactic acidosis"))))))
# The trap the migration exists to avoid: a target cohort is the thing a
# denominator is generated FROM, not a denominator itself.
test_that("validate: a target cohort cannot be the denominator either",
          expect_true(any(grepl("denominator or target-denominator",
                                problems(with_params(denominatorTable = "Metformin new users"))))))
# The mirror image of the denominator check: outcome and censoring must be
# PLAIN cohorts, never a generated denominator.
test_that("validate: an outcome pointing at a denominator is reported", {
  expect_true(any(grepl("Outcome must be a plain cohort",
                        problems(with_params(outcomeTable = "Metformin denominator")))))
})
test_that("validate: a censoring cohort pointing at a denominator is reported", {
  expect_true(any(grepl("Censoring must be a plain cohort",
                        problems(with_params(censorTable = "Men only")))))
  expect_false(any(grepl("Censoring must be",
                         problems(with_params(censorTable = "Lactic acidosis")))))
})

test_that("validate: an unset washout is reported",
          expect_true(any(grepl("stated explicitly",
                                problems(with_params(outcomeWashout = NULL,
                                                     repeatedEvents = FALSE))))))
test_that("validate: repeated events with an unbounded washout is reported",
          expect_true(any(grepl("finite outcome washout",
                                problems(with_params(outcomeWashout = parse_washout("Inf"),
                                                     repeatedEvents = TRUE))))))
# The unbounded washout as it actually comes back off disk: [null] -> list(NULL).
test_that("validate: repeated events is reported for an unbounded washout read from JSON",
          expect_true(any(grepl("finite outcome washout",
                                problems(with_params(outcomeWashout = list(NULL),
                                                     repeatedEvents = TRUE))))))
test_that("validate: a 0-day washout is a stated washout, not an unset one",
          expect_false(any(grepl("stated explicitly",
                                 problems(with_params(outcomeWashout = parse_washout("0"),
                                                      repeatedEvents = TRUE))))))
test_that("validate: an unset washout does not ALSO trip the repeated-events rule",
          expect_false(any(grepl("finite outcome washout",
                                 problems(with_params(outcomeWashout = NULL,
                                                      repeatedEvents = TRUE))))))

# Strata are columns on the denominator cohort table. Two distinct failures:
# a column the cohort does not carry (estimateIncidence would error), and a
# column it carries but has already collapsed (it would succeed, uselessly).
test_that("validate: cannot stratify by a column the denominator does not carry",
          expect_true(any(grepl("does not carry that column",
                                problems(with_params(strata = list(list("region"))))))))
test_that("validate: a crossed group checks every variable in it",
          expect_true(any(grepl("'region'",
                                problems(with_params(strata = list(list("sex", "region"))))))))
test_that("validate: cannot stratify by sex on a male-only denominator",
          expect_true(any(grepl("stratify by sex",
                                problems(with_params(denominatorTable = "Men only"))))))
test_that("validate: cannot stratify by age_group when the denominator has one age group",
          expect_true(any(grepl("stratify by age_group",
                                problems(with_params(denominatorTable = "Men only",
                                                     strata = list(list("age_group"))))))))
test_that("validate: an unstratified analysis raises no strata problems",
          expect_length(problems(with_params(strata = list())), 0))
test_that("validate: a cohort nobody defined does not error and is not called wrong-kind",
          expect_false(any(grepl("denominator or target-denominator",
                                 problems(with_params(denominatorTable = "Typed by hand"))))))
test_that("validate: templates with no validator report nothing",
          expect_length(ANALYSIS_TEMPLATES[["Other"]]$validate(list(), cohorts_idx), 0))
test_that("validate: prevalence strata are checked against its denominator",
          expect_true(any(grepl("does not carry that column",
                                ANALYSIS_TEMPLATES[["Prevalence"]]$validate(
                                  list(denominatorTable = "Metformin denominator",
                                       strata = list(list("region"))),
                                  cohorts_idx)))))
test_that("validate: prevalence strata the denominator carries raise no problems",
          expect_length(ANALYSIS_TEMPLATES[["Prevalence"]]$validate(
            list(denominatorTable = "Metformin denominator", strata = list(list("sex"))),
            cohorts_idx), 0))
# Prevalence now enforces the same cohort-kind discipline as Incidence.
test_that("validate: a prevalence denominator must be a denominator kind",
          expect_true(any(grepl("denominator or target-denominator",
                                ANALYSIS_TEMPLATES[["Prevalence"]]$validate(
                                  list(denominatorTable = "Lactic acidosis"),
                                  cohorts_idx)))))
test_that("validate: a prevalence outcome cannot be a generated denominator",
          expect_true(any(grepl("Outcome must be a plain cohort",
                                ANALYSIS_TEMPLATES[["Prevalence"]]$validate(
                                  list(denominatorTable = "Metformin denominator",
                                       outcomeTable = "Men only"),
                                  cohorts_idx)))))
