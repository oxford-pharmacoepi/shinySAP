# Rename propagation. References between sections are BY NAME on purpose (the
# JSON mirrors the estimators' own arguments), so renaming a cohort must walk
# every picker still holding the old name -- or it dangles, live and in the
# saved file. Emit half: the card reports {old -> new}. Gate: cohorts_server
# drops the event if another card still holds the old name. Follow half:
# apply_rename_to_pickers() moves matching picker values.

skip_if_not(file.exists(file.path(app_root, "R", "mod_cohorts.R")),
            "source tree is not available")

test_that("rename: a unique rename emits one gated event", {
  testServer(cohorts_server, {
    session$setInputs(add = 1)
    session$setInputs("cohort_1-name" = "A", "cohort_1-kind" = "target")
    session$setInputs("cohort_1-name" = "B")
    ev <- isolate(rename_ev())
    expect_identical(ev$old, "A")
    expect_identical(ev$new, "B")
  })
})

test_that("rename: a duplicated old name emits nothing -- not this card's call", {
  testServer(cohorts_server, {
    session$setInputs(add = 1)
    session$setInputs("cohort_1-name" = "A", "cohort_1-kind" = "target")
    session$setInputs(add = 2)
    session$setInputs("cohort_2-name" = "A", "cohort_2-kind" = "outcome")
    session$setInputs("cohort_1-name" = "B")
    expect_null(isolate(rename_ev()))
  })
})

test_that("rename: clearing a name is not a rename, and re-linking survives it", {
  testServer(cohorts_server, {
    session$setInputs(add = 1)
    session$setInputs("cohort_1-name" = "A", "cohort_1-kind" = "target")
    session$setInputs("cohort_1-name" = "")
    expect_null(isolate(rename_ev()))
    # The previous name is kept across the blank, so the fresh name links back.
    session$setInputs("cohort_1-name" = "C")
    ev <- isolate(rename_ev())
    expect_identical(ev$old, "A")
    expect_identical(ev$new, "C")
  })
})

# The follow half resolves updateSelectizeInput lexically through the global
# environment (where helper-app.R sources the app code), so a recorder defined
# there intercepts the calls without a real client.
with_update_recorder <- function(code) {
  seen <- new.env(parent = emptyenv())
  assign("updateSelectizeInput", # nolint: object_name_linter. Shiny's own name.
         function(session, inputId, choices = NULL, selected = NULL, ...) { # nolint: object_name_linter.
           assign(inputId, list(choices = choices, selected = selected), envir = seen)
         },
         envir = globalenv())
  on.exit(rm("updateSelectizeInput", envir = globalenv()), add = TRUE)
  force(code)
  seen
}

test_that("apply_rename_to_pickers: matching values move, others are untouched", {
  seen <- with_update_recorder(
    apply_rename_to_pickers(NULL, list(denominatorTable = "A", outcomeTable = "X"),
                            c("denominatorTable", "outcomeTable", "absent"),
                            "A", "B", available = "X")
  )
  expect_identical(seen$denominatorTable$selected, "B")
  # The new name must be among the options, or selectize drops the selection.
  expect_true("B" %in% seen$denominatorTable$choices)
  expect_null(seen$outcomeTable)   # holds a different name: not updated
  expect_null(seen$absent)         # holds nothing: not updated
})

test_that("rename: a target denominator's picker follows the rename end to end", {
  seen <- with_update_recorder(
    testServer(cohorts_server, {
      session$setInputs(add = 1)
      session$setInputs("cohort_1-name" = "A", "cohort_1-kind" = "target")
      session$setInputs(add = 2)
      session$setInputs("cohort_2-name" = "TD", "cohort_2-kind" = "target_denominator")
      session$setInputs("cohort_2-targetCohortTable" = "A")
      session$setInputs("cohort_1-name" = "B")
    })
  )
  expect_identical(seen$targetCohortTable$selected, "B")
})
skip_if_not(dir.exists(file.path(app_root, "R")), "source tree is not available")
