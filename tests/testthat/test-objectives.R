# Objectives as referenceable things, and the coverage check that follows.
#
# The point of giving an objective an id is that position CANNOT be referenced:
# insert one at the top and every reference below it silently means something
# else. Most of these tests are that property from one side or the other.

test_that("migrate_objectives: bare strings gain ids in order, losslessly", {
  objs <- migrate_objectives(list("First objective", "Second objective"))
  expect_identical(vapply(objs, objective_id, character(1)), c("obj_1", "obj_2"))
  expect_identical(vapply(objs, objective_text, character(1)),
                   c("First objective", "Second objective"))
})

test_that("migrate_objectives leaves ids that already exist alone", {
  objs <- migrate_objectives(list(
    list(id = "obj_7", text = "Kept"), "Newly minted"))
  # obj_8, not obj_1: ids count past the highest in use so a gap can never be
  # refilled by a different objective.
  expect_identical(vapply(objs, objective_id, character(1)), c("obj_7", "obj_8"))
})

test_that("migrate_sap gives a study's objectives ids", {
  sap <- migrate_sap(list(study = list(objectives = list("A", "B"))))
  expect_identical(vapply(sap$study$objectives, objective_id, character(1)),
                   c("obj_1", "obj_2"))
})

test_that("objective_text and objective_id read both the old and new shapes", {
  expect_identical(objective_text("bare string"), "bare string")
  expect_true(is.na(objective_id("bare string")))
  expect_identical(objective_text(list(id = "obj_1", text = "typed")), "typed")
  expect_identical(objective_id(list(id = "obj_1", text = "typed")), "obj_1")
})

# Reconciliation ------------------------------------------------------------------
#
# The textarea holds text, the SAP holds ids. Everything below is about which
# edits keep an id and which mint a new one.

test_that("reordering objectives keeps their ids", {
  existing <- migrate_objectives(list("First", "Second", "Third"))
  out <- reconcile_objectives(c("Third", "First", "Second"), existing)
  expect_identical(vapply(out, objective_id, character(1)), c("obj_3", "obj_1", "obj_2"))
})

# The failure this design exists to prevent: with position as identity, an
# objective inserted at the top would repoint every reference below it.
test_that("inserting an objective does not repoint the ones after it", {
  existing <- migrate_objectives(list("First", "Second"))
  out <- reconcile_objectives(c("Brand new", "First", "Second"), existing)
  expect_identical(vapply(out, objective_text, character(1)),
                   c("Brand new", "First", "Second"))
  expect_identical(vapply(out, objective_id, character(1))[2:3], c("obj_1", "obj_2"))
  # The new one counts past the highest in use rather than stealing obj_1.
  expect_identical(objective_id(out[[1]]), "obj_3")
})

# A deleted objective's id must NOT be reissued: an analysis still referencing it
# would silently start pointing at a different objective. Gaps are the safe
# outcome -- the id is opaque, and the document numbers by position.
test_that("deleting an objective does not free its id for reuse", {
  existing <- migrate_objectives(list("First", "Second"))
  out <- reconcile_objectives(c("Second", "Third"), existing)
  expect_identical(vapply(out, objective_id, character(1)), c("obj_2", "obj_3"))
})

# Deliberate: a reworded objective may be a different objective, so the link
# breaks visibly rather than silently following the new wording.
test_that("rewording an objective mints a new id", {
  existing <- migrate_objectives(list("First"))
  out <- reconcile_objectives("First, but rewritten", existing)
  expect_false(identical(objective_id(out[[1]]), "obj_1"))
})

test_that("two objectives with identical text still get distinct ids", {
  out <- reconcile_objectives(c("Same", "Same"), list())
  expect_length(unique(vapply(out, objective_id, character(1))), 2)
})

test_that("blank lines are not objectives", {
  expect_length(reconcile_objectives(c("Real", "", "  "), list()), 1)
})

# Coverage ------------------------------------------------------------------------

sap_with <- function(n_obj, ...) {
  list(study = list(objectives = migrate_objectives(as.list(
    sprintf("Objective %d", seq_len(n_obj))))), analyses = list(...))
}

test_that("an objective no analysis answers is reported", {
  s <- sap_with(2)
  found <- objective_coverage_problems(
    s$study, list(list(name = "A", objectives = list("obj_1"))))
  msgs <- paste(unlist(lapply(found, function(f) f$messages)), collapse = " ")
  expect_match(msgs, "Objective 2", fixed = TRUE)
  expect_false(grepl("Objective 1", msgs, fixed = TRUE))
})

# Many-to-many: one objective answered by three analyses is the normal case
# (complete, 5-year and 2-year prevalence of the same disease), not a problem.
test_that("several analyses answering one objective is not a problem", {
  s <- sap_with(1)
  found <- objective_coverage_problems(s$study, list(
    list(name = "Complete", objectives = list("obj_1")),
    list(name = "5-year",   objectives = list("obj_1")),
    list(name = "2-year",   objectives = list("obj_1"))))
  expect_length(found, 0)
})

test_that("one analysis answering several objectives is not a problem", {
  s <- sap_with(2)
  expect_length(objective_coverage_problems(
    s$study, list(list(name = "Both", objectives = list("obj_1", "obj_2")))), 0)
})

test_that("an analysis answering no objective is reported", {
  s <- sap_with(1)
  found <- objective_coverage_problems(
    s$study, list(list(name = "Orphan", objectives = list("obj_1")),
                  list(name = "Stray")))
  msgs <- paste(unlist(lapply(found, function(f) f$messages)), collapse = " ")
  expect_match(msgs, "answers no objective", fixed = TRUE)
})

# The break a rewording causes has to surface, or a stale link persists quietly.
test_that("an analysis naming an objective that no longer exists is reported", {
  s <- sap_with(1)
  found <- objective_coverage_problems(
    s$study, list(list(name = "Stale", objectives = list("obj_1", "obj_99"))))
  msgs <- paste(unlist(lapply(found, function(f) f$messages)), collapse = " ")
  expect_match(msgs, "no longer exists", fixed = TRUE)
  expect_match(msgs, "obj_99", fixed = TRUE)
})

test_that("a SAP with neither objectives nor analyses has nothing to report", {
  expect_length(objective_coverage_problems(list(), list()), 0)
})

# Study export ---------------------------------------------------------------------

test_that("the generated study code names the objective an estimate answers", {
  sap <- list(
    study = list(objectives = migrate_objectives(list("First", "Second"))),
    cohorts = list(),
    proposed_analyses = list(list(
      name = "PP", analysis_type = "estimatePointPrevalence",
      objectives = list("obj_2"),
      parameters = list(denominatorTable = "D", outcomeTable = "O"))))
  analyses <- study_files(sap)[["analyses/incidencePrevalence.R"]]
  expect_match(analyses, "[objective 2]", fixed = TRUE)
})

test_that("an analysis naming no objective gets no label rather than a blank one", {
  sap <- list(
    study = list(objectives = migrate_objectives(list("First"))),
    proposed_analyses = list(list(
      name = "PP", analysis_type = "estimatePointPrevalence",
      parameters = list(denominatorTable = "D", outcomeTable = "O"))))
  expect_false(grepl("[objective", study_files(sap)[["analyses/incidencePrevalence.R"]],
                     fixed = TRUE))
})

# The card ---------------------------------------------------------------------------

test_that("the study card keeps objective ids across an edit to another line", {
  testServer(study_server, {
    session$setInputs(objectives = "First\nSecond")
    ids <- vapply(isolate(objectives()), objective_id, character(1))
    session$setInputs(objectives = "First\nSecond\nThird")
    after <- vapply(isolate(objectives()), objective_id, character(1))
    expect_identical(after[1:2], ids)
  })
})

test_that("an analysis card collects the objectives it answers", {
  testServer(analyses_server, {
    session$setInputs(add = 1)
    session$setInputs("analysis_1-name" = "PP",
                      "analysis_1-objectives" = c("obj_1", "obj_2"))
    expect_identical(as.character(isolate(items$data())[[1]]$data$objectives),
                     c("obj_1", "obj_2"))
  })
})

# The picker is only useful if the Study tab's objectives actually reach it.
# analyses_server() defaults objective_choices to an empty reactive, so a missing
# argument at the call site renders a dropdown with nothing in it -- which is
# exactly what happened, and looks like the feature simply not working.
test_that("objective choices reach the analysis card's picker", {
  choices <- reactive(stats::setNames(c("obj_1", "obj_2"),
                                      c("1. First objective", "2. Second objective")))
  testServer(analyses_server, args = list(objective_choices = choices), {
    session$setInputs(add = 1)
    session$flushReact()
    # The card can only select an id the picker was given.
    session$setInputs("analysis_1-objectives" = "obj_2")
    expect_identical(as.character(isolate(items$data())[[1]]$data$objectives), "obj_2")
  })
})

test_that("app.R hands analyses_server its objective choices", {
  app <- readLines(testthat::test_path("..", "..", "app.R"))
  call <- paste(app, collapse = "\n")
  expect_match(call, "objective_choices = objective_choices", fixed = TRUE)
})
