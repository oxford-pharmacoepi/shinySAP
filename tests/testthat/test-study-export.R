# The SAP rendered into the empty files OmopStudyBuilder::initStudy() leaves
# behind. The tests that matter are the ones about the SPLIT: what the harness
# owns, this must not write; what the harness already does, this must not repeat.

sample_sap <- function(...) {
  base <- list(
    study = list(study_code = "C1-001", version = "1.0", min_cell_count = 5),
    sap_schema_version = "0.4.21",
    codelists = list(list(name = "cs_fl", category = "Index event")),
    cohorts = list(
      list(name = "FL", kind = "target", operations = list(
        list(op = "concept_cohort", codelist = "cs_fl"),
        list(op = "require_first_entry"))),
      list(name = "General population", kind = "denominator", sex = list("Both"),
           ageGroup = list(c(0, 150)), daysPriorObservation = list(0))),
    proposed_analyses = list(list(
      name = "Point prevalence", analysis_type = "estimatePointPrevalence",
      parameters = list(denominatorTable = "General population", outcomeTable = "FL"))))
  utils::modifyList(base, list(...))
}

test_that("the export writes the files initStudy leaves empty, and only those", {
  files <- study_files(sample_sap())
  expect_true(all(c("codelist/codelistCreation.R", "cohorts/instantiateCohorts.R",
                    "analyses/incidencePrevalence.R") %in% names(files)))
  # codeToRun.R and runStudy.R carry database credentials and the harness's own
  # ordering; an export has no business rewriting either.
  expect_false(any(c("codeToRun.R", "runStudy.R") %in% names(files)))
})

# codeToRun.R already loads every package, so a second set of calls is noise --
# and is the sort of drift that makes a generated file annoying to keep.
test_that("no library() calls: the harness's codeToRun.R already loads them", {
  for (f in study_files(sample_sap())) {
    expect_false(grepl("library(", f, fixed = TRUE))
  }
})

# runStudy.R finishes with exportSummarisedResult(minCellCount = min_cell_count),
# so generating a second suppression step would apply the threshold twice.
test_that("no suppression: runStudy.R applies the threshold on export", {
  files <- study_files(sample_sap())
  expect_false(any(vapply(files, function(f) grepl("suppress", f, fixed = TRUE), logical(1))))
  # It is reported instead, so the plan's threshold and the study's cannot differ
  # silently just because this file may not be written.
  expect_match(study_export_notes(sample_sap()), "min_cell_count <- 5", fixed = TRUE)
})

test_that("a SAP with no stated threshold says so rather than assuming one", {
  sap <- sample_sap()
  sap$study$min_cell_count <- NULL
  expect_match(study_export_notes(sap), "states no minimum cell count", fixed = TRUE)
})

# `results` is what runStudy.R binds and exports, so an estimate assigned to a
# free variable would simply never leave the data partner.
test_that("estimates accumulate into the results list runStudy.R exports", {
  analyses <- study_files(sample_sap())[["analyses/incidencePrevalence.R"]]
  expect_match(analyses, 'results[["point_prevalence_1"]] <- estimatePointPrevalence(',
               fixed = TRUE)
  # The denominator still goes back into cdm -- a different return type, so a
  # different idiom (see sap_code.R).
  expect_match(analyses, "cdm <- generateDenominatorCohortSet(", fixed = TRUE)
})

test_that("cohorts are rendered from the same generator the appendix uses", {
  cohorts <- study_files(sample_sap())[["cohorts/instantiateCohorts.R"]]
  expect_match(cohorts, "cdm$fl <- conceptCohort(", fixed = TRUE)
  expect_match(cohorts, "requireIsFirstEntry()", fixed = TRUE)
})

# Codelists ---------------------------------------------------------------------

# The SAP names its codelists but does not carry their codes, so the export
# writes no CSVs -- the study team places them at the paths the script reads.
test_that("the export writes no codelist CSVs", {
  expect_false(any(grepl("[.]csv$", names(study_files(sample_sap())))))
})

# omopgenerics' own reader, not a hand-rolled read.csv(): it keys the codelist by
# the file name, which is the codelist's name, so conceptCohort(conceptSet =)
# takes the result directly. It is also what a real DARWIN study writes. The
# path follows the layout DARWIN studies use: one folder per category.
test_that("codelistCreation.R expects each CSV under its category folder", {
  files <- study_files(sample_sap())
  expect_match(files[["codelist/codelistCreation.R"]],
               'cs_fl <- importCodelist(path = here("codelist/Index_event/cs_fl.csv"), type = "csv")',
               fixed = TRUE)
  # Said out loud, not implied by a failing read: the CSV is the team's to supply.
  expect_match(files[["codelist/codelistCreation.R"]], "# TODO", fixed = TRUE)
  expect_false(grepl("read.csv", files[["codelist/codelistCreation.R"]], fixed = TRUE))
})

test_that("an uncategorised codelist is expected under Other, as in the document", {
  sap <- sample_sap()
  sap$codelists[[1]]$category <- NULL
  expect_match(study_files(sap)[["codelist/codelistCreation.R"]],
               'here("codelist/Other/cs_fl.csv")', fixed = TRUE)
})

# An index changes no result, so it belongs in the code that runs and not in the
# plan a reviewer signs -- which is why the document's appendix has none.
test_that("every generated cohort gets a table index in the study code, not the plan", {
  cohorts <- study_files(sample_sap())[["cohorts/instantiateCohorts.R"]]
  expect_match(cohorts, "cdm$fl <- cdm$fl |> addCohortTableIndex()", fixed = TRUE)
  expect_false(grepl("addCohortTableIndex",
                     sap_r_script(sample_sap()), fixed = TRUE))
})

# subsetCohort builds a cohort only among people already in another one, which is
# what every outcome in a real exposure study does.
test_that("an entry can be restricted to another cohort's members", {
  sap <- sample_sap()
  sap$cohorts[[1]]$operations[[1]]$subset_cohort <- "General population"
  cohorts <- study_files(sap)[["cohorts/instantiateCohorts.R"]]
  expect_match(cohorts, 'subsetCohort = "general_population"', fixed = TRUE)
})

test_that("restricting to a cohort the SAP does not define is a problem", {
  ops <- list(list(op = "concept_cohort", codelist = "cs_fl",
                   subset_cohort = "Nowhere"))
  errs <- cohort_operations_problems(
    list(name = "FL", kind = "target", operations = ops),
    codelist_names = "cs_fl", cohort_names = c("FL", "General population"))
  expect_match(paste(errs, collapse = " "), "which this SAP does not define", fixed = TRUE)
})

test_that("the restriction is stated in the generated prose too", {
  line <- cohort_operations_prose(list(operations = list(
    list(op = "concept_cohort", codelist = "cs_fl", subset_cohort = "Vaccinated"))))[[1]]
  expect_match(line, "among people in the Vaccinated cohort", fixed = TRUE)
})

# A codelist cited only in prose is documentation, not an input -- the same rule
# the standalone script applies.
test_that("only codelists a typed entry cites reach codelistCreation.R", {
  sap <- sample_sap()
  sap$codelists[[2]] <- list(name = "cs_unused", category = "Covariate")
  expect_false(grepl("cs_unused",
                     study_files(sap)[["codelist/codelistCreation.R"]], fixed = TRUE))
})

# Empty SAPs ---------------------------------------------------------------------

# An empty file would read as "nothing to do here"; a stated reason reads as the
# gap it is, which is the rule the document follows too.
test_that("nothing to generate produces a stated reason, not an empty file", {
  files <- study_files(list())
  expect_match(files[["cohorts/instantiateCohorts.R"]], "No cohorts are generated yet",
               fixed = TRUE)
  expect_match(files[["codelist/codelistCreation.R"]], "No concept sets", fixed = TRUE)
  expect_match(files[["analyses/incidencePrevalence.R"]], "No denominator cohort sets",
               fixed = TRUE)
})

test_that("every generated R file is syntactically valid R", {
  for (nm in names(study_files(sample_sap()))) {
    if (!grepl("[.]R$", nm)) next
    expect_silent(parse(text = study_files(sample_sap())[[nm]]))
  }
})

test_that("each file names the SAP it was generated from", {
  for (f in study_files(sample_sap())) {
    if (!grepl("Generated by shinySAP", f, fixed = TRUE)) next
    expect_match(f, "C1-001", fixed = TRUE)
    expect_match(f, "SAP schema 0.4.21", fixed = TRUE)
  }
})

# The plan fields carried into the result ------------------------------------------
#
# name, role and objectives are not arguments to any estimator -- none of them is
# a computational choice -- so before 0.4.27 they reached the script only as a
# comment. The exported results are what a study report is written from, and a
# results file that cannot say which of nine prevalence estimates is the primary
# one just moves "it is only in prose" one step downstream.
#
# The mechanism is verified against omopgenerics 1.4.1 and IncidencePrevalence
# 1.2.1: extra settings columns survive bind(), suppress() and the export/import
# round trip, and the C1-001 estimates block as generated runs and comes back
# carrying its role.

tagged_sap <- function() {
  s <- sample_sap()
  s$study$objectives <- list(list(id = "obj_1", text = "Estimate prevalence"))
  s$proposed_analyses <- list(list(
    name = "Point prevalence", analysis_type = "estimatePointPrevalence",
    role = "primary", objectives = list("obj_1"),
    parameters = list(denominatorTable = "General population", outcomeTable = "FL")))
  s
}

test_that("an estimate is labelled with the plan fields no estimator takes", {
  code <- study_files(tagged_sap())[["analyses/incidencePrevalence.R"]]
  expect_match(code, "sapTag <- function(result, analysis", fixed = TRUE)
  expect_match(code, 'analysis = "Point prevalence"', fixed = TRUE)
  expect_match(code, 'role = "primary"', fixed = TRUE)
  # Numbered as the document numbers them, from the same helper the comment uses.
  expect_match(code, 'objectives = "1"', fixed = TRUE)
  expect_match(code, "omopgenerics::newSummarisedResult(result, settings = s)", fixed = TRUE)
})

# The helper is one function, not one per estimate.
test_that("the tag helper is defined exactly once", {
  s <- tagged_sap()
  s$proposed_analyses <- rep(s$proposed_analyses, 4)
  code <- study_files(s)[["analyses/incidencePrevalence.R"]]
  expect_length(gregexpr("sapTag <- function", code, fixed = TRUE)[[1]], 1)
})

# An unstated role must not become a claim. The helper's own NA default stands
# for "the plan never said", so the argument is simply not passed.
test_that("an analysis with no role passes no role", {
  s <- tagged_sap()
  s$proposed_analyses[[1]]$role <- NULL
  code <- study_files(s)[["analyses/incidencePrevalence.R"]]
  # The CALL, not the file: the helper's own signature carries `role =` as its
  # NA default, which is exactly what an unstated role should fall back to.
  call <- regmatches(code, regexpr("<- sapTag\\((?s).*?\\n\\)", code, perl = TRUE))
  expect_length(call, 1)
  expect_match(call, 'analysis = "Point prevalence"', fixed = TRUE)
  expect_false(grepl("role =", call, fixed = TRUE))
})

# Data sources as a guard ----------------------------------------------------------
#
# `data_sources` is WHICH databases an item runs against -- the SAP-level
# counterpart of `cdm`. It reached the document and stopped there, so a plan
# restricting an analysis to two of five databases generated code that ran it at
# all five: the plan said one thing and the code exported another.

# Assigned, NOT passed through sample_sap(): modifyList() recurses into nested
# lists, and two UNNAMED lists merge to the original -- so a `cohorts =` argument
# is silently ignored and the fixture keeps the base cohorts. (The same trap
# test-validators.R calls out.)
restricted_sap <- function() {
  s <- sample_sap()
  s$cdm_sources <- list(list(name = "S", source_key = "SIDIAP"),
                        list(name = "C", source_key = "CPRD GOLD"),
                        list(name = "I", source_key = "IPCI"))
  s$cohorts <- list(
    list(name = "FL", kind = "target",
         data_sources = list("SIDIAP", "CPRD GOLD"),   # not IPCI
         operations = list(list(op = "concept_cohort", codelist = "cs_fl"),
                           list(op = "require_first_entry"))),
    list(name = "General population", kind = "denominator",
         data_sources = list("SIDIAP", "CPRD GOLD", "IPCI"),
         sex = list("Both"), ageGroup = list(c(0, 150)),
         daysPriorObservation = list(0)))
  s$proposed_analyses <- list(list(
    name = "Point prevalence", analysis_type = "estimatePointPrevalence",
    data_sources = list("SIDIAP"),
    parameters = list(denominatorTable = "General population", outcomeTable = "FL")))
  s
}

test_that("a restricted cohort is guarded by the database it names", {
  code <- study_files(restricted_sap())[["cohorts/instantiateCohorts.R"]]
  expect_match(code, 'omopgenerics::cdmName(cdm) %in% c("SIDIAP", "CPRD GOLD")',
               fixed = TRUE)
  # The index belongs INSIDE the guard: at IPCI the table is never created, and
  # indexing a table that does not exist is an error, not a no-op.
  guard_body <- sub("(?s).*cdmName\\(cdm\\)[^\n]*\\{(.*?)\n\\}.*", "\\1", code, perl = TRUE)
  expect_match(guard_body, "addCohortTableIndex", fixed = TRUE)
})

test_that("an unrestricted item is emitted bare, with no always-true guard", {
  code <- study_files(sample_sap())[["analyses/incidencePrevalence.R"]]
  expect_false(grepl("cdmName", code, fixed = TRUE))
})

# The estimates are bound together at the end, so a guarded estimate must still
# DEFINE its variable everywhere -- a bare `if` would leave it missing and take
# bind() down with "object not found" at every excluded partner.
test_that("a restricted estimate keeps its variable defined everywhere", {
  code <- study_files(restricted_sap())[["analyses/incidencePrevalence.R"]]
  expect_match(code, '<- if (omopgenerics::cdmName(cdm) %in% c("SIDIAP"))', fixed = TRUE)
  expect_match(code, "omopgenerics::emptySummarisedResult()", fixed = TRUE)
})

test_that("guarded code is still syntactically valid R", {
  for (nm in names(study_files(restricted_sap()))) {
    if (!grepl("[.]R$", nm)) next
    expect_silent(parse(text = study_files(restricted_sap())[[nm]]))
  }
})

# The guard compares the SAP's source keys against whatever codeToRun.R passed as
# cdmName. Nothing in the generated code can check that, so it is reported.
test_that("a plan with guards says the source keys have to match codeToRun.R", {
  expect_match(study_export_notes(restricted_sap()), "cdmName", fixed = TRUE)
  expect_false(grepl("cdmName", study_export_notes(sample_sap()), fixed = TRUE))
})

# Writing --------------------------------------------------------------------------

test_that("write_study_files creates the tree and leaves everything else alone", {
  dir  <- withr::local_tempdir()
  # A file the harness owns, which the export must not touch.
  writeLines("min_cell_count <- 99", file.path(dir, "codeToRun.R"))
  paths <- write_study_files(sample_sap(), dir)

  expect_true(all(file.exists(paths)))
  expect_true(file.exists(file.path(dir, "codelist/codelistCreation.R")))
  expect_identical(readLines(file.path(dir, "codeToRun.R")), "min_cell_count <- 99")
})
