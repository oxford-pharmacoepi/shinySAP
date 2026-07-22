# The SAP rendered into the empty files OmopStudyBuilder::initStudy() leaves
# behind. The tests that matter are the ones about the SPLIT: what the harness
# owns, this must not write; what the harness already does, this must not repeat.

sample_sap <- function(...) {
  base <- list(
    study = list(study_code = "C1-001", version = "1.0", min_cell_count = 5),
    sap_schema_version = "0.4.21",
    codelists = list(list(
      name = "cs_fl", category = "Condition / observation",
      concept_set_expression = list(
        list(concept_id = "111", excluded = FALSE, descendants = FALSE, mapped = FALSE)),
      codes = list(list(code = "111", name = "Follicular lymphoma, grade 1")))),
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

# The layout DARWIN studies already use: one folder per category, one CSV per
# codelist, so a study directory looks like the ones a team already reads.
test_that("a codelist becomes a CSV under its category folder", {
  files <- study_files(sample_sap())
  expect_true("codelist/Condition_observation/cs_fl.csv" %in% names(files))
  expect_match(files[["codelist/Condition_observation/cs_fl.csv"]],
               "concept_id,concept_name", fixed = TRUE)
})

test_that("an uncategorised codelist falls into Other, as in the document", {
  sap <- sample_sap()
  sap$codelists[[1]]$category <- NULL
  expect_true("codelist/Other/cs_fl.csv" %in% names(study_files(sap)))
})

# A concept name containing a comma would otherwise split into two columns.
test_that("concept names are quoted so commas survive the CSV", {
  csv <- study_files(sample_sap())[["codelist/Condition_observation/cs_fl.csv"]]
  expect_match(csv, '"Follicular lymphoma, grade 1"', fixed = TRUE)
})

# omopgenerics' own reader, not a hand-rolled read.csv(): it keys the codelist by
# the file name, which is the codelist's name, so conceptCohort(conceptSet =)
# takes the result directly. It is also what a real DARWIN study writes.
test_that("codelistCreation.R reads its CSVs with importCodelist", {
  files <- study_files(sample_sap())
  expect_match(files[["codelist/codelistCreation.R"]],
               'cs_fl <- importCodelist(path = here("codelist/Condition_observation/cs_fl.csv"), type = "csv")',
               fixed = TRUE)
  expect_false(grepl("read.csv", files[["codelist/codelistCreation.R"]], fixed = TRUE))
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
test_that("only codelists a typed entry cites get a CSV", {
  sap <- sample_sap()
  sap$codelists[[2]] <- list(name = "cs_unused", category = "Outcome",
                             codes = list(list(code = "222")))
  files <- study_files(sap)
  expect_false(any(grepl("cs_unused", names(files), fixed = TRUE)))
})

# An expanding expression's CSV holds the concepts it NAMES, not the descendants
# it also covers, so the file must say so rather than look complete.
test_that("an expanding concept set carries its warning into the study code", {
  sap <- sample_sap()
  sap$codelists[[1]]$concept_set_expression <- list(
    list(concept_id = "111", excluded = FALSE, descendants = TRUE, mapped = FALSE))
  expect_match(study_files(sap)[["codelist/codelistCreation.R"]], "# TODO", fixed = TRUE)
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

# Writing --------------------------------------------------------------------------

test_that("write_study_files creates the tree and leaves everything else alone", {
  dir  <- withr::local_tempdir()
  # A file the harness owns, which the export must not touch.
  writeLines("min_cell_count <- 99", file.path(dir, "codeToRun.R"))
  paths <- write_study_files(sample_sap(), dir)

  expect_true(all(file.exists(paths)))
  expect_true(file.exists(file.path(dir, "codelist/Condition_observation/cs_fl.csv")))
  expect_identical(readLines(file.path(dir, "codeToRun.R")), "min_cell_count <- 99")
})
