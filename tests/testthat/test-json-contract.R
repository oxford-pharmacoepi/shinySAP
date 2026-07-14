# The JSON contract the app promises: list fields are always arrays (even with
# one entry), blank text is null, and a saved SAP reads back.

sap <- list(
  sap_schema_version = "0.3.0",
  generated_at = "2026-07-09T00:00:00+0000",
  study = list(
    title = "Metformin and lactic acidosis",
    acronym = blank_to_na("   "),
    authors = as_array("A. Researcher"),          # single author
    version = "1.0",
    date = "2026-07-09",
    background = blank_to_na(""),
    objectives = as_array(split_lines("Estimate incidence\n\n  Characterise users  "))
  ),
  cdm_sources = list(list(
    name = "CPRD GOLD",
    source_key = "cprd",
    data_type = "Primary care records",
    population_size = 12000000,
    cdm_version = "5.4",
    country = blank_to_na("")
  )),
  cdm_changes = list(),
  cohorts = list(list(
    name = "Metformin new users",
    kind = "target_denominator",
    cohort_id = 1001,
    entry_events = as_array(split_lines("First metformin dispensation")),
    inclusion_criteria = as_array(character(0)),
    washout_days = NA
  )),
  # No analysis_type: an analysis can be saved without one, and it is exactly the
  # input that breaks a resolver that indexes the registry directly.
  proposed_analyses = list(list(
    name = "Incidence",
    data_sources = as_array("CPRD GOLD"),
    parameters = list(
      outcome_cohort = "Lactic acidosis",
      time_at_risk = list(start_offset_days = 1, start_anchor = "cohort start")
    )
  ))
)

txt <- as.character(sap_json(sap))
back <- fromJSON(txt, simplifyVector = FALSE)

test_that("blank text serialises to null", expect_null(back$study$acronym))
test_that("single author stays an array", {
  expect_true(is.list(back$study$authors))
  expect_length(back$study$authors, 1)
})
test_that("objectives split and trimmed",
          expect_identical(unlist(back$study$objectives),
                           c("Estimate incidence", "Characterise users")))
test_that("empty section is an array", expect_match(txt, '"cdm_changes": \\[\\]'))
test_that("empty criteria list is an array", expect_match(txt, '"inclusion_criteria": \\[\\]'))
test_that("NA numeric serialises to null", expect_null(back$cohorts[[1]]$washout_days))
test_that("scalars are unboxed",
          expect_identical(back$study$title, "Metformin and lactic acidosis"))
test_that("nested time_at_risk survives",
          expect_equal(back$proposed_analyses[[1]]$parameters$time_at_risk$start_offset_days, 1))
test_that("cdm_sources serialises", expect_identical(back$cdm_sources[[1]]$source_key, "cprd"))
test_that("single data_source stays an array", {
  expect_true(is.list(back$proposed_analyses[[1]]$data_sources))
  expect_length(back$proposed_analyses[[1]]$data_sources, 1)
})

# Section rename: 0.2.0 reads proposed_analyses, but must still load 0.1.0 files.
test_that("coalesce_key prefers the new name",
          expect_identical(coalesce_key(list(proposed_analyses = list("new"), analyses = list("old")),
                                        "proposed_analyses", "analyses"), list("new")))
test_that("coalesce_key falls back to the old name",
          expect_identical(coalesce_key(list(analyses = list("old")), "proposed_analyses", "analyses"),
                           list("old")))
test_that("coalesce_key on an empty new key falls back",
          expect_identical(coalesce_key(list(proposed_analyses = list(), analyses = list("old")),
                                        "proposed_analyses", "analyses"), list("old")))
test_that("coalesce_key with neither key gives an empty list",
          expect_identical(coalesce_key(list(), "proposed_analyses", "analyses"), list()))

test_that("slugify",
          expect_identical(slugify("Metformin & Lactic Acidosis!"), "metformin-lactic-acidosis"))
test_that("slugify empty falls back", expect_identical(slugify(""), "sap"))
test_that("join_lines round-trips split_lines",
          expect_identical(split_lines(join_lines(list("a", "b"))), c("a", "b")))

test_that("prefiller returns value, defaults on NA, defaults on absent", {
  pf <- prefiller(list(name = "x", missing = NA))
  expect_identical(pf("name"), "x")
  expect_identical(pf("missing", "d"), "d")
  expect_identical(pf("nope", "d"), "d")
})

test_that("save_sap writes a slugged file that reads back", {
  tmp <- file.path(tempdir(), "sap-out")
  on.exit(unlink(tmp, recursive = TRUE))
  path <- save_sap(sap, tmp)
  expect_true(file.exists(path))
  expect_match(basename(path), "^sap-metformin-and-lactic-acidosis-\\d{8}-\\d{6}\\.json$")
  expect_identical(read_sap(path)$study$title, sap$study$title)
})
