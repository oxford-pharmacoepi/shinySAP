# The JSON contract the app promises: list fields are always arrays (even with
# one entry), blank text is null, and a saved SAP reads back.

sap <- list(
  sap_schema_version = "0.3.0",
  generated_at = "2026-07-09T00:00:00+0000",
  study = list(
    title = "Metformin and lactic acidosis",
    study_code = blank_to_na("   "),
    authors = as_array("A. Researcher"),          # single author
    version = "1.0",
    date = "2026-07-09",
    background = blank_to_na(""),
    objectives = as_array(split_lines("Estimate incidence\n\n  Characterise users  ")),
    amendments = list()
  ),
  cdm_sources = list(list(
    name = "CPRD GOLD",
    source_key = "cprd",
    country = blank_to_na("")
  )),
  cdm_changes = list(),
  cohorts = list(list(
    name = "Metformin new users",
    kind = "target_denominator",
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

test_that("blank text serialises to null", expect_null(back$study$study_code))
test_that("single author stays an array", {
  expect_true(is.list(back$study$authors))
  expect_length(back$study$authors, 1)
})
test_that("objectives split and trimmed",
          expect_identical(unlist(back$study$objectives),
                           c("Estimate incidence", "Characterise users")))
test_that("empty section is an array", expect_match(txt, '"cdm_changes": \\[\\]'))
test_that("empty amendment history is an array", expect_match(txt, '"amendments": \\[\\]'))
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

test_that("slugify",
          expect_identical(slugify("Metformin & Lactic Acidosis!"), "metformin-lactic-acidosis"))
test_that("slugify empty stays empty", expect_identical(slugify(""), ""))

# date_input(): the placeholder says what typing into the field looks like; the
# empty data-initial-date is what keeps a blank field from silently becoming
# today (blank is meaningful -- see the comment on date_input()).
test_that("date_input: a blank field shows the format and stays blank", {
  html <- as.character(date_input("d", "Date"))
  expect_match(html, 'placeholder="YYYY-MM-DD"', fixed = TRUE)
  expect_match(html, 'data-initial-date=""', fixed = TRUE)
})
test_that("date_input: a prefilled field keeps its date and the format placeholder", {
  html <- as.character(date_input("d", "Date", "2024-01-31"))
  expect_match(html, 'placeholder="YYYY-MM-DD"', fixed = TRUE)
  expect_match(html, 'data-initial-date="2024-01-31"', fixed = TRUE)
})

test_that("sap_file_base prefers the study code and carries the version",
          expect_identical(
            sap_file_base(list(title = "Metformin and lactic acidosis",
                               study_code = "MELA", version = "1.0")),
            "sap-mela-v1.0"))
test_that("sap_file_base falls back to the title",
          expect_identical(sap_file_base(list(title = "Metformin study", study_code = NA)),
                           "sap-metformin-study"))
test_that("sap_file_base never doubles the sap prefix", {
  expect_identical(sap_file_base(list(title = "SAP")), "sap-untitled")
  expect_identical(sap_file_base(list(title = "SAP metformin study")),
                   "sap-metformin-study")
})
test_that("sap_file_base with no title is untitled",
          expect_identical(sap_file_base(list(title = NA)), "sap-untitled"))

test_that("next_sap_version bumps the major", {
  expect_identical(next_sap_version("1"), "2")
  expect_identical(next_sap_version("1.0"), "2.0")
  expect_identical(next_sap_version("1.2"), "2.0")
})
test_that("next_sap_version gives no prefill for a non-numeric version", {
  expect_identical(next_sap_version("draft"), "")
  expect_identical(next_sap_version(NA), "")
  expect_identical(next_sap_version(NULL), "")
})
test_that("join_lines round-trips split_lines",
          expect_identical(split_lines(join_lines(list("a", "b"))), c("a", "b")))

test_that("prefiller returns value, defaults on NA, defaults on absent", {
  pf <- prefiller(list(name = "x", missing = NA))
  expect_identical(pf("name"), "x")
  expect_identical(pf("missing", "d"), "d")
  expect_identical(pf("nope", "d"), "d")
})

test_that("working_sap_path: the study code outranks the title", {
  study <- list(title = "A long descriptive study title",
                study_code = "P4-C1-016", version = "1.0")
  expect_identical(basename(working_sap_path(study)), "sap-p4-c1-016-v1.0.json")
  # No code -> the title carries the name.
  study$study_code <- NA
  expect_identical(basename(working_sap_path(study)),
                   "sap-a-long-descriptive-study-title-v1.0.json")
})

# A SAP lives in ONE file: a stable slugged name with NO timestamp, created on
# the first write and rewritten in place by every write after it -- clicked
# Save and autosave alike.
test_that("write_sap rewrites one stable working file that reads back", {
  tmp <- file.path(tempdir(), "sap-out")
  on.exit(unlink(tmp, recursive = TRUE))
  path <- working_sap_path(sap$study, tmp)
  expect_match(basename(path), "^sap-metformin-and-lactic-acidosis-v1\\.0\\.json$")
  p1 <- write_sap(sap, path)
  p2 <- write_sap(sap, path)
  expect_identical(p1, p2)
  expect_length(list.files(tmp), 1)
  expect_identical(read_sap(p1)$study$title, sap$study$title)
})

# The app as it starts must not autosave: version and date carry defaults the
# author never typed, so they alone are not content.
test_that("sap_is_empty: a fresh app is empty, anything authored is not", {
  fresh <- list(study = list(title = NA, study_code = NA, authors = character(0),
                             version = "1.0", date = "2026-07-17",
                             background = NA, aim = NA, objectives = character(0),
                             amendments = list()),
                cdm_sources = list(), cdm_changes = list(), codelists = list(),
                cohorts = list(), proposed_analyses = list())
  expect_true(sap_is_empty(fresh))
  # The block the study card always writes, empty. Four nulls are not content.
  blank_protocol <- fresh
  blank_protocol$study$protocol <- list(reference = NA, version = NA,
                                        date = NA, url = NA)
  expect_true(sap_is_empty(blank_protocol))
  named_protocol <- blank_protocol
  named_protocol$study$protocol$reference <- "DARWIN EU® Study Protocol C1-001"
  expect_false(sap_is_empty(named_protocol))
  titled <- fresh
  titled$study$title <- "My study"
  expect_false(sap_is_empty(titled))
  with_cohort <- fresh
  with_cohort$cohorts <- list(list(name = "D"))
  expect_false(sap_is_empty(with_cohort))
})
