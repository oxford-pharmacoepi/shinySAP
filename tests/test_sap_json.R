#!/usr/bin/env Rscript
# Checks the JSON contract the app promises: list fields are always arrays
# (even with one entry), blank text is null, and a saved SAP reads back.

library(jsonlite)
source("R/utils.R")

failures <- 0
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n", sep = "")
  if (!isTRUE(ok)) failures <<- failures + 1
}

sap <- list(
  sap_schema_version = "0.1.0",
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
  cdm_changes = list(),
  cohorts = list(list(
    name = "Metformin new users",
    role = "Target",
    cohort_id = 1001,
    entry_events = as_array(split_lines("First metformin dispensation")),
    inclusion_criteria = as_array(character(0)),
    washout_days = NA
  )),
  analyses = list(list(
    name = "Incidence",
    time_at_risk = list(start_offset_days = 1, start_anchor = "cohort start")
  ))
)

txt <- as.character(sap_json(sap))
back <- fromJSON(txt, simplifyVector = FALSE)

check("blank text serialises to null", is.null(back$study$acronym))
check("single author stays an array", length(back$study$authors) == 1 && is.list(back$study$authors))
check("objectives split and trimmed", identical(unlist(back$study$objectives),
                                                c("Estimate incidence", "Characterise users")))
check("empty section is an array", grepl('"cdm_changes": \\[\\]', txt))
check("empty criteria list is an array", grepl('"inclusion_criteria": \\[\\]', txt))
check("NA numeric serialises to null", is.null(back$cohorts[[1]]$washout_days))
check("scalars are unboxed", identical(back$study$title, "Metformin and lactic acidosis"))
check("nested time_at_risk survives", back$analyses[[1]]$time_at_risk$start_offset_days == 1)

check("slugify", identical(slugify("Metformin & Lactic Acidosis!"), "metformin-lactic-acidosis"))
check("slugify empty falls back", identical(slugify(""), "sap"))
check("join_lines round-trips split_lines",
      identical(split_lines(join_lines(list("a", "b"))), c("a", "b")))

pf <- prefiller(list(name = "x", missing = NA))
check("prefiller returns value", identical(pf("name"), "x"))
check("prefiller default on NA", identical(pf("missing", "d"), "d"))
check("prefiller default on absent", identical(pf("nope", "d"), "d"))

tmp <- file.path(tempdir(), "sap-out")
path <- save_sap(sap, tmp)
check("save_sap writes a file", file.exists(path))
check("filename is slugged", grepl("^sap-metformin-and-lactic-acidosis-\\d{8}-\\d{6}\\.json$", basename(path)))
check("saved file reads back", identical(read_sap(path)$study$title, sap$study$title))
unlink(tmp, recursive = TRUE)

cat("\n", if (failures == 0) "All checks passed.\n" else sprintf("%d check(s) failed.\n", failures), sep = "")
quit(status = if (failures == 0) 0 else 1)
