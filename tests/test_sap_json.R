#!/usr/bin/env Rscript
# Checks the JSON contract the app promises: list fields are always arrays
# (even with one entry), blank text is null, a saved SAP reads back, and every
# analysis template round-trips through collect() -> JSON -> flatten().

library(jsonlite)
library(shiny)
library(bslib)

# utils.R and dynamic_items.R come first here, unlike under Shiny's loadSupport():
# the templates call %||% and entity_picker() when they run, and the tests run
# them. The templates are then globbed exactly as the app finds them, so a new
# analysis_type_*.R file is covered by the checks below without touching this
# file.
source("R/utils.R")
source("R/dynamic_items.R")
source("R/analysis_registry.R")
source("R/mod_cohorts.R")     # cohort kinds and cohort_by_name(), which validators use
for (f in sort(list.files("R", pattern = "^analysis_type_.*\\.R$", full.names = TRUE))) source(f)

failures <- 0
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n", sep = "")
  if (!isTRUE(ok)) failures <<- failures + 1
}

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

check("blank text serialises to null", is.null(back$study$acronym))
check("single author stays an array", length(back$study$authors) == 1 && is.list(back$study$authors))
check("objectives split and trimmed", identical(unlist(back$study$objectives),
                                                c("Estimate incidence", "Characterise users")))
check("empty section is an array", grepl('"cdm_changes": \\[\\]', txt))
check("empty criteria list is an array", grepl('"inclusion_criteria": \\[\\]', txt))
check("NA numeric serialises to null", is.null(back$cohorts[[1]]$washout_days))
check("scalars are unboxed", identical(back$study$title, "Metformin and lactic acidosis"))
check("nested time_at_risk survives",
      back$proposed_analyses[[1]]$parameters$time_at_risk$start_offset_days == 1)
check("cdm_sources serialises", identical(back$cdm_sources[[1]]$source_key, "cprd"))
check("single data_source stays an array",
      is.list(back$proposed_analyses[[1]]$data_sources) &&
        length(back$proposed_analyses[[1]]$data_sources) == 1)

# Section rename: 0.2.0 reads proposed_analyses, but must still load 0.1.0 files.
check("coalesce_key prefers the new name",
      identical(coalesce_key(list(proposed_analyses = list("new"), analyses = list("old")),
                             "proposed_analyses", "analyses"), list("new")))
check("coalesce_key falls back to the old name",
      identical(coalesce_key(list(analyses = list("old")), "proposed_analyses", "analyses"),
                list("old")))
check("coalesce_key on an empty new key falls back",
      identical(coalesce_key(list(proposed_analyses = list(), analyses = list("old")),
                             "proposed_analyses", "analyses"), list("old")))
check("coalesce_key with neither key gives an empty list",
      identical(coalesce_key(list(), "proposed_analyses", "analyses"), list()))

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

# Analysis type resolution ----------------------------------------------------
#
# analysis_template() is called on whatever a saved file happens to hold, and
# load() has already cleared the section by then -- so nothing here may error.

check("resolver: NULL falls back", identical(canonical_analysis_type(NULL), ANALYSIS_TYPES[1]))
check("resolver: NA falls back", identical(canonical_analysis_type(NA), ANALYSIS_TYPES[1]))
check("resolver: empty string falls back", identical(canonical_analysis_type(""), ANALYSIS_TYPES[1]))
check("resolver: a renamed type is aliased",
      identical(canonical_analysis_type("Incidence rate"), "Incidence"))
check("resolver: a known type is left alone",
      identical(canonical_analysis_type("Prevalence"), "Prevalence"))

check("template: NULL resolves without error",
      identical(analysis_template(NULL), ANALYSIS_TEMPLATES[["Other"]]))
check("template: a renamed type gets the Incidence template",
      identical(analysis_template("Incidence rate"), ANALYSIS_TEMPLATES[["Incidence"]]))
check("template: an unknown type falls back to Other",
      identical(analysis_template("Bayesian hierarchical model"), ANALYSIS_TEMPLATES[["Other"]]))
check("template: a type with no entry yet falls back to Other",
      identical(analysis_template("Case-control"), ANALYSIS_TEMPLATES[["Other"]]))

check("Other is always present, it is the fallback",
      "Other" %in% names(ANALYSIS_TEMPLATES))
check("every template key is an offered analysis type",
      all(names(ANALYSIS_TEMPLATES) %in% ANALYSIS_TYPES))

# Template input ids ----------------------------------------------------------

picker_ids <- function(tmpl) unlist(tmpl$pickers, use.names = FALSE) %||% character(0)
all_pickers <- unique(unlist(lapply(ANALYSIS_TEMPLATES, picker_ids)))

for (type in names(ANALYSIS_TEMPLATES)) {
  tmpl <- ANALYSIS_TEMPLATES[[type]]
  ids  <- template_field_ids(tmpl)     # also a smoke test that ui() runs at all
  pk   <- picker_ids(tmpl)

  check(sprintf("[%s] ui() renders at least one input", type), length(ids) > 0)
  check(sprintf("[%s] no id collides with a common field", type),
        length(intersect(ids, RESERVED_INPUT_IDS)) == 0)
  check(sprintf("[%s] every declared picker is actually rendered", type), all(pk %in% ids))
  # An id that sync_pickers() owns in one template must not be a plain input in
  # another: after a type switch the stale selectize string would be handed to
  # whatever widget the new template built for that id.
  check(sprintf("[%s] no id is a picker elsewhere but a plain input here", type),
        length(setdiff(intersect(ids, all_pickers), pk)) == 0)
}

# Template round-trips: collect() -> JSON -> flatten() -> prefiller() ----------

round_trip <- function(tmpl, input) {
  params <- tmpl$collect(input)
  rt     <- fromJSON(as.character(sap_json(params)), simplifyVector = FALSE)
  list(json = rt, flat = tmpl$flatten(rt), pf = prefiller(tmpl$flatten(rt)))
}

# THE MIRROR INVARIANT. Every input a template renders has to survive
# collect() -> JSON -> flatten() and be findable again by pf(), or it silently
# comes back blank when a saved SAP is loaded. This is what catches a collect()
# that nests a block into `estimand` or `reporting` and a flatten() that forgets
# to unpack it -- a bug no amount of reading the template will show you.
for (type in names(ANALYSIS_TEMPLATES)) {
  tmpl <- ANALYSIS_TEMPLATES[[type]]
  ids  <- setdiff(template_field_ids(tmpl), DISPLAY_ONLY_IDS)
  # A plausible non-empty value for every input; only presence is under test.
  faked <- stats::setNames(lapply(ids, function(i) "1"), ids)
  flat  <- round_trip(tmpl, faked)$flat
  missing <- setdiff(ids, names(flat))
  check(sprintf("[%s] every rendered input survives collect -> JSON -> flatten%s", type,
                if (length(missing)) paste0(" (lost: ", paste(missing, collapse = ", "), ")") else ""),
        length(missing) == 0)
}

inc <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(
  denominator_cohort          = "Metformin new users",
  outcome_cohort              = "Lactic acidosis",
  censor_cohort               = "",
  outcome_washout             = "365",
  repeated_events             = TRUE,
  interval                    = c("years", "overall"),
  complete_database_intervals = TRUE,
  strata                      = c("sex", "sex, age_group"),
  include_overall_strata      = TRUE
))
check("incidence: the estimand nests", identical(inc$json$estimand$outcome_washout, 365L))
check("incidence: a ticked checkbox is true, not null",
      identical(inc$json$estimand$repeated_events, TRUE))
check("incidence: flatten un-nests the estimand back onto the inputs",
      inc$pf("outcome_washout") == "365")
check("incidence: flatten leaves no nested blocks in the prefill",
      is.null(inc$flat$estimand))
check("incidence: a multi-select interval stays an array",
      identical(unlist(inc$json$estimand$interval), c("years", "overall")))
check("incidence: prefiller recovers a picker value",
      identical(inc$pf("denominator_cohort"), "Metformin new users"))

# `parameters` maps 1:1 onto estimateIncidence(). Anything the function does not
# take is not part of this analysis: rate-per-N and the denominator unit are
# presentation choices made downstream, and a sensitivity analysis is a second
# call, not an argument to this one.
inc_keys  <- names(inc$json)
inc_estim <- names(inc$json$estimand)
check("incidence: no reporting-only fields leak into parameters",
      !any(c("reporting", "denominator_unit", "rate_multiplier",
             "sensitivity_analyses", "stratifications", "time_at_risk") %in% inc_keys))
check("incidence: the estimand is exactly estimateIncidence()'s arguments",
      setequal(inc_estim, c("interval", "complete_database_intervals", "outcome_washout",
                            "repeated_events", "strata", "include_overall_strata")))
check("incidence: names exactly the three cohort tables the function takes",
      setequal(setdiff(inc_keys, "estimand"),
               c("denominator_cohort", "outcome_cohort", "censor_cohort")))

# strata is a list of variable GROUPS: list("sex", c("sex","age_group")) means one
# stratification by sex and another by the cross of the two. A comma in a token
# crosses its variables.
check("strata: each token is one group",  length(inc$json$estimand$strata) == 2)
check("strata: a plain token is a one-variable group",
      identical(unlist(inc$json$estimand$strata[[1]]), "sex"))
check("strata: a comma crosses the variables in one group",
      identical(unlist(inc$json$estimand$strata[[2]]), c("sex", "age_group")))
check("strata: a single group still serialises as a list of arrays",
      is.list(parse_strata("sex")[[1]]) || length(parse_strata("sex")[[1]]) == 1)
check("strata: tokens round-trip back into the multi-select",
      identical(inc$pf("strata"), c("sex", "sex, age_group")))
check("strata: no strata is an empty list, which is estimateIncidence()'s default",
      identical(parse_strata(character(0)), list()))

# The washout has three distinct states, and JSON has no Infinity: a bare Inf
# would serialise to null under na = "null" and become indistinguishable from
# "never stated", which is the one thing validate() must be able to tell apart.
# A 0-day washout is a different analysis from an unstated one. The select must
# therefore offer an empty choice, or the browser silently picks the first option
# and validate()'s "no safe default" rule can never fire.
check("washout: the select can be genuinely unset", "" %in% OUTCOME_WASHOUT_CHOICES)
check("washout: unset parses to NULL", is.null(parse_washout("")))
check("washout: a number parses to a number", identical(parse_washout("365"), 365))
check("washout: the unbounded sentinel parses to itself",
      identical(parse_washout(WASHOUT_UNBOUNDED), WASHOUT_UNBOUNDED))
check("washout: a legacy Inf parses to the sentinel",
      identical(parse_washout("Inf"), WASHOUT_UNBOUNDED))
check("washout: bare Inf would be lost by the JSON contract, the sentinel is not",
      is.null(fromJSON(as.character(sap_json(list(w = Inf))), simplifyVector = FALSE)$w) &&
        identical(fromJSON(as.character(sap_json(list(w = WASHOUT_UNBOUNDED))),
                           simplifyVector = FALSE)$w, WASHOUT_UNBOUNDED))
unb <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(outcome_washout = WASHOUT_UNBOUNDED))
check("washout: unbounded survives the full round trip",
      washout_is_unbounded(unb$json$estimand$outcome_washout))

prev <- round_trip(ANALYSIS_TEMPLATES[["Prevalence"]], list(
  denominator_cohort   = "Metformin new users",
  outcome_cohort       = "Lactic acidosis",
  prevalence_type      = "Point prevalence",
  interval_length_days = NA,
  full_contribution    = TRUE,
  time_points          = "2020-01-01",
  stratifications      = "",
  sensitivity_analyses = ""
))
check("prevalence: emits no time_at_risk", is.null(prev$json$time_at_risk))
check("prevalence: a cleared numeric is null", is.null(prev$json$interval_length_days))
check("prevalence: a ticked checkbox is true", identical(prev$json$full_contribution, TRUE))
check("prevalence: a single time point stays an array",
      is.list(prev$json$time_points) && length(prev$json$time_points) == 1)
check("prevalence: prefiller recovers the prevalence type",
      identical(prev$pf("prevalence_type"), "Point prevalence"))

# Cohort kinds -----------------------------------------------------------------
# 0.3.0 replaced `role` (Target / Comparator / ...) with `kind`.

check("cohort kind: an old role is aliased",
      identical(canonical_cohort_kind("Target"), "target_denominator"))
check("cohort kind: a current kind is left alone",
      identical(canonical_cohort_kind("denominator"), "denominator"))
check("cohort kind: NULL falls back", identical(canonical_cohort_kind(NULL), "denominator"))
check("cohort_by_name: a known cohort",
      identical(cohort_by_name(list(A = list(kind = "outcome")), "A")$kind, "outcome"))
check("cohort_by_name: a free-typed cohort nobody defined is NULL, not an error",
      is.null(cohort_by_name(list(A = list(kind = "outcome")), "Typed by hand")))
check("cohort_by_name: an empty pick is NULL",
      is.null(cohort_by_name(list(A = list()), "")))

# Template validators ----------------------------------------------------------

INC <- ANALYSIS_TEMPLATES[["Incidence"]]
cohorts_idx <- list(
  "Metformin new users" = list(kind = "target_denominator", sex = "Both",
                               age_groups = list("0-17", "18-64"),
                               strata_variables = list("age_group", "sex")),
  "Men only"            = list(kind = "denominator", sex = "Male",
                               age_groups = list("18-64"),
                               strata_variables = list("age_group", "sex")),
  "Lactic acidosis"     = list(kind = "outcome", sex = "Both")
)
ok_params <- list(denominator_cohort = "Metformin new users",
                  estimand = list(outcome_washout = 365, repeated_events = TRUE,
                                  strata = list(list("sex"))))

# NOT modifyList(): it recurses into nested lists and merges them, so replacing
# `stratifications` with list("Age group") would silently keep list("Sex").
# Whole-key replacement is what we want. (Same trap that load() avoids.)
with_params <- function(...) {
  p <- ok_params
  new <- list(...)
  p[names(new)] <- new
  p
}
problems <- function(p) INC$validate(p, cohorts_idx)

check("validate: a well-formed incidence analysis has no problems",
      length(problems(ok_params)) == 0)
check("validate: an outcome cohort cannot be the denominator",
      any(grepl("denominator or target-denominator",
                problems(with_params(denominator_cohort = "Lactic acidosis")))))
check("validate: an unset washout is reported",
      any(grepl("stated explicitly",
                problems(with_params(estimand = list(repeated_events = FALSE))))))
check("validate: repeated events with an unbounded washout is reported",
      any(grepl("finite outcome washout",
                problems(with_params(estimand = list(outcome_washout = WASHOUT_UNBOUNDED,
                                                     repeated_events = TRUE))))))
check("validate: an unset washout does not ALSO trip the repeated-events rule",
      !any(grepl("finite outcome washout",
                 problems(with_params(estimand = list(repeated_events = TRUE))))))

# Strata are columns on the denominator cohort table. Two distinct failures:
# a column the cohort does not carry (estimateIncidence would error), and a
# column it carries but has already collapsed (it would succeed, uselessly).
check("validate: cannot stratify by a column the denominator does not carry",
      any(grepl("does not carry that column",
                problems(with_params(estimand = list(outcome_washout = 365,
                                                     strata = list(list("region"))))))))
check("validate: a crossed group checks every variable in it",
      any(grepl("'region'",
                problems(with_params(estimand = list(outcome_washout = 365,
                                                     strata = list(list("sex", "region"))))))))
check("validate: cannot stratify by sex on a male-only denominator",
      any(grepl("stratify by sex", problems(with_params(denominator_cohort = "Men only")))))
check("validate: cannot stratify by age_group when the denominator has one age group",
      any(grepl("stratify by age_group",
                problems(with_params(denominator_cohort = "Men only",
                                     estimand = list(outcome_washout = 365,
                                                     strata = list(list("age_group"))))))))
check("validate: an unstratified analysis raises no strata problems",
      length(problems(with_params(estimand = list(outcome_washout = 365,
                                                  strata = list())))) == 0)
check("validate: a cohort nobody defined does not error and is not called wrong-kind",
      !any(grepl("denominator or target-denominator",
                 problems(with_params(denominator_cohort = "Typed by hand")))))
check("validate: templates with no validator report nothing",
      length(ANALYSIS_TEMPLATES[["Other"]]$validate(list(), cohorts_idx)) == 0)

# Loading a pre-0.3.0 analysis: flat top-level keys, the old type name, and the
# generic form's `target_cohort` where the template now wants a denominator.
legacy <- list(
  name = "Legacy incidence", analysis_type = "Incidence rate",
  target_cohort = "Metformin new users", outcome_cohort = "Lactic acidosis",
  time_at_risk = list(start_offset_days = 7, start_anchor = "cohort start"),
  stratifications = list("Sex")
)
legacy_tmpl <- analysis_template(legacy$analysis_type)
legacy_pf   <- prefiller(legacy_tmpl$flatten(legacy))   # no `parameters` -> read flat
check("legacy: the old type name resolves to the Incidence template",
      identical(legacy_tmpl, ANALYSIS_TEMPLATES[["Incidence"]]))
check("legacy: target_cohort migrates to the denominator",
      identical(legacy_pf("denominator_cohort"), "Metformin new users"))
check("legacy: outcome_cohort survives", identical(legacy_pf("outcome_cohort"), "Lactic acidosis"))
check("migration never overwrites a denominator the file already has",
      identical(
        prefiller(INC$flatten(
          list(denominator_cohort = "Real denominator", target_cohort = "Stale target")
        ))("denominator_cohort"),
        "Real denominator"))

# 0.3.0 moved time at risk from the analysis onto the cohort. A template cannot
# do that (it can only rewrite its own analysis), so migrate_sap() does it before
# any section loads.
old_sap <- list(
  cohorts = list(list(name = "Metformin new users", role = "Target"),
                 list(name = "Untouched", role = "Outcome")),
  proposed_analyses = list(list(
    name = "Legacy incidence", analysis_type = "Incidence rate",
    target_cohort = "Metformin new users",
    time_at_risk = list(start_offset_days = 7, start_anchor = "cohort start")
  ))
)
migrated <- migrate_sap(old_sap)
check("migrate_sap: time at risk lands on the denominator cohort",
      identical(migrated$cohorts[[1]]$time_at_risk$start_offset_days, 7))
check("migrate_sap: other cohorts are untouched",
      is.null(migrated$cohorts[[2]]$time_at_risk))
check("migrate_sap: the incidence template then drops the analysis-level copy",
      is.null(INC$flatten(migrated$proposed_analyses[[1]])$time_at_risk))
check("migrate_sap: never overwrites a time at risk the cohort already has",
      identical(
        migrate_sap(list(
          cohorts = list(list(name = "C", time_at_risk = list(start_offset_days = 99))),
          proposed_analyses = list(list(denominator_cohort = "C",
                                        time_at_risk = list(start_offset_days = 7)))
        ))$cohorts[[1]]$time_at_risk$start_offset_days, 99))
check("migrate_sap: an analysis naming an undefined cohort is a no-op, not an error",
      identical(migrate_sap(list(
        cohorts = list(list(name = "C")),
        proposed_analyses = list(list(denominator_cohort = "Nope",
                                      time_at_risk = list(start_offset_days = 7)))
      ))$cohorts[[1]]$time_at_risk, NULL))

# Nothing a template collects may collide with a common key, or c() in load()
# would silently prefer the common one.
for (type in names(ANALYSIS_TEMPLATES)) {
  keys <- names(ANALYSIS_TEMPLATES[[type]]$collect(list()))
  check(sprintf("[%s] no collected key collides with a common field", type),
        length(intersect(keys, ANALYSIS_COMMON_FIELDS)) == 0)
}

cat("\n", if (failures == 0) "All checks passed.\n" else sprintf("%d check(s) failed.\n", failures), sep = "")
quit(status = if (failures == 0) 0 else 1)
