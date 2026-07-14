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
source("R/cohort_kinds.R")    # cohort kind registry; the analysis validators read it
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
check("incidence: the estimand nests",
      identical(washout_days(inc$json$estimand$outcome_washout), 365))
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

# estimateIncidence(outcomeWashout =) is a NUMBER of days, defaulting to Inf. But
# JSON has no Infinity, and three states have to stay apart: unset, a number of
# days (including 0, a substantively different analysis from an unstated one), and
# Inf. So the washout is a one-element numeric array -- [365], [0], [null] for Inf
# -- while a bare null keeps its schema-wide meaning of "the author never said".
# The select must offer an empty choice, or the browser silently picks the first
# option and validate()'s "no safe default" rule can never fire.
check("washout: the select can be genuinely unset", "" %in% OUTCOME_WASHOUT_CHOICES)
check("washout: unset parses to NULL", is.null(parse_washout("")))
check("washout: a number of days parses to a numeric array",
      identical(as.numeric(parse_washout("365")), 365))
check("washout: 0 days is a value, not an absence",
      identical(washout_days(parse_washout("0")), 0))
check("washout: Inf parses to a numeric array holding Inf",
      is.infinite(washout_days(parse_washout("Inf"))))
check("washout: junk is not guessed at", is.null(parse_washout("about a year")))
check("washout: a negative washout is rejected", is.null(parse_washout("-30")))

# The encoding, end to end. A bare Inf really would be lost -- that is why the
# value is wrapped in an array, where a null means Inf rather than "absent".
check("washout: a bare Inf WOULD be lost by the JSON contract",
      is.null(fromJSON(as.character(sap_json(list(w = Inf))), simplifyVector = FALSE)$w))
check("washout: unbounded serialises as [null], not null",
      grepl('"w": [null]', as.character(sap_json(list(w = parse_washout("Inf")))), fixed = TRUE))
check("washout: a finite washout serialises as a one-element numeric array",
      grepl('"w": [365]', as.character(sap_json(list(w = parse_washout("365")))), fixed = TRUE))
w_json <- fromJSON(as.character(sap_json(list(w = parse_washout("Inf")))),
                   simplifyVector = FALSE)$w
check("washout: [null] reads back as a length-1 list, not an empty one",
      length(w_json) == 1 && is.null(w_json[[1]]))
check("washout: washout_days() resolves the null back to Inf",
      is.infinite(washout_days(w_json)))
check("washout: unset stays distinguishable from unbounded after a round trip",
      is.null(washout_days(NULL)) && !is.null(washout_days(w_json)))

unb <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(outcome_washout = "Inf"))
check("washout: unbounded survives the full round trip",
      washout_is_unbounded(unb$json$estimand$outcome_washout))
check("washout: unbounded round-trips back into the select as Inf",
      identical(unb$pf("outcome_washout"), "Inf"))
fin <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(outcome_washout = "365"))
check("washout: a finite washout round-trips back into the select",
      identical(fin$pf("outcome_washout"), "365"))
zero <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(outcome_washout = "0"))
check("washout: a 0-day washout round-trips and does not become 'unset'",
      identical(zero$pf("outcome_washout"), "0") &&
        identical(washout_days(zero$json$estimand$outcome_washout), 0))

# Pre-0.3.2 files: the string sentinel, and a bare number for a finite washout.
check("washout: the pre-0.3.2 'unbounded' sentinel still loads",
      identical(washout_select_value("unbounded"), "Inf"))
check("washout: a pre-0.3.2 bare number still loads",
      identical(washout_select_value(365), "365"))
check("washout: a pre-0.3.2 sentinel migrates to a numeric array on the way out",
      is.infinite(washout_days(parse_washout(washout_select_value("unbounded")))))

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
# 0.3.0 replaced `role` (Target / Comparator / ...) with `kind`. Each kind now
# carries only the arguments its generator actually takes.

# The cohort index as cohorts$by_name() hands it to a validator: keyed by name,
# with the JSON shapes (sex a vector, age_groups a list of numeric pairs).
#
# Note the target cohort and the target denominator are separate entries. They are
# different objects: the first is defined by entry criteria, the second is
# generated from it by generateTargetDenominatorCohortSet().
cohorts_idx <- list(
  "Metformin new users"   = list(kind = "target", entry_events = list("First dispensation")),
  "Metformin denominator" = list(kind = "target_denominator",
                                 target_cohort = "Metformin new users",
                                 time_at_risk = list(c(0, Inf)), sex = list("Both"),
                                 age_groups = list(c(0, 17), c(18, 64)),
                                 strata_variables = list("age_group", "sex")),
  "Men only"              = list(kind = "denominator", sex = list("Male"),
                                 age_groups = list(c(18, 64)),
                                 strata_variables = list("age_group", "sex")),
  "Lactic acidosis"       = list(kind = "outcome", sex = list("Both"))
)

# An old Target cohort is a PLAIN cohort, not a generated denominator -- mapping it
# to one would drop its entry events, which that kind's block does not carry.
check("cohort kind: an old Target role becomes a plain target cohort",
      identical(canonical_cohort_kind("Target"), "target"))
check("cohort kind: a target cohort is not a denominator",
      !is_denominator_kind("target") && is_denominator_kind("target_denominator"))
check("cohort kind: a current kind is left alone",
      identical(canonical_cohort_kind("denominator"), "denominator"))
check("cohort kind: NULL falls back", identical(canonical_cohort_kind(NULL), "denominator"))
check("cohort_by_name: a known cohort",
      identical(cohort_by_name(list(A = list(kind = "outcome")), "A")$kind, "outcome"))
check("cohort_by_name: a free-typed cohort nobody defined is NULL, not an error",
      is.null(cohort_by_name(list(A = list(kind = "outcome")), "Typed by hand")))
check("cohort_by_name: an empty pick is NULL",
      is.null(cohort_by_name(list(A = list()), "")))
check("cohort kind: an outcome falls back to the plain-cohort template",
      identical(cohort_template("outcome"), COHORT_TEMPLATES[["other"]]))
check("cohort kind: every registered key is an offered kind",
      all(names(COHORT_TEMPLATES) %in% COHORT_KINDS))

# Bounded intervals ------------------------------------------------------------
# ageGroup = list(c(0,17), c(18,30)) and timeAtRisk = list(c(0,30), c(31,60)) are
# both lists of numeric pairs, so they share one parser.

check("bounds: a comma pair", identical(parse_bounds("0, 30"), c(0, 30)))
check("bounds: a dash pair", identical(parse_bounds("18-64"), c(18, 64)))
check("bounds: Inf as an upper bound", identical(parse_bounds("0, Inf"), c(0, Inf)))
check("bounds: a trailing + is open-ended", identical(parse_bounds("65+"), c(65, Inf)))
check("bounds: junk is dropped, not guessed at", is.null(parse_bounds("sometime")))
check("bounds: a textarea becomes a list of pairs",
      identical(lapply(parse_bound_list("0, 30\n31, 60"), as.numeric),
                list(c(0, 30), c(31, 60))))

# JSON has no Infinity. Under na = "null" jsonlite writes Inf as null, so an
# unbounded upper bound is [0, null] -- and reading it back gives list(0, NULL).
# unlist() would collapse that to a ONE-element pair, silently losing the bound.
tar_json <- fromJSON(as.character(sap_json(list(t = parse_bound_list("0, Inf")))),
                     simplifyVector = FALSE)$t
check("bounds: an unbounded upper bound serialises as null",
      grepl("[0, null]", as.character(sap_json(list(t = parse_bound_list("0, Inf")))),
            fixed = TRUE))
check("bounds: a null upper bound reads back as a two-element pair",
      length(tar_json[[1]]) == 2 && is.null(tar_json[[1]][[2]]))
check("bounds: bound_upper() resolves a null back to Inf",
      is.infinite(bound_upper(tar_json[[1]])))
check("bounds: an unbounded pair round-trips back to the textarea",
      identical(format_bound_list(tar_json), "0, Inf"))
check("bounds: a finite pair round-trips",
      identical(format_bound_list(fromJSON(as.character(sap_json(
        list(t = parse_bound_list("31, 60")))), simplifyVector = FALSE)$t), "31, 60"))

# Cohort kind templates: mirror invariant, same as the analysis templates ------

for (kind in names(COHORT_TEMPLATES)) {
  tmpl <- COHORT_TEMPLATES[[kind]]
  ids  <- template_field_ids(tmpl)
  pk   <- unlist(tmpl$pickers, use.names = FALSE) %||% character(0)
  check(sprintf("[cohort:%s] ui() renders at least one input", kind), length(ids) > 0)
  check(sprintf("[cohort:%s] no id collides with a common field", kind),
        length(intersect(ids, c(COHORT_COMMON_FIELDS, "remove", "box", "kind_fields"))) == 0)
  check(sprintf("[cohort:%s] every declared picker is actually rendered", kind), all(pk %in% ids))

  faked <- stats::setNames(lapply(ids, function(i) "1"), ids)
  flat  <- tmpl$flatten(fromJSON(as.character(sap_json(tmpl$collect(faked))),
                                 simplifyVector = FALSE))
  missing <- setdiff(ids, names(flat))
  check(sprintf("[cohort:%s] every rendered input survives collect -> JSON -> flatten%s", kind,
                if (length(missing)) paste0(" (lost: ", paste(missing, collapse = ", "), ")") else ""),
        length(missing) == 0)
}

# Neither generator takes a washout: it is estimateIncidence(outcomeWashout = ),
# which the Incidence analysis captures. And a plain denominator has no
# timeAtRisk -- only generateTargetDenominatorCohortSet() does.
for (kind in names(COHORT_TEMPLATES)) {
  keys <- names(COHORT_TEMPLATES[[kind]]$collect(list()))
  check(sprintf("[cohort:%s] carries no washout", kind), !"washout_days" %in% keys)
}
check("cohort: a plain denominator has no time at risk",
      !"time_at_risk" %in% names(COHORT_TEMPLATES[["denominator"]]$collect(list())))
check("cohort: a target denominator does have a time at risk",
      "time_at_risk" %in% names(COHORT_TEMPLATES[["target_denominator"]]$collect(list())))
check("cohort: an outcome cohort carries none of the generator arguments",
      length(intersect(names(COHORT_TEMPLATES[["other"]]$collect(list())),
                       c("age_groups", "sex", "time_at_risk", "days_prior_observation"))) == 0)
check("cohort: the target denominator's block is exactly the generator's extra args",
      all(c("target_cohort", "time_at_risk", "requirements_at_entry") %in%
            names(COHORT_TEMPLATES[["target_denominator"]]$collect(list()))))

# Cohort validators -------------------------------------------------------------

TD <- COHORT_TEMPLATES[["target_denominator"]]
td_ok <- list(name = "TD", kind = "target_denominator", target_cohort = "Metformin new users",
              time_at_risk = list(c(0, 30)), sex = list("Both"), age_groups = list(c(0, 150)))
check("cohort validate: a well-formed target denominator has no problems",
      length(TD$validate(td_ok, cohorts_idx)) == 0)
check("cohort validate: a target denominator must name its target cohort",
      any(grepl("must name the target cohort",
                TD$validate(within(td_ok, target_cohort <- NA), cohorts_idx))))
check("cohort validate: the target cannot be another denominator",
      any(grepl("not another denominator",
                TD$validate(within(td_ok, target_cohort <- "Men only"), cohorts_idx))))
check("cohort validate: time at risk cannot end before it starts",
      any(grepl("ends before it starts",
                TD$validate(within(td_ok, time_at_risk <- list(c(30, 0))), cohorts_idx))))
check("cohort validate: time at risk must have at least one interval",
      any(grepl("at least one interval",
                TD$validate(within(td_ok, time_at_risk <- list()), cohorts_idx))))

# Template validators ----------------------------------------------------------

INC <- ANALYSIS_TEMPLATES[["Incidence"]]
# outcome_washout in its real on-disk shape: a one-element numeric array. The bare
# 365 used in the strata checks below is the pre-0.3.2 shape, which still reads.
ok_params <- list(denominator_cohort = "Metformin denominator",
                  estimand = list(outcome_washout = parse_washout("365"),
                                  repeated_events = TRUE,
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
# The trap the migration exists to avoid: a target cohort is the thing a
# denominator is generated FROM, not a denominator itself.
check("validate: a target cohort cannot be the denominator either",
      any(grepl("denominator or target-denominator",
                problems(with_params(denominator_cohort = "Metformin new users")))))
check("validate: an unset washout is reported",
      any(grepl("stated explicitly",
                problems(with_params(estimand = list(repeated_events = FALSE))))))
check("validate: repeated events with an unbounded washout is reported",
      any(grepl("finite outcome washout",
                problems(with_params(estimand = list(outcome_washout = parse_washout("Inf"),
                                                     repeated_events = TRUE))))))
# The unbounded washout as it actually comes back off disk: [null] -> list(NULL).
check("validate: repeated events is reported for an unbounded washout read from JSON",
      any(grepl("finite outcome washout",
                problems(with_params(estimand = list(outcome_washout = list(NULL),
                                                     repeated_events = TRUE))))))
check("validate: a 0-day washout is a stated washout, not an unset one",
      !any(grepl("stated explicitly",
                 problems(with_params(estimand = list(outcome_washout = parse_washout("0"),
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

# Before 0.3.2 an analysis named a plain target cohort as its denominator and
# carried its own time at risk. IncidencePrevalence has no such object: the
# denominator is a cohort set generated FROM the target, with timeAtRisk as one of
# the generator's arguments. migrate_sap() synthesises the missing denominator --
# a template cannot, because an analysis can only rewrite itself, not add a cohort.
old_sap <- list(
  cohorts = list(
    list(name = "Metformin new users", role = "Target",
         entry_events = list("First metformin dispensation"), concept_set = "cs_metformin"),
    list(name = "Lactic acidosis", role = "Outcome")
  ),
  proposed_analyses = list(list(
    name = "Legacy incidence", analysis_type = "Incidence rate",
    target_cohort = "Metformin new users", outcome_cohort = "Lactic acidosis",
    time_at_risk = list(start_offset_days = 7, start_anchor = "cohort start",
                        end_offset_days = 30, end_anchor = "cohort end")
  ))
)
migrated <- migrate_sap(old_sap)
den <- migrated$cohorts[[3]]

check("migrate_sap: an old Target stays a PLAIN cohort, keeping its definition",
      identical(migrated$cohorts[[1]]$kind, "target") &&
        identical(unlist(migrated$cohorts[[1]]$entry_events), "First metformin dispensation"))
check("migrate_sap: the missing target denominator is synthesised",
      identical(den$kind, "target_denominator") &&
        identical(den$target_cohort, "Metformin new users"))
check("migrate_sap: the anchored time at risk becomes one [start, end] interval",
      identical(as.numeric(den$time_at_risk[[1]]), c(7, 30)))
check("migrate_sap: the analysis is repointed at the synthesised denominator",
      identical(migrated$proposed_analyses[[1]]$denominator_cohort, den$name))
check("migrate_sap: the analysis no longer carries a time at risk",
      is.null(migrated$proposed_analyses[[1]]$time_at_risk))
check("migrate_sap: unrelated cohorts are untouched",
      identical(migrated$cohorts[[2]]$kind, "outcome") &&
        is.null(migrated$cohorts[[2]]$time_at_risk))

# Two analyses on the same target and the same window are one generator call, so
# they must share one denominator -- but a different window is a different call.
two <- migrate_sap(list(
  cohorts = list(list(name = "T", role = "Target")),
  proposed_analyses = list(
    list(name = "A", target_cohort = "T",
         time_at_risk = list(start_offset_days = 0, end_offset_days = 30)),
    list(name = "B", target_cohort = "T",
         time_at_risk = list(start_offset_days = 0, end_offset_days = 30)),
    list(name = "C", target_cohort = "T",
         time_at_risk = list(start_offset_days = 31, end_offset_days = 60))
  )
))
check("migrate_sap: analyses sharing a target and a window share one denominator",
      identical(two$proposed_analyses[[1]]$denominator_cohort,
                two$proposed_analyses[[2]]$denominator_cohort))
check("migrate_sap: a different window gets its own denominator",
      !identical(two$proposed_analyses[[1]]$denominator_cohort,
                 two$proposed_analyses[[3]]$denominator_cohort))
check("migrate_sap: one target, two windows, two synthesised denominators",
      length(two$cohorts) == 3)

check("migrate_sap: an analysis naming an undefined cohort is a no-op, not an error",
      length(migrate_sap(list(
        cohorts = list(list(name = "C", role = "Outcome")),
        proposed_analyses = list(list(denominator_cohort = "Nope",
                                      time_at_risk = list(start_offset_days = 7)))
      ))$cohorts) == 1)
check("migrate_sap: an analysis already on a real denominator is left alone",
      identical(migrate_sap(list(
        cohorts = list(list(name = "D", kind = "denominator")),
        proposed_analyses = list(list(denominator_cohort = "D"))
      ))$proposed_analyses[[1]]$denominator_cohort, "D"))

# Neither generator takes a washout, and there is nowhere to move a cohort one to.
check("migrate_sap: an old cohort washout is dropped, not misapplied",
      is.null(migrate_sap(list(
        cohorts = list(list(name = "D", kind = "denominator", washout_days = 365))
      ))$cohorts[[1]]$washout_days))
check("migrate_sap: old free-text age groups become numeric pairs",
      identical(as.numeric(migrate_sap(list(cohorts = list(list(
        name = "D", kind = "denominator", age_groups = list("18-64", "65+")
      ))))$cohorts[[1]]$age_groups[[1]]), c(18, 64)))
check("migrate_sap: the old study period becomes the cohort date range",
      identical(migrate_sap(list(cohorts = list(list(
        name = "D", kind = "denominator", study_period_start = "2015-01-01"
      ))))$cohorts[[1]]$cohort_date_range_start, "2015-01-01"))

# Nothing a template collects may collide with a common key, or c() in load()
# would silently prefer the common one.
for (type in names(ANALYSIS_TEMPLATES)) {
  keys <- names(ANALYSIS_TEMPLATES[[type]]$collect(list()))
  check(sprintf("[%s] no collected key collides with a common field", type),
        length(intersect(keys, ANALYSIS_COMMON_FIELDS)) == 0)
}

cat("\n", if (failures == 0) "All checks passed.\n" else sprintf("%d check(s) failed.\n", failures), sep = "")
quit(status = if (failures == 0) 0 else 1)
