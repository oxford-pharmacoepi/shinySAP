# Cohort kinds. Each kind carries only the arguments its generator actually takes.

# A target cohort is a PLAIN cohort, not a generated denominator -- reading it as
# one would drop its entry events, which that kind's block does not carry.
test_that("cohort kind: a target cohort is not a denominator", {
  expect_false(is_denominator_kind("target"))
  expect_true(is_denominator_kind("target_denominator"))
})
test_that("cohort kind: a current kind is left alone",
          expect_identical(canonical_cohort_kind("denominator"), "denominator"))
# A new card starts with no kind chosen -- unset is "", a state of its own, not a
# silent denominator. An unknown kind from a hand-edited file reads as a plain
# cohort: that loses nothing, where a denominator would invent generator arguments.
test_that("cohort kind: unset stays unset, never a silent default", {
  expect_identical(canonical_cohort_kind(NULL), "")
  expect_identical(canonical_cohort_kind(""), "")
  expect_identical(canonical_cohort_kind(NA), "")
})
test_that("cohort kind: an unknown kind falls back to a plain cohort",
          expect_identical(canonical_cohort_kind("something made up"), "other"))
test_that("cohort: an unset kind is a problem, not a validated card", {
  msgs <- cohort_problems(list(name = "X"), list())
  expect_true(any(grepl("no kind", msgs)))
})
test_that("cohort: a kind that is set validates through its template",
          expect_true(any(grepl("Sex must be",
                                cohort_problems(list(kind = "denominator"), list())))))
test_that("cohort_by_name: a known cohort",
          expect_identical(cohort_by_name(list(A = list(kind = "outcome")), "A")$kind, "outcome"))
test_that("cohort_by_name: a free-typed cohort nobody defined is NULL, not an error",
          expect_null(cohort_by_name(list(A = list(kind = "outcome")), "Typed by hand")))
test_that("cohort_by_name: an empty pick is NULL",
          expect_null(cohort_by_name(list(A = list()), "")))
test_that("cohort kind: an outcome falls back to the plain-cohort template",
          expect_identical(cohort_template("outcome"), COHORT_TEMPLATES[["other"]]))
test_that("cohort kind: every registered key is an offered kind",
          expect_true(all(names(COHORT_TEMPLATES) %in% COHORT_KINDS)))

# Bounded intervals ------------------------------------------------------------
# ageGroup = list(c(0,17), c(18,30)) and timeAtRisk = list(c(0,30), c(31,60)) are
# both lists of numeric pairs, so they share one parser.

test_that("bounds: a comma pair", expect_identical(parse_bounds("0, 30"), c(0, 30)))
test_that("bounds: a dash pair", expect_identical(parse_bounds("18-64"), c(18, 64)))
test_that("bounds: Inf as an upper bound", expect_identical(parse_bounds("0, Inf"), c(0, Inf)))
test_that("bounds: a trailing + is open-ended", expect_identical(parse_bounds("65+"), c(65, Inf)))
test_that("bounds: junk is dropped, not guessed at", expect_null(parse_bounds("sometime")))
test_that("bounds: a textarea becomes a list of pairs",
          expect_identical(lapply(parse_bound_list("0, 30\n31, 60"), as.numeric),
                           list(c(0, 30), c(31, 60))))

# JSON has no Infinity. Under na = "null" jsonlite writes Inf as null, so an
# unbounded upper bound is [0, null] -- and reading it back gives list(0, NULL).
# unlist() would collapse that to a ONE-element pair, silently losing the bound.
tar_json <- fromJSON(as.character(sap_json(list(t = parse_bound_list("0, Inf")))),
                     simplifyVector = FALSE)$t

test_that("bounds: an unbounded upper bound serialises as null",
          expect_match(as.character(sap_json(list(t = parse_bound_list("0, Inf")))),
                       "[0, null]", fixed = TRUE))
test_that("bounds: a null upper bound reads back as a two-element pair", {
  expect_length(tar_json[[1]], 2)
  expect_null(tar_json[[1]][[2]])
})
test_that("bounds: bound_upper() resolves a null back to Inf",
          expect_true(is.infinite(bound_upper(tar_json[[1]]))))
test_that("bounds: an unbounded pair round-trips back to the textarea",
          expect_identical(format_bound_list(tar_json), "0, Inf"))
test_that("bounds: a finite pair round-trips",
          expect_identical(format_bound_list(fromJSON(as.character(sap_json(
            list(t = parse_bound_list("31, 60")))), simplifyVector = FALSE)$t), "31, 60"))

# Cohort kind templates: mirror invariant, same as the analysis templates ------

for (kind in names(COHORT_TEMPLATES)) {
  tmpl <- COHORT_TEMPLATES[[kind]]
  # The cohort-set preview is an output, not an input: it holds no value, so it
  # is exempt from the round trip -- same as the analysis DISPLAY_ONLY_IDS.
  ids  <- setdiff(template_field_ids(tmpl), COHORT_DISPLAY_ONLY_IDS)
  pk   <- unlist(tmpl$pickers, use.names = FALSE) %||% character(0)

  test_that(sprintf("[cohort:%s] ui() renders at least one input", kind),
            expect_gt(length(ids), 0))
  test_that(sprintf("[cohort:%s] no id collides with a common field", kind),
            expect_length(intersect(ids, c(COHORT_COMMON_FIELDS, "remove", "duplicate",
                                           "box", "kind_fields")),
                          0))
  test_that(sprintf("[cohort:%s] every declared picker is actually rendered", kind),
            expect_true(all(pk %in% ids)))
  test_that(sprintf("[cohort:%s] every rendered input survives collect -> JSON -> flatten", kind), {
    faked <- stats::setNames(lapply(ids, function(i) "1"), ids)
    flat  <- tmpl$flatten(fromJSON(as.character(sap_json(tmpl$collect(faked))),
                                   simplifyVector = FALSE))
    expect_identical(setdiff(ids, names(flat)), character(0))   # failure prints what was lost
  })
}

# Neither generator takes a washout: it is estimateIncidence(outcomeWashout = ),
# which the Incidence analysis captures. And a plain denominator has no
# timeAtRisk -- only generateTargetDenominatorCohortSet() does.
for (kind in names(COHORT_TEMPLATES)) {
  test_that(sprintf("[cohort:%s] carries no washout", kind),
            expect_false("washout_days" %in% names(COHORT_TEMPLATES[[kind]]$collect(list()))))
}
test_that("cohort: a plain denominator has no time at risk",
          expect_false("timeAtRisk" %in%
                         names(COHORT_TEMPLATES[["denominator"]]$collect(list()))))
test_that("cohort: a target denominator does have a time at risk",
          expect_true("timeAtRisk" %in%
                        names(COHORT_TEMPLATES[["target_denominator"]]$collect(list()))))
# 0.4.15 folded index_rule and concept_set back into the text fields: the
# codelist is cited inline in the entry event, the index rule is an inclusion
# criterion.
test_that("cohort: a plain cohort no longer collects index_rule or concept_set", {
  keys <- names(COHORT_TEMPLATES[["target"]]$collect(list()))
  expect_false(any(c("index_rule", "concept_set") %in% keys))
})

test_that("cohort: an outcome cohort carries none of the generator arguments",
          expect_length(intersect(names(COHORT_TEMPLATES[["other"]]$collect(list())),
                                  c("ageGroup", "sex", "timeAtRisk",
                                    "daysPriorObservation")), 0))

# The whole point: the JSON keys ARE generateTargetDenominatorCohortSet()'s
# argument names, in its own order. `cdm` is a live database handle and `name` is
# the cohort's own name, so neither is in the kind's block. targetCohortId is not
# captured yet -- the ids are handled internally for now.
test_that("cohort: the target denominator's keys are exactly the generator's arguments", {
  expect_identical(
    names(COHORT_TEMPLATES[["target_denominator"]]$collect(list())),
    c("targetCohortTable", "cohortDateRange", "timeAtRisk", "ageGroup", "sex",
      "daysPriorObservation", "requirementsAtEntry", "requirementInteractions"))
})
test_that("cohort: the plain denominator's keys are exactly its generator's arguments", {
  expect_identical(
    names(COHORT_TEMPLATES[["denominator"]]$collect(list())),
    c("cohortDateRange", "ageGroup", "sex", "daysPriorObservation",
      "requirementInteractions"))
})
# An untouched form must not put decisions in the JSON: no "Both", no 0 days,
# no interactions. The generator's defaults still apply downstream (the cohort
# set preview says so), but the SAP records only what the author chose -- and
# the validator objects until sex actually is chosen.
test_that("cohort: an untouched denominator form collects no decisions", {
  p <- COHORT_TEMPLATES[["denominator"]]$collect(list())
  expect_length(p$sex, 0)
  expect_length(p$daysPriorObservation, 0)
  expect_false(p$requirementInteractions)
})
test_that("cohort: an untouched target denominator collects no decisions either", {
  p <- COHORT_TEMPLATES[["target_denominator"]]$collect(list())
  expect_length(p$timeAtRisk, 0)
  expect_false(p$requirementsAtEntry)
})
test_that("cohort: an unset sex is a problem, not a silent Both",
          expect_true(any(grepl("Sex must be",
                                COHORT_TEMPLATES[["denominator"]]$validate(
                                  list(kind = "denominator"), list())))))

test_that("cohort: no denominator key is snake_case any more", {
  for (kind in c("denominator", "target_denominator")) {
    keys <- names(COHORT_TEMPLATES[[kind]]$collect(list()))
    expect_false(any(grepl("_", keys)), info = kind)
  }
})

# cohortDateRange is ONE argument taking TWO dates -- as.Date(c(NA, NA)) -- so it
# is one key holding a pair, not two keys, and a missing bound is null.
test_that("cohort: cohortDateRange is a two-element array, not two keys", {
  den  <- COHORT_TEMPLATES[["denominator"]]
  both <- den$collect(list(cohortDateRangeStart = "2015-01-01",
                           cohortDateRangeEnd = "2024-12-31"))$cohortDateRange
  expect_identical(as.character(both), c("2015-01-01", "2024-12-31"))
  expect_match(as.character(sap_json(list(r = both))),
               '"r": ["2015-01-01", "2024-12-31"]', fixed = TRUE)
})
test_that("cohort: an unset cohortDateRange is [null, null], the argument's own default", {
  den <- COHORT_TEMPLATES[["denominator"]]
  dr  <- den$collect(list())$cohortDateRange
  expect_match(as.character(sap_json(list(r = dr))), '"r": [null, null]', fixed = TRUE)
})
test_that("cohort: a null bound survives the round trip without collapsing the pair", {
  den <- COHORT_TEMPLATES[["denominator"]]
  dr  <- den$collect(list(cohortDateRangeStart = "2015-01-01"))$cohortDateRange
  rt  <- fromJSON(as.character(sap_json(list(r = dr))), simplifyVector = FALSE)$r
  expect_length(rt, 2)                          # unlist() would make this 1
  expect_identical(date_bound(rt, 1), "2015-01-01")
  expect_null(date_bound(rt, 2))
})
test_that("cohort: flatten splits cohortDateRange back onto the two date fields", {
  flat <- COHORT_TEMPLATES[["denominator"]]$flatten(
    list(cohortDateRange = list("2015-01-01", NULL)))
  expect_identical(flat$cohortDateRangeStart, "2015-01-01")
  expect_null(flat$cohortDateRangeEnd)
})

# Cohort validators -------------------------------------------------------------

TD <- COHORT_TEMPLATES[["target_denominator"]]
td_ok <- list(name = "TD", kind = "target_denominator",
              targetCohortTable = "Metformin new users",
              timeAtRisk = list(c(0, 30)), sex = list("Both"), ageGroup = list(c(0, 150)),
              daysPriorObservation = list(0))

# A copy of td_ok with one key changed. NOT within(): its `targetCohortTable <- x`
# reads as a camelCase *variable* assignment, which the linter rejects. These are
# list keys, and they are camelCase on purpose -- they are the generator's own
# argument names.
td_but <- function(key, value) {
  x <- td_ok
  x[[key]] <- value
  x
}

test_that("cohort validate: a well-formed target denominator has no problems",
          expect_length(TD$validate(td_ok, cohorts_idx), 0))
test_that("cohort validate: a target denominator must name its target cohort",
          expect_true(any(grepl("must name the target cohort",
                                TD$validate(td_but("targetCohortTable", NA), cohorts_idx)))))
test_that("cohort validate: the target cannot be another denominator",
          expect_true(any(grepl("not another denominator",
                                TD$validate(td_but("targetCohortTable", "Men only"),
                                            cohorts_idx)))))
test_that("cohort validate: time at risk cannot end before it starts",
          expect_true(any(grepl("ends before it starts",
                                TD$validate(td_but("timeAtRisk", list(c(30, 0))),
                                            cohorts_idx)))))
test_that("cohort validate: time at risk must have at least one interval",
          expect_true(any(grepl("at least one interval",
                                TD$validate(td_but("timeAtRisk", list()), cohorts_idx)))))
# Both bounds are days from TARGET ENTRY: there is no time before day 0.
test_that("cohort validate: time at risk cannot start before target entry", {
  expect_true(any(grepl("must start at day 0",
                        TD$validate(td_but("timeAtRisk", list(c(-7, 30))), cohorts_idx))))
  expect_true(any(grepl("must start at day 0",
                        TD$validate(td_but("timeAtRisk", list(c(NA, 30))), cohorts_idx))))
})

# Value domains mirror what generateDenominatorCohortSet() will actually accept:
# finite ages within 0..150, non-negative days, a forward date range -- and [],
# the blank form's honest "not decided yet", is a problem, never a silent default.
test_that("cohort validate: empty age groups and prior observation are problems", {
  expect_true(any(grepl("Age groups must have at least one",
                        TD$validate(td_but("ageGroup", list()), cohorts_idx))))
  expect_true(any(grepl("prior observation must have at least one",
                        TD$validate(td_but("daysPriorObservation", list()), cohorts_idx))))
})
test_that("cohort validate: an age group must end at a finite age", {
  # "65+" parses to an unbounded upper bound, which serialises as [65, null].
  errs <- TD$validate(td_but("ageGroup", parse_bound_list("65+")), cohorts_idx)
  expect_true(any(grepl("no upper bound.*finite", errs)))
})
test_that("cohort validate: ages beyond 0..150 are problems", {
  expect_true(any(grepl("must lie within 0 and 150",
                        TD$validate(td_but("ageGroup", list(c(0, 200))), cohorts_idx))))
  expect_true(any(grepl("must lie within 0 and 150",
                        TD$validate(td_but("ageGroup", list(c(-5, 64))), cohorts_idx))))
})
test_that("cohort validate: prior observation must be non-negative numbers", {
  expect_true(any(grepl("none negative",
                        TD$validate(td_but("daysPriorObservation", list(-30)), cohorts_idx))))
  # A lone NA reads as unset (%||% treats a length-1 NA as absent), so it lands
  # on the at-least-one-value message; an NA hiding AMONG numbers is caught here.
  expect_true(any(grepl("at least one value",
                        TD$validate(td_but("daysPriorObservation", list(NA)), cohorts_idx))))
  expect_true(any(grepl("none negative",
                        TD$validate(td_but("daysPriorObservation", list(0, NA)), cohorts_idx))))
})
test_that("cohort validate: a date range cannot start after it ends", {
  bad <- td_but("cohortDateRange", list("2024-12-31", "2015-01-01"))
  expect_true(any(grepl("starts .* after it ends", TD$validate(bad, cohorts_idx))))
  # One open bound is fine: null means the database decides that side.
  half <- td_but("cohortDateRange", list("2015-01-01", NULL))
  expect_length(TD$validate(half, cohorts_idx), 0)
})

# Strata columns are NOT a cohort field ----------------------------------------
#
# 0.4.1 removed the "strata columns on this denominator" textarea.
# generateDenominatorCohortSet() produces age_group and sex and nothing else, so
# there was nothing for an author to decide -- and the field let them *declare* a
# column the generator does not make, which the strata picker then offered and the
# validator waved through.

test_that("cohort: a denominator no longer collects strata columns", {
  expect_false("strata_variables" %in%
                 names(COHORT_TEMPLATES[["denominator"]]$collect(list())))
  expect_false("strata_variables" %in%
                 names(COHORT_TEMPLATES[["target_denominator"]]$collect(list())))
})
test_that("cohort: the denominator card no longer renders a strata input", {
  for (kind in c("denominator", "target_denominator")) {
    expect_false("strata_variables" %in%
                   template_field_ids(COHORT_TEMPLATES[[kind]]))
  }
})
test_that("cohort: a denominator's strata columns come from the kind, not a field", {
  expect_identical(cohort_strata_variables(list(kind = "denominator")), STRATA_VARIABLES)
  expect_identical(cohort_strata_variables(list(kind = "target_denominator")),
                   STRATA_VARIABLES)
})
test_that("cohort: a declared column in an OLD file cannot smuggle itself back in", {
  # The field is gone; naming it in the JSON must not re-enable stratifying by it.
  smuggled <- list(kind = "denominator", strata_variables = list("age_group", "sex", "region"))
  expect_false("region" %in% cohort_strata_variables(smuggled))
})
test_that("cohort: anything that is not a denominator carries no strata columns", {
  expect_length(cohort_strata_variables(list(kind = "outcome")), 0)
  expect_length(cohort_strata_variables(NULL), 0)
})

# What the cohort set actually generates ---------------------------------------
#
# A denominator is a cohort SET: the arguments are axes it is crossed over, and an
# analysis runs on every cohort in it. The Analyses tab spells them out, so the
# enumeration has to match what generate*DenominatorCohortSet() would really make.

den_set <- function(...) denominator_cohort_set(list(kind = "denominator", ...))

test_that("cohort set: interactions cross ageGroup x sex x daysPriorObservation", {
  # 3 age groups x 3 sexes = 9 cohorts, not 1.
  s <- den_set(ageGroup = list(c(0, 17), c(18, 45), c(46, 64)),
               sex = list("Both", "Female", "Male"), daysPriorObservation = list(0))
  expect_length(s, 9)
  expect_length(den_set(ageGroup = list(c(0, 17), c(18, 64)),
                        sex = list("Male", "Female"),
                        daysPriorObservation = list(0, 365)), 8)   # 2 x 2 x 2
})
# The ORDER is the part that matters beyond the count, because it is what a
# cohort id means: an analysis restricted to denominatorCohortId = 3 gets
# whichever combination the generator numbered 3, and this app's preview is what
# tells the author which that is.
#
# Pinned against the real thing rather than reasoned about. Running
# generateDenominatorCohortSet() (IncidencePrevalence 1.2.1) on a mock CDM with
# ageGroup = list(c(0, 40), c(41, 150)), sex = c("Both", "Female") and
# daysPriorObservation = c(0, 365) returns, by cohort_definition_id:
#
#   1  0 to 40   Both    0        5  41 to 150  Both    0
#   2  0 to 40   Both    365      6  41 to 150  Both    365
#   3  0 to 40   Female  0        7  41 to 150  Female  0
#   4  0 to 40   Female  365      8  41 to 150  Female  365
#
# -- age outermost, then sex, then days of prior observation. If a future
# version of the package reorders that, this test is what says so.
test_that("cohort set: the order matches the generator's own cohort ids", {
  s <- den_set(ageGroup = list(c(0, 40), c(41, 150)),
               sex = list("Both", "Female"),
               daysPriorObservation = list(0, 365))
  got <- lapply(s, function(x) list(age = unlist(x$ageGroup), sex = x$sex,
                                    prior = x$daysPriorObservation))
  expect_identical(vapply(got, function(g) g$age[[1]], numeric(1)),
                   c(0, 0, 0, 0, 41, 41, 41, 41))
  expect_identical(vapply(got, function(g) as.character(g$sex), character(1)),
                   c("Both", "Both", "Female", "Female",
                     "Both", "Both", "Female", "Female"))
  expect_identical(vapply(got, function(g) as.numeric(g$prior), numeric(1)),
                   c(0, 365, 0, 365, 0, 365, 0, 365))
})

test_that("cohort set: each generated cohort is one distinct combination", {
  s <- den_set(ageGroup = list(c(0, 17), c(18, 64)), sex = list("Male", "Female"))
  lines <- vapply(s, format_denominator_cohort, character(1))
  expect_length(unique(lines), 4)
  expect_true(any(grepl("^Age 18, 64 . Female", lines)))
})

# requirementInteractions = FALSE: "only the first value specified for the other
# factors will be used" -- so each factor varies alone against the baseline. That
# is why the docs say order matters when it is off.
test_that("cohort set: without interactions each factor varies alone", {
  s <- denominator_cohort_set(list(
    kind = "denominator", requirementInteractions = FALSE,
    ageGroup = list(c(0, 17), c(18, 45), c(46, 64)),
    sex = list("Both", "Female", "Male"), daysPriorObservation = list(0)))
  expect_length(s, 5)                       # baseline + 2 extra ages + 2 extra sexes
  lines <- vapply(s, format_denominator_cohort, character(1))
  # Every non-baseline cohort holds the FIRST value of the factors it does not vary.
  expect_true(all(grepl("Both", lines[2:3])))          # the extra ages keep sex[1]
  expect_true(all(grepl("^Age 0, 17", lines[4:5])))    # the extra sexes keep age[1]
})
test_that("cohort set: without interactions, order decides the baseline", {
  s <- denominator_cohort_set(list(
    kind = "denominator", requirementInteractions = FALSE,
    ageGroup = list(c(18, 64), c(0, 17)), sex = list("Male", "Female")))
  expect_match(format_denominator_cohort(s[[1]]), "^Age 18, 64 . Male")
})

# timeAtRisk is NOT one of the interacting factors: each interval generates its own
# SET, so it multiplies whatever the requirements produce.
test_that("cohort set: each time-at-risk interval multiplies the set", {
  s <- denominator_cohort_set(list(
    kind = "target_denominator", targetCohortTable = "T",
    ageGroup = list(c(0, 17), c(18, 64)), sex = list("Male", "Female"),
    timeAtRisk = list(c(0, 30), c(31, NULL))))
  expect_length(s, 8)                                  # 2 x 2 x 2 windows
  expect_match(format_denominator_cohort(s[[1]]), "time at risk 0, 30")
})
test_that("cohort set: a plain denominator has no time at risk in its lines", {
  expect_no_match(format_denominator_cohort(den_set()[[1]]), "time at risk")
})
test_that("cohort set: an empty denominator is the generator's own defaults", {
  s <- den_set()
  expect_length(s, 1)
  expect_match(format_denominator_cohort(s[[1]]), "Age 0, 150 . Both . 0 days")
})

# 0.4.13: a cohort names the CDM sources it is generated against -- the
# SAP-level counterpart of the generators' `cdm` argument. A common field, so
# the kind blocks stay exactly the generator's other arguments.
test_that("cohort: the card collects its CDM sources and they survive a round trip", {
  testServer(cohorts_server, {
    session$setInputs(add = 1)
    session$setInputs("cohort_1-name" = "D", "cohort_1-kind" = "denominator",
                      "cohort_1-data_sources" = c("cprd", "sidiap"))
    d <- isolate(data_r())[[1]]
    expect_identical(as.character(d$data_sources), c("cprd", "sidiap"))
    # The Duplicate/load path keeps them: common keys pass through flatten.
    expect_identical(as.character(cohort_to_prefill(d)$data_sources), c("cprd", "sidiap"))
  })
})
test_that("cohort: no picked sources collect as an empty array, not a guess", {
  testServer(cohorts_server, {
    session$setInputs(add = 1)
    session$setInputs("cohort_1-name" = "D", "cohort_1-kind" = "denominator")
    expect_length(isolate(data_r())[[1]]$data_sources, 0)
  })
})

# Shiny drops a selected value it cannot find among the choices, so a
# create-to-type selectize MUST fold its saved values into them -- or every
# rebuild (load, kind switch, duplicate) empties the field and the next
# autosave erases the value from the file. The user-visible bug: save 0 days,
# reload the SAP, the field is blank.
test_that("cohort: saved daysPriorObservation renders back into its field", {
  for (v in list(0, 365)) {
    html <- as.character(COHORT_TEMPLATES[["denominator"]]$ui(
      function(x) x, prefiller(list(daysPriorObservation = list(v)))))
    expect_match(html, sprintf("<option value=\"%s\" selected", v), fixed = TRUE)
  }
})

# The cohort card previews the set live, so an author sees the cross product at
# the point of editing rather than inferring it (or first meeting it on the
# Analyses tab). The placeholder is display-only: the card server fills it.
test_that("cohort: both denominator kinds render the cohort-set preview", {
  for (kind in c("denominator", "target_denominator")) {
    expect_true("cohort_set_preview" %in% template_field_ids(COHORT_TEMPLATES[[kind]]),
                info = kind)
  }
})
test_that("cohort: a plain cohort renders no cohort-set preview",
          expect_false("cohort_set_preview" %in%
                         template_field_ids(COHORT_TEMPLATES[["other"]])))
test_that("cohort: the preview never reaches the JSON", {
  for (kind in c("denominator", "target_denominator")) {
    expect_false("cohort_set_preview" %in%
                   names(COHORT_TEMPLATES[[kind]]$collect(list())), info = kind)
  }
})
# Duplicated names shadow each other in cohort_by_name and every picker --
# a problem of the list, which no single card's validator can see.
# The bracket convention as a contract: [cs_x] must resolve to a codelist on
# the Codelists tab, and an idle codelist gets a nudge.
test_that("codelist refs: bracketed references are extracted from all text fields", {
  refs <- extract_codelist_refs(list(
    entry_events = list("Flu vaccination [cs_flu]"),
    inclusion_criteria = list("Prior MI [cs_mi]", "Index on first occurrence"),
    exit_criteria = list("End of observation")
  ))
  expect_identical(sort(refs), c("cs_flu", "cs_mi"))
  expect_length(extract_codelist_refs(list(kind = "denominator")), 0)
})
test_that("codelist refs: an unresolved citation is a problem naming the cohort", {
  p <- codelist_reference_problems(
    list(list(name = "T", entry_events = list("Flu [cs_flu]"))), c("cs_influenza"))
  msgs <- unlist(lapply(p, function(x) x$messages))
  expect_true(any(grepl("\\[cs_flu\\], which is not on the Codelists tab", msgs)))
  expect_true(any(grepl("No cohort cites \\[cs_influenza\\]", msgs)))
})
test_that("codelist refs: a resolved citation raises nothing", {
  expect_length(codelist_reference_problems(
    list(list(name = "T", entry_events = list("Flu [cs_flu]"))), "cs_flu"), 0)
})
test_that("codelist refs: no codelists and no references is silent", {
  expect_length(codelist_reference_problems(list(list(name = "T")), character(0)), 0)
})

test_that("duplicate names: two cohorts sharing a name is one problem", {
  p <- duplicate_name_problems(list(list(name = "D"), list(name = "D"),
                                    list(name = "Other")))
  expect_length(p, 1)
  expect_identical(p[[1]]$name, "D")
  expect_match(p[[1]]$messages, "2 cohorts are named 'D'")
})
test_that("duplicate names: distinct, blank and NA names raise nothing",
          expect_length(duplicate_name_problems(list(list(name = "A"), list(name = "B"),
                                                     list(name = ""), list(name = NA),
                                                     list())), 0))

# cohort_to_prefill() is the ONE path a card is rebuilt from -- load and the
# Duplicate button both use it.
test_that("cohort_to_prefill: an unknown kind is canonicalised and flattened", {
  p <- cohort_to_prefill(list(name = "T", kind = "something made up"))
  expect_identical(p$kind, "other")
})
test_that("cohort_to_prefill: a denominator's date range splits back onto its fields", {
  p <- cohort_to_prefill(list(kind = "denominator",
                              cohortDateRange = list("2015-01-01", NULL)))
  expect_identical(p$cohortDateRangeStart, "2015-01-01")
})
test_that("cohort_to_prefill: no kind survives as no kind", {
  expect_identical(cohort_to_prefill(list(name = "X"))$kind, "")
})

# The cohort card and the analysis card render the SAME panel -- facts grid plus
# generated set -- so the two views can never disagree; only the sentences differ.
test_that("denominator_panel: facts, custom intro and custom lead render together", {
  html <- as.character(denominator_panel(
    list(kind = "denominator", sex = list("Female"), ageGroup = list(c(0, 17))),
    "My intro:", lead = function(n) sprintf("Makes %d.", n)
  ))
  for (needle in c("My intro:", "Age groups", "0, 17", "Sex", "Female",
                   "Prior observation", "Makes 1.")) {
    expect_match(html, needle, fixed = TRUE)
  }
  expect_no_match(html, "Time at risk")   # plain denominator: no such fact
})

test_that("denominator_cohort_set_ui: a caller can supply its own lead sentence", {
  ui <- denominator_cohort_set_ui(list(kind = "denominator"),
                                  lead = function(n) sprintf("Makes %d.", n))
  expect_match(as.character(ui), "Makes 1.", fixed = TRUE)
  # NULL keeps the analysis-card wording.
  expect_match(as.character(denominator_cohort_set_ui(list(kind = "denominator"))),
               "the analysis runs on it")
})

# A real denominator multiplies: C1-001's is 15 age groups x 3 sexes x 3
# prior-observation values. Laid out in full that pushed the card past the bottom
# of the layout column, so the list is bounded and scrolls inside the card.
test_that("denominator_cohort_set_ui: a large set scrolls instead of growing the card", {
  big <- list(kind = "denominator", requirementInteractions = TRUE,
              sex = list("Both", "Male", "Female"),
              daysPriorObservation = list(365, 0, 1095),
              ageGroup = lapply(seq(0, 140, by = 10), function(a) c(a, a + 9)))
  html <- as.character(denominator_cohort_set_ui(big))
  expect_match(html, "max-height", fixed = TRUE)
  expect_match(html, "overflow: auto", fixed = TRUE)
  # A marker sits in the list's left padding, so that padding must clear the
  # widest one -- narrower, and the scroll container clips the numbers.
  expect_match(html, 'class="mb-0 ps-5 font-monospace"', fixed = TRUE)
  # EVERY cohort is listed: the scroll box is what bounds the card, not a cap on
  # the list, so nothing is dropped and there is no remainder to report.
  expect_equal(length(denominator_cohort_set(big)), 135)
  expect_length(gregexpr("<li>", html, fixed = TRUE)[[1]], 135)
  expect_no_match(html, "not listed here", fixed = TRUE)
  # The last entry is reachable -- the point of listing them all.
  expect_match(html, "Age 140, 149 | Female | 1095 days prior observation", fixed = TRUE)
})
