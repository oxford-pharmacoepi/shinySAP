# Cohort kinds. 0.3.0 replaced `role` (Target / Comparator / ...) with `kind`.
# Each kind now carries only the arguments its generator actually takes.

# An old Target cohort is a PLAIN cohort, not a generated denominator -- mapping it
# to one would drop its entry events, which that kind's block does not carry.
test_that("cohort kind: an old Target role becomes a plain target cohort",
          expect_identical(canonical_cohort_kind("Target"), "target"))
test_that("cohort kind: a target cohort is not a denominator", {
  expect_false(is_denominator_kind("target"))
  expect_true(is_denominator_kind("target_denominator"))
})
test_that("cohort kind: a current kind is left alone",
          expect_identical(canonical_cohort_kind("denominator"), "denominator"))
test_that("cohort kind: NULL falls back",
          expect_identical(canonical_cohort_kind(NULL), "denominator"))
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
  ids  <- template_field_ids(tmpl)
  pk   <- unlist(tmpl$pickers, use.names = FALSE) %||% character(0)

  test_that(sprintf("[cohort:%s] ui() renders at least one input", kind),
            expect_gt(length(ids), 0))
  test_that(sprintf("[cohort:%s] no id collides with a common field", kind),
            expect_length(intersect(ids, c(COHORT_COMMON_FIELDS, "remove", "box", "kind_fields")),
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
              timeAtRisk = list(c(0, 30)), sex = list("Both"), ageGroup = list(c(0, 150)))

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
test_that("migrate_sap: an old file's declared strata columns are dropped", {
  m <- migrate_sap(list(cohorts = list(list(
    name = "D", kind = "denominator", strata_variables = list("age_group", "sex", "region")
  ))))
  expect_null(m$cohorts[[1]]$strata_variables)
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
