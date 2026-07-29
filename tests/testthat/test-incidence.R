# The Incidence template: collect() -> JSON -> flatten() round trips, the
# strata encoding, and the outcome washout in all its states.

inc <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]], list(
  denominatorTable          = "Metformin new users",
  outcomeTable              = "Lactic acidosis",
  censorTable               = "",
  outcomeWashout            = 365,
  outcomeWashout_unbounded  = FALSE,
  repeatedEvents            = TRUE,
  interval                  = c("years", "overall"),
  completeDatabaseIntervals = TRUE,
  strata                    = c("sex", "sex, age_group"),
  include_overall_strata    = TRUE
))

# `parameters` maps 1:1 onto estimateIncidence(): the keys ARE the argument names,
# flat and in signature order, with no `estimand` wrapper the function has no
# concept of. cdm is a runtime handle, so it is the only argument absent.
test_that("incidence: parameters are exactly estimateIncidence()'s arguments, in order",
          expect_identical(
            names(inc$json),
            c("denominatorTable", "outcomeTable", "censorTable",
              "denominatorCohortId", "outcomeCohortId", "censorCohortId",
              "interval", "completeDatabaseIntervals", "outcomeWashout",
              "repeatedEvents", "strata", "includeOverallStrata")))
test_that("incidence: nothing is nested under an estimand wrapper", {
  expect_null(inc$json$estimand)
  expect_false("estimand" %in% names(inc$json))
})
test_that("incidence: no snake_case parameter survives",
          expect_false(any(grepl("_", setdiff(names(inc$json), "")))))
test_that("incidence: the washout is a flat outcomeWashout, read back to the field", {
  expect_identical(washout_days(inc$json$outcomeWashout), 365)
  expect_identical(inc$pf("outcomeWashout"), 365)
})
test_that("incidence: a ticked checkbox is true, not null",
          expect_identical(inc$json$repeatedEvents, TRUE))
test_that("incidence: a multi-select interval stays an array",
          expect_identical(unlist(inc$json$interval), c("years", "overall")))
test_that("incidence: prefiller recovers a picker value",
          expect_identical(inc$pf("denominatorTable"), "Metformin new users"))

# The three *CohortId arguments select which cohorts of a set to use; null is the
# estimator's own default of "all". Unset, they serialise as null -- present and
# faithful, not omitted.
test_that("incidence: the three cohort-id arguments are present and null by default", {
  for (k in c("denominatorCohortId", "outcomeCohortId", "censorCohortId")) {
    expect_true(k %in% names(inc$json), info = k)
    expect_null(inc$json[[k]], info = k)
  }
})
test_that("incidence: explicit cohort ids serialise as a numeric array", {
  ids <- round_trip(ANALYSIS_TEMPLATES[["Incidence"]],
                    list(denominatorTable = "D", denominatorCohortId = c("1", "2")))
  expect_identical(as.numeric(unlist(ids$json$denominatorCohortId)), c(1, 2))
})

# `parameters` takes nothing estimateIncidence() does not: rate-per-N and the
# denominator unit are presentation choices made downstream, and a sensitivity
# analysis is a second call, not an argument to this one.
test_that("incidence: no reporting-only fields leak into parameters",
          expect_false(any(c("reporting", "denominator_unit", "rate_multiplier",
                             "sensitivity_analyses", "stratifications",
                             "time_at_risk") %in% names(inc$json))))

# strata is a list of variable GROUPS: list("sex", c("sex","age_group")) means one
# stratification by sex and another by the cross of the two. A comma in a token
# crosses its variables.
test_that("strata: each token is one group", expect_length(inc$json$strata, 2))
test_that("strata: a plain token is a one-variable group",
          expect_identical(unlist(inc$json$strata[[1]]), "sex"))
test_that("strata: a comma crosses the variables in one group",
          expect_identical(unlist(inc$json$strata[[2]]), c("sex", "age_group")))
test_that("strata: a single group still serialises as a list of arrays",
          expect_true(is.list(parse_strata("sex")[[1]]) || length(parse_strata("sex")[[1]]) == 1))
test_that("strata: tokens round-trip back into the multi-select",
          expect_identical(inc$pf("strata"), c("sex", "sex, age_group")))
test_that("strata: includeOverallStrata round-trips back to the checkbox input",
          expect_true(inc$pf("include_overall_strata")))
test_that("strata: no strata is an empty list, which is estimateIncidence()'s default",
          expect_identical(parse_strata(character(0)), list()))

# estimateIncidence(outcomeWashout =) is a NUMBER of days, defaulting to Inf. But
# JSON has no Infinity, and three states have to stay apart: unset, a number of
# days (including 0, a substantively different analysis from an unstated one), and
# Inf. So the washout is a one-element numeric array -- [365], [0], [null] for Inf
# -- while a bare null keeps its schema-wide meaning of "the author never said".
#
# The form captures it as a NUMBER FIELD plus an "unbounded" checkbox: a number
# field cannot hold Inf, and blanking it already means "never stated". The two
# inputs together carry the three states, and the checkbox wins.
test_that("washout: a blank number field is unset, not a value", {
  expect_null(parse_washout(NULL))
  expect_null(parse_washout(NA))            # exactly what a cleared numericInput sends
  expect_null(parse_washout(""))
})
test_that("washout: a typed number of days parses to a numeric array",
          expect_identical(as.numeric(parse_washout(365)), 365))
test_that("washout: any number is allowed, not just the old preset menu",
          expect_identical(washout_days(parse_washout(180)), 180))
test_that("washout: 0 days is a value, not an absence",
          expect_identical(washout_days(parse_washout(0)), 0))
test_that("washout: the unbounded checkbox is Inf",
          expect_true(is.infinite(washout_days(parse_washout(NA, TRUE)))))
test_that("washout: the checkbox wins over whatever the number field still holds",
          expect_true(is.infinite(washout_days(parse_washout(30, TRUE)))))
test_that("washout: junk is not guessed at", expect_null(parse_washout("about a year")))
test_that("washout: a negative washout is rejected", expect_null(parse_washout(-30)))

# The encoding, end to end. A bare Inf really would be lost -- that is why the
# value is wrapped in an array, where a null means Inf rather than "absent".
test_that("washout: a bare Inf WOULD be lost by the JSON contract",
          expect_null(fromJSON(as.character(sap_json(list(w = Inf))), simplifyVector = FALSE)$w))
test_that("washout: unbounded serialises as [null], not null",
          expect_match(as.character(sap_json(list(w = parse_washout(NA, TRUE)))),
                       '"w": [null]', fixed = TRUE))
test_that("washout: a finite washout serialises as a one-element numeric array",
          expect_match(as.character(sap_json(list(w = parse_washout(365)))),
                       '"w": [365]', fixed = TRUE))

w_json <- fromJSON(as.character(sap_json(list(w = parse_washout(NA, TRUE)))),
                   simplifyVector = FALSE)$w
test_that("washout: [null] reads back as a length-1 list, not an empty one", {
  expect_length(w_json, 1)
  expect_null(w_json[[1]])
})
test_that("washout: washout_days() resolves the null back to Inf",
          expect_true(is.infinite(washout_days(w_json))))
test_that("washout: unset stays distinguishable from unbounded after a round trip", {
  expect_null(washout_days(NULL))
  expect_false(is.null(washout_days(w_json)))
})

INC_T <- ANALYSIS_TEMPLATES[["Incidence"]]

test_that("washout: unbounded survives the full round trip", {
  unb <- round_trip(INC_T, list(outcomeWashout = NA, outcomeWashout_unbounded = TRUE))
  expect_true(washout_is_unbounded(unb$json$outcomeWashout))
  # A number field cannot show Inf, so it comes back blank with the box ticked.
  expect_null(unb$pf("outcomeWashout", NULL))
  expect_true(unb$pf("outcomeWashout_unbounded"))
})
test_that("washout: a finite washout round-trips back into the number field", {
  fin <- round_trip(INC_T, list(outcomeWashout = 365, outcomeWashout_unbounded = FALSE))
  expect_identical(fin$pf("outcomeWashout"), 365)
  expect_false(fin$pf("outcomeWashout_unbounded"))
})
test_that("washout: a 0-day washout round-trips and does not become 'unset'", {
  zero <- round_trip(INC_T, list(outcomeWashout = 0, outcomeWashout_unbounded = FALSE))
  expect_identical(zero$pf("outcomeWashout"), 0)
  expect_identical(washout_days(zero$json$outcomeWashout), 0)
})
test_that("washout: an unset washout leaves both inputs blank and stays null", {
  none <- round_trip(INC_T, list(outcomeWashout = NA, outcomeWashout_unbounded = FALSE))
  expect_null(none$json$outcomeWashout)
  expect_null(none$pf("outcomeWashout", NULL))
  expect_false(none$pf("outcomeWashout_unbounded"))
})
