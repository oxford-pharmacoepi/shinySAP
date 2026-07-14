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
          expect_false("time_at_risk" %in%
                         names(COHORT_TEMPLATES[["denominator"]]$collect(list()))))
test_that("cohort: a target denominator does have a time at risk",
          expect_true("time_at_risk" %in%
                        names(COHORT_TEMPLATES[["target_denominator"]]$collect(list()))))
test_that("cohort: an outcome cohort carries none of the generator arguments",
          expect_length(intersect(names(COHORT_TEMPLATES[["other"]]$collect(list())),
                                  c("age_groups", "sex", "time_at_risk",
                                    "days_prior_observation")), 0))
test_that("cohort: the target denominator's block is exactly the generator's extra args",
          expect_true(all(c("target_cohort", "time_at_risk", "requirements_at_entry") %in%
                            names(COHORT_TEMPLATES[["target_denominator"]]$collect(list())))))

# Cohort validators -------------------------------------------------------------

TD <- COHORT_TEMPLATES[["target_denominator"]]
td_ok <- list(name = "TD", kind = "target_denominator", target_cohort = "Metformin new users",
              time_at_risk = list(c(0, 30)), sex = list("Both"), age_groups = list(c(0, 150)))

test_that("cohort validate: a well-formed target denominator has no problems",
          expect_length(TD$validate(td_ok, cohorts_idx), 0))
test_that("cohort validate: a target denominator must name its target cohort",
          expect_true(any(grepl("must name the target cohort",
                                TD$validate(within(td_ok, target_cohort <- NA), cohorts_idx)))))
test_that("cohort validate: the target cannot be another denominator",
          expect_true(any(grepl("not another denominator",
                                TD$validate(within(td_ok, target_cohort <- "Men only"),
                                            cohorts_idx)))))
test_that("cohort validate: time at risk cannot end before it starts",
          expect_true(any(grepl("ends before it starts",
                                TD$validate(within(td_ok, time_at_risk <- list(c(30, 0))),
                                            cohorts_idx)))))
test_that("cohort validate: time at risk must have at least one interval",
          expect_true(any(grepl("at least one interval",
                                TD$validate(within(td_ok, time_at_risk <- list()),
                                            cohorts_idx)))))
