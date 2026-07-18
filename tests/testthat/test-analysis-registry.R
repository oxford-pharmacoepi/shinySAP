# Analysis type resolution.
#
# analysis_template() is called on whatever a saved file happens to hold, and
# load() has already cleared the section by then -- so nothing here may error.

# A new card starts with no type chosen -- unset is "", a state of its own,
# never a silent Incidence. Same rule as the cohort kind.
test_that("resolver: unset stays unset, never a silent default", {
  expect_identical(canonical_analysis_type(NULL), "")
  expect_identical(canonical_analysis_type(NA), "")
  expect_identical(canonical_analysis_type(""), "")
})
test_that("resolver: a renamed type is aliased",
          expect_identical(canonical_analysis_type("Incidence rate"), "Incidence"))
test_that("resolver: serialised estimator names resolve to the Prevalence template", {
  expect_identical(canonical_analysis_type("estimatePointPrevalence"), "Prevalence")
  expect_identical(canonical_analysis_type("estimatePeriodPrevalence"), "Prevalence")
})
test_that("resolver: a known type is left alone",
          expect_identical(canonical_analysis_type("Prevalence"), "Prevalence"))

test_that("template: NULL resolves without error",
          expect_identical(analysis_template(NULL), ANALYSIS_TEMPLATES[["Other"]]))
test_that("template: a renamed type gets the Incidence template",
          expect_identical(analysis_template("Incidence rate"), ANALYSIS_TEMPLATES[["Incidence"]]))
test_that("template: an unknown type falls back to Other",
          expect_identical(analysis_template("Bayesian hierarchical model"),
                           ANALYSIS_TEMPLATES[["Other"]]))
test_that("template: a type with no entry yet falls back to Other",
          expect_identical(analysis_template("Case-control"), ANALYSIS_TEMPLATES[["Other"]]))

# The card-level contract for the unset type: nothing collected, one problem.
test_that("analysis: a card with no type chosen collects nothing and is a problem", {
  testServer(analyses_server, {
    session$setInputs(add = 1)
    session$setInputs("analysis_1-name" = "Untyped")
    d <- isolate(data_r())[[1]]
    expect_true(is.na(d$analysis_type))
    expect_false("parameters" %in% names(d))
    msgs <- unlist(lapply(isolate(problems_r()), function(p) p$messages))
    expect_true(any(grepl("no type chosen", msgs)))
  })
})

test_that("Other is always present, it is the fallback",
          expect_true("Other" %in% names(ANALYSIS_TEMPLATES)))
test_that("every template key is an offered analysis type",
          expect_true(all(names(ANALYSIS_TEMPLATES) %in% ANALYSIS_TYPES)))

# Template input ids -----------------------------------------------------------

all_pickers <- unique(unlist(lapply(ANALYSIS_TEMPLATES, picker_ids)))

for (type in names(ANALYSIS_TEMPLATES)) {
  tmpl <- ANALYSIS_TEMPLATES[[type]]
  ids  <- template_field_ids(tmpl)     # also a smoke test that ui() runs at all
  pk   <- picker_ids(tmpl)

  test_that(sprintf("[%s] ui() renders at least one input", type),
            expect_gt(length(ids), 0))
  test_that(sprintf("[%s] no id collides with a common field", type),
            expect_length(intersect(ids, RESERVED_INPUT_IDS), 0))
  test_that(sprintf("[%s] every declared picker is actually rendered", type),
            expect_true(all(pk %in% ids)))
  # An id that sync_pickers() owns in one template must not be a plain input in
  # another: after a type switch the stale selectize string would be handed to
  # whatever widget the new template built for that id.
  test_that(sprintf("[%s] no id is a picker elsewhere but a plain input here", type),
            expect_length(setdiff(intersect(ids, all_pickers), pk), 0))
}

# THE MIRROR INVARIANT. Every input a template renders has to survive
# collect() -> JSON -> flatten() and be findable again by pf(), or it silently
# comes back blank when a saved SAP is loaded. This is what catches a collect()
# that nests a block into `estimand` or `reporting` and a flatten() that forgets
# to unpack it -- a bug no amount of reading the template will show you.
for (type in names(ANALYSIS_TEMPLATES)) {
  test_that(sprintf("[%s] every rendered input survives collect -> JSON -> flatten", type), {
    tmpl <- ANALYSIS_TEMPLATES[[type]]
    ids  <- setdiff(template_field_ids(tmpl), DISPLAY_ONLY_IDS)
    # A plausible non-empty value for every input; only presence is under test.
    faked <- stats::setNames(lapply(ids, function(i) "1"), ids)
    flat  <- round_trip(tmpl, faked)$flat
    expect_identical(setdiff(ids, names(flat)), character(0))   # failure prints what was lost
  })
}

# Nothing a template collects may collide with a common key, or c() in load()
# would silently prefer the common one.
for (type in names(ANALYSIS_TEMPLATES)) {
  test_that(sprintf("[%s] no collected key collides with a common field", type), {
    keys <- names(ANALYSIS_TEMPLATES[[type]]$collect(list()))
    expect_length(intersect(keys, ANALYSIS_COMMON_FIELDS), 0)
  })
}
