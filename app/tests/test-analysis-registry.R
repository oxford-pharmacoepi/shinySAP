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
test_that("resolver: serialised estimator names resolve to the Prevalence template", {
  expect_identical(canonical_analysis_type("estimatePointPrevalence"), "Prevalence")
  expect_identical(canonical_analysis_type("estimatePeriodPrevalence"), "Prevalence")
})
test_that("resolver: a known type is left alone",
          expect_identical(canonical_analysis_type("Prevalence"), "Prevalence"))

test_that("template: NULL resolves without error",
          expect_identical(analysis_template(NULL), ANALYSIS_TEMPLATES[["Other"]]))
test_that("template: a serialised estimator name gets the Prevalence template",
          expect_identical(analysis_template("estimatePeriodPrevalence"),
                           ANALYSIS_TEMPLATES[["Prevalence"]]))
test_that("template: an unknown type falls back to Other",
          expect_identical(analysis_template("Bayesian hierarchical model"),
                           ANALYSIS_TEMPLATES[["Other"]]))
test_that("template: a type with no entry falls back to Other",
          expect_identical(analysis_template("Case-control"), ANALYSIS_TEMPLATES[["Other"]]))

# The dropdown offers the two estimators the app generates code for, and the
# generic form. Anything else would name a study it has no estimator for.
test_that("types: only the two estimators and the generic form are offered",
          expect_identical(ANALYSIS_TYPES, c("Incidence", "Prevalence", "Other")))

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

# Role: primary or sensitivity -------------------------------------------------
#
# Not an estimator argument -- it is the fact a reviewer reads first, and the one
# the analysis NAME used to have to carry.

test_that("role: unset stays unset, never a silent primary", {
  expect_identical(canonical_analysis_role(NULL), "")
  expect_identical(canonical_analysis_role(NA), "")
  expect_identical(canonical_analysis_role(""), "")
})
# A role the vocabulary does not know comes back as itself. Coercing "main" to
# "primary" would let a hand-edited file read as a decision nobody made.
test_that("role: an unknown role is reported, not coerced", {
  expect_identical(canonical_analysis_role("main"), "main")
  msgs <- unlist(lapply(analysis_role_problems(
    list(list(name = "A", role = "main"))), function(p) p$messages))
  expect_true(any(grepl("not a role this app knows", msgs)))
})

roled <- function(...) lapply(c(...), function(r) list(name = r, role = r))

test_that("role: a plan that states none says nothing", {
  expect_length(analysis_role_problems(list(list(name = "A"))), 0)
  expect_length(analysis_role_problems(list()), 0)
})
test_that("role: exactly one primary is clean",
          expect_length(analysis_role_problems(
            roled("primary", "sensitivity", "sensitivity")), 0))
test_that("role: all sensitivity leaves nothing to conclude from", {
  msgs <- unlist(lapply(analysis_role_problems(roled("sensitivity", "sensitivity")),
                        function(p) p$messages))
  expect_true(any(grepl("No analysis is marked primary", msgs)))
})
test_that("role: two primaries is a warning, not a rule", {
  msgs <- unlist(lapply(analysis_role_problems(roled("primary", "primary")),
                        function(p) p$messages))
  expect_true(any(grepl("co-primary", msgs)))
})

test_that("analysis: the card collects its role", {
  testServer(analyses_server, {
    session$setInputs(add = 1)
    session$setInputs("analysis_1-name" = "PP", "analysis_1-role" = "primary")
    expect_identical(isolate(items$data())[[1]]$data$role, "primary")
  })
})
# The mirror, in the direction that broke `objectives` before it: a field the
# common half of the card owns must come back out of a saved file, or load()
# renders it empty and the next autosave writes that emptiness over the plan.
test_that("analysis: role survives the trip back into a card's prefill",
          expect_identical(
            analysis_to_prefill(list(name = "PP", analysis_type = "Prevalence",
                                     role = "primary"))$role,
            "primary"))

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

# The outcome / censoring role hint. Deliberately quiet: it speaks only on a
# DEFINITE crossing, because a plain cohort in either slot is ordinary and a
# cohort that is genuinely both (death as one analysis's outcome and the next
# one's censoring event) must not be nagged into a false correction.
has_note <- function(...) !is.null(cohort_role_notes(...))
role_cohort <- function(name, kind) list(name = name, kind = kind)

test_that("role notes: correctly-kinded slots say nothing", {
  expect_false(has_note(role_cohort("MI", "outcome"), role_cohort("Death", "censor")))
})
test_that("role notes: a censoring cohort in the outcome slot is flagged", {
  note <- as.character(cohort_role_notes(role_cohort("Death", "censor"), NULL))
  expect_true(grepl("'Death' is the outcome here", note, fixed = TRUE))
})
test_that("role notes: an outcome cohort in the censoring slot is flagged", {
  note <- as.character(cohort_role_notes(NULL, role_cohort("MI", "outcome")))
  expect_true(grepl("'MI' is the censoring cohort here", note, fixed = TRUE))
})
# The false positives this block exists to avoid.
test_that("role notes: a plain or kindless cohort in either slot says nothing", {
  for (k in c("other", "target", "")) {
    expect_false(has_note(role_cohort("X", k), NULL))
    expect_false(has_note(NULL, role_cohort("X", k)))
  }
})
test_that("role notes: nothing picked says nothing", expect_false(has_note(NULL, NULL)))
# A generated denominator in either slot is unambiguously wrong, so it belongs to
# validate() -- which BLOCKS -- and this hint must not duplicate it.
test_that("role notes: a denominator in either slot is left to validate()", {
  for (k in c("denominator", "target_denominator")) {
    expect_false(has_note(role_cohort("D", k), NULL))
    expect_false(has_note(NULL, role_cohort("D", k)))
  }
  errs <- ANALYSIS_TEMPLATES[["Incidence"]]$validate(
    list(outcomeTable = "D"), list(D = role_cohort("D", "denominator")))
  expect_true(any(grepl("Outcome must be a plain cohort", errs, fixed = TRUE)))
})
