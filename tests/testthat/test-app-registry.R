skip_without_app()

test_that("every schema type has a template", {
  expect_true(app("assert_templates_cover_schema")())
  for (type in sapSchemaTypes("analysis")) {
    expect_false(identical(app("analysis_template")(type), app("EMPTY_ANALYSIS_TEMPLATE")), info = type)
  }
  for (type in sapSchemaTypes("cohort")) {
    expect_false(identical(app("cohort_template")(type), app("EMPTY_COHORT_TEMPLATE")), info = type)
  }
})

test_that("templates render unique input ids that avoid the common half", {
  ids_of <- app("template_field_ids")
  for (type in sapSchemaTypes("analysis")) {
    ids <- ids_of(app("analysis_template")(type))
    expect_false(any(duplicated(ids)), info = type)
    expect_length(intersect(ids, app("RESERVED_INPUT_IDS")), 0)
  }
  for (type in sapSchemaTypes("cohort")) {
    ids <- ids_of(app("cohort_template")(type))
    expect_false(any(duplicated(ids)), info = type)
    expect_length(intersect(ids, app("COHORT_COMMON_FIELDS")), 0)
  }
})

test_that("the remembered input ids are each template's own", {
  # Point/period prevalence and the two survival types come from one factory
  # each: same source text, different fields.
  display_only <- c(app("DISPLAY_ONLY_IDS"), app("COHORT_DISPLAY_ONLY_IDS"))
  templates <- c(app("ANALYSIS_TEMPLATES"), app("COHORT_TEMPLATES"), app("MODIFICATION_TEMPLATES"))
  for (pass in 1:2) {
    for (type in names(templates)) {
      expect_setequal(app("template_input_ids")(templates[[type]]),
                      setdiff(app("template_field_ids")(templates[[type]]), display_only))
    }
  }
  ids_of <- function(type) app("template_input_ids")(app("analysis_template")(type))
  expect_true("level" %in% ids_of("period_prevalence"))
  expect_false("level" %in% ids_of("point_prevalence"))
  expect_true("competing_outcome_cohort_id" %in% ids_of("competing_risk_survival"))
  expect_false("competing_outcome_cohort_id" %in% ids_of("single_event_survival"))
})

test_that("an unknown type falls back to an empty template that keeps nothing", {
  tmpl <- app("analysis_template")("not_a_type")
  expect_identical(tmpl$collect(list(a = 1)), list())
  expect_identical(app("cohort_template")("")$flatten(list(x = 1)), list())
})

test_that("unbounded day helpers encode three states", {
  parse <- app("parse_unbounded_days")
  expect_null(parse(NULL, FALSE))
  expect_null(parse("", FALSE))
  expect_identical(parse(365, FALSE), 365)
  expect_identical(parse(NA, TRUE), Inf)
  expect_null(parse(-1, FALSE))
  pf <- app("unbounded_days_prefill")(list(washout = Inf), "washout")
  expect_true(pf$washout_unbounded)
  expect_null(pf[["washout"]])
  pf <- app("unbounded_days_prefill")(list(washout = 30), "washout")
  expect_false(pf$washout_unbounded)
  expect_identical(pf[["washout"]], 30)
})

test_that("strata tokens and groups mirror each other", {
  groups <- list("sex", c("sex", "age_group"))
  toks <- app("strata_tokens")(groups)
  expect_identical(toks, c("sex", "sex, age_group"))
  expect_identical(app("parse_strata")(toks), groups)
  expect_null(app("parse_strata")(character(0)))
})
