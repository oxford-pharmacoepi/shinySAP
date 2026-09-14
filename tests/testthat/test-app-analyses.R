skip_without_app()

# The mirror invariant: a saved analysis flattened to a prefill and collected back
# through its template is the same analysis. The prefill IS the fake input: input
# ids are the parameter keys.
test_that("analysis templates collect what they flatten, for every fixture analysis", {
  sap <- fixture_sap()
  for (a in sap$analyses) {
    tmpl <- app("analysis_template")(a$type)
    pf <- app("analysis_to_prefill")(a)
    ids <- app("template_field_ids")(tmpl)
    extra <- setdiff(setdiff(names(pf), app("ANALYSIS_COMMON_FIELDS")), c(ids, app("DISPLAY_ONLY_IDS")))
    expect_length(extra, 0)
    back <- tmpl$collect(pf)
    expect_setequal(nm(back), nm(a$parameters))
    if (length(a$parameters)) expect_equal(back[nm(a$parameters)], a$parameters, info = a$type)
    else expect_length(back, 0)
  }
})

test_that("collected parameter keys are exactly schema parameters", {
  sap <- fixture_sap()
  for (a in sap$analyses) {
    allowed <- sub("^parameters\\.", "", grep("^parameters\\.", sapSchemaFields("analysis", a$type)$path, value = TRUE))
    pf <- app("analysis_to_prefill")(a)
    expect_true(all(names(app("analysis_template")(a$type)$collect(pf)) %in% allowed), info = a$type)
  }
})

test_that("an incidence card with nothing chosen collects only its checkboxes' defaults", {
  p <- app("analysis_template")("incidence")$collect(list())
  expect_identical(p, list(complete_database_intervals = TRUE, repeated_events = FALSE,
                           include_overall_strata = TRUE))
  expect_identical(app("analysis_template")("other")$collect(list()), list())
})

test_that("survival flatten does not confuse censor_on_date with its _variable twin", {
  a <- list(type = "competing_risk_survival",
            parameters = list(censor_on_date_variable = "end_of_study", follow_up_days = Inf))
  pf <- app("analysis_to_prefill")(a)
  expect_null(pf[["censor_on_date"]])
  expect_identical(pf$censor_on_date_variable, "end_of_study")
  expect_true(pf$follow_up_days_unbounded)
})
