skip_without_app()

test_that("bounds parse and format both ways", {
  parse <- app("parse_bounds")
  expect_identical(parse("0, 17"), c(0, 17))
  expect_identical(parse("18-64"), c(18, 64))
  expect_identical(parse("65+"), c(65, Inf))
  expect_identical(parse("0, Inf"), c(0, Inf))
  expect_null(parse("abc"))
  expect_null(parse(""))
  lines <- "0, 17\n18, 64\n\n65+"
  pairs <- app("parse_bound_list")(lines)
  expect_identical(pairs, list(c(0, 17), c(18, 64), c(65, Inf)))
  expect_identical(app("format_bound_list")(pairs), c("0, 17", "18, 64", "65, Inf"))
  expect_identical(app("format_bound_list")(pairs, open = 150), c("0, 17", "18, 64", "65, 150"))
  expect_null(app("parse_bound_list")(""))
})

test_that("cohort templates collect what they flatten, for every fixture cohort", {
  sap <- fixture_sap()
  for (co in sap$cohorts) {
    tmpl <- app("cohort_template")(co$type)
    pf <- app("cohort_to_prefill")(co)
    ids <- app("template_field_ids")(tmpl)
    extra <- setdiff(setdiff(names(pf), app("COHORT_COMMON_FIELDS")), c(ids, app("COHORT_DISPLAY_ONLY_IDS")))
    expect_length(extra, 0)
    back <- tmpl$collect(pf)
    expect_setequal(nm(back), nm(co$parameters))
    if (length(co$parameters)) expect_equal(back[nm(co$parameters)], co$parameters, info = co$type)
    else expect_length(back, 0)
    allowed <- sub("^parameters\\.", "", grep("^parameters\\.", sapSchemaFields("cohort", co$type)$path, value = TRUE))
    expect_true(all(names(back) %in% allowed), info = co$type)
  }
})

test_that("a denominator cohort set multiplies over age groups and time at risk", {
  set <- app("denominator_cohort_set")
  plain <- list(type = "denominator", parameters = list(age_group = list(c(0, 17), c(18, 150)), sex = "Both"))
  expect_length(set(plain), 2)
  target <- list(type = "target_denominator",
                 parameters = list(age_group = list(c(0, 17), c(18, 150)),
                                   time_at_risk = list(c(0, 30), c(31, Inf))))
  expect_length(set(target), 4)
  expect_identical(set(target)[[1]]$time_at_risk, c(0, 30))
  # Unset fields use the generator's defaults.
  expect_length(set(list(type = "denominator", parameters = list())), 1)
  expect_match(app("format_denominator_cohort")(set(plain)[[1]]), "Age 0, 17 | Both | 0 days", fixed = TRUE)
})

test_that("cohort picker choices are grouped by type and keyed by id", {
  index <- list(coh_1 = list(name = "Diabetes", type = "concept_cohort"),
                coh_2 = list(name = "Denominator", type = "denominator"),
                coh_3 = list(name = "No type", type = NULL),
                coh_4 = list(name = NULL, type = "concept_cohort"))
  choices <- app("grouped_cohort_choices")(index)
  expect_identical(unname(choices[["Concept cohort"]]), c("coh_1", "coh_4"))
  expect_identical(names(choices[["Concept cohort"]]), c("Diabetes", "coh_4"))
  expect_identical(unname(choices[["Denominator"]]), "coh_2")
  expect_identical(unname(choices[["No type chosen"]]), "coh_3")
  expect_identical(app("grouped_cohort_choices")(list()), list())
})
