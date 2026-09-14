skip_without_app()

test_that("the study module emits exactly the schema's study fields", {
  shiny::testServer(app("study_server"), args = list(id = "study"), {
    session$setInputs(study_id = "C1-001", title = "A study", authors = "One, Two", version = "v1.0.0",
                      description = "")
    d <- session$returned$data()
    expect_identical(d, list(study_id = "C1-001", title = "A study", authors = c("One", "Two"),
                             version = "v1.0.0"))
  })
})

test_that("data sources are minted ids and offered as name -> id choices", {
  shiny::testServer(app("data_sources_server"), args = list(id = "sources"), {
    session$setInputs(add = 1)
    session$setInputs(add = 2)
    session$setInputs(`source_1-name` = "CPRD GOLD")
    d <- session$returned$data()
    expect_identical(vapply(d, function(x) x$id, character(1)), c("ds_1", "ds_2"))
    expect_identical(d[[1]]$name, "CPRD GOLD")
    expect_null(d[[2]]$name)
    expect_identical(session$returned$choices(), c("CPRD GOLD" = "ds_1", "ds_2" = "ds_2"))
    # A loaded item keeps its id and later additions count on from the highest.
    session$returned$load(list(list(id = "ds_7", name = "SIDIAP")))
    session$setInputs(add = 3)
    expect_identical(vapply(session$returned$data(), function(x) x$id, character(1)), c("ds_7", "ds_8"))
  })
})

test_that("a codelist card emits id, name, type and content, and re-imports uploads by name", {
  upload <- withr::local_tempdir()
  omopgenerics::exportCodelist(omopgenerics::newCodelist(list(diabetes = c(201826L, 443238L))),
                               path = upload, type = "json")
  shiny::testServer(app("codelists_server"), args = list(id = "codelists"), {
    session$setInputs(add = 1)
    session$setInputs(`codelist_1-name` = "Diabetes", `codelist_1-type` = "codelist")
    d <- session$returned$data()
    expect_identical(d[[1]], list(id = "cl_1", name = "Diabetes", type = "codelist"))
    session$setInputs(`codelist_1-upload` = list(name = "diabetes.json",
                                                datapath = file.path(upload, "diabetes.json")))
    d <- session$returned$data()
    expect_s3_class(d[[1]]$content, "codelist")
    expect_identical(names(d[[1]]$content), "diabetes")
    expect_identical(session$returned$choices(), c("Diabetes" = "cl_1"))
  })
})

test_that("a cohort card emits schema-shaped parameters and the section indexes by id", {
  shiny::testServer(app("cohorts_server"), args = list(id = "cohorts"), {
    session$setInputs(add = 1)
    session$setInputs(`cohort_1-name` = "General population", `cohort_1-type` = "denominator",
                      `cohort_1-data_source_id` = c("ds_1", "ds_2"),
                      `cohort_1-age_group` = "0, 17\n18, 150", `cohort_1-sex` = "Both",
                      `cohort_1-days_prior_observation` = 365, `cohort_1-requirement_interactions` = TRUE)
    d <- session$returned$data()
    expect_identical(d[[1]]$id, "coh_1")
    expect_identical(d[[1]]$type, "denominator")
    expect_identical(d[[1]]$data_source_id, c("ds_1", "ds_2"))
    expect_identical(d[[1]]$parameters,
                     list(age_group = list(c(0, 17), c(18, 150)), sex = "Both",
                          days_prior_observation = 365, requirement_interactions = TRUE))
    expect_identical(names(session$returned$by_id()), "coh_1")
    # A type switch collects only the new type's ids: the denominator fields
    # stay in the input store but never reach the SAP.
    session$setInputs(`cohort_1-type` = "concept_cohort", `cohort_1-codelist_id` = "cl_1",
                      `cohort_1-exit` = "event_end_date", `cohort_1-overlap` = "merge")
    expect_identical(session$returned$data()[[1]]$parameters,
                     list(codelist_id = "cl_1", exit = "event_end_date", overlap = "merge"))
  })
})

test_that("an analysis card with no type collects no parameters", {
  shiny::testServer(app("analyses_server"), args = list(id = "analyses"), {
    session$setInputs(add = 1)
    session$setInputs(`analysis_1-name` = "Something")
    d <- session$returned$data()
    expect_identical(d[[1]], list(id = "an_1", name = "Something", parameters = list()))
    session$setInputs(`analysis_1-type` = "incidence", `analysis_1-denominator_cohort_id` = "coh_2",
                      `analysis_1-washout` = NA, `analysis_1-washout_unbounded` = TRUE,
                      `analysis_1-interval` = "years")
    p <- session$returned$data()[[1]]$parameters
    expect_identical(p$denominator_cohort_id, "coh_2")
    expect_identical(p$washout, Inf)
    expect_identical(p$interval, "years")
  })
})
