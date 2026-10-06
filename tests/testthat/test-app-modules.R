skip_without_app()

# Every section mutates ONE shinySAP `sap` object, only through the package's
# CRUD. The tests hold that object in a reactiveVal, exactly as app.R does, and
# read it back with the package's own accessors.
draft_sap <- function() shinySAP::createSap(shinySAP::newSapStudy(validate = FALSE), validate = FALSE)
ids_of <- function(rv, collection) shinySAP::sapComponentIds(rv(), collection)

test_that("the study card writes a draft sap_study through updateStudy()", {
  rv <- shiny::reactiveVal(draft_sap())
  shiny::testServer(app("study_server"), args = list(id = "study", sap = rv), {
    session$setInputs(study_id = "C1-001", title = "A study", authors = "One, Two", version = "v1.0.0",
                      description = "")
    study <- rv()$study
    expect_s3_class(study, "sap_study")
    expect_identical(study$study_id, "C1-001")
    expect_identical(study$authors, c("One", "Two"))
    expect_null(study$description)
    # A cleared field is absent, and the draft is reported, never refused.
    session$setInputs(title = "")
    expect_null(rv()$study$title)
    expect_true("study.title" %in% vapply(shinySAP::checkSap(rv()), function(p) p$path, character(1)))
    # refresh() pushes the SAP's study back into the inputs after a load.
    rv(shinySAP::createSap(shinySAP::newSapStudy("X-1", "Loaded", validate = FALSE), validate = FALSE))
    expect_no_error(session$returned$refresh())
  })
})

test_that("data sources are minted by newSapId() and added through addSapComponent()", {
  rv <- shiny::reactiveVal(draft_sap())
  shiny::testServer(app("data_sources_server"), args = list(id = "sources", sap = rv), {
    session$setInputs(add = 1)
    session$setInputs(add = 2)
    expect_identical(ids_of(rv, "data_sources"), c("ds_1", "ds_2"))
    session$setInputs(`source_1-name` = "CPRD GOLD")
    ds <- shinySAP::getSapComponent(rv(), "data_sources", "ds_1")
    expect_s3_class(ds, "sap_data_source")
    expect_identical(ds$name, "CPRD GOLD")
    expect_null(shinySAP::getSapComponent(rv(), "data_sources", "ds_2")$name)
    expect_identical(app("item_choices")(rv()$data_sources), c("CPRD GOLD" = "ds_1", "ds_2" = "ds_2"))

    # Remove goes through removeSapComponent(); a removed id is never reissued.
    session$setInputs(`source_2-remove` = 1)
    expect_identical(ids_of(rv, "data_sources"), "ds_1")
    session$setInputs(add = 3)
    expect_identical(ids_of(rv, "data_sources"), c("ds_1", "ds_3"))

    # Undo re-adds the very component that was removed.
    removed <- shinySAP::getSapComponent(rv(), "data_sources", "ds_3")
    session$setInputs(`source_3-remove` = 1)
    session$setInputs(source_undo = 1)
    expect_identical(shinySAP::getSapComponent(rv(), "data_sources", "ds_3"), removed)

    # Duplicate copies the component under a fresh id through the constructor.
    session$setInputs(`source_1-duplicate` = 1)
    copy <- shinySAP::getSapComponent(rv(), "data_sources", "ds_4")
    expect_s3_class(copy, "sap_data_source")
    expect_identical(copy$name, "CPRD GOLD (copy)")

    # A load swaps the object; cards are rebuilt and ids count on from the highest.
    session$returned$reset()
    rv(shinySAP::createSap(shinySAP::newSapStudy(validate = FALSE),
                           dataSources = list(shinySAP::newSapDataSource("ds_7", "SIDIAP")), validate = FALSE))
    session$flushReact()
    session$setInputs(add = 4)
    expect_identical(ids_of(rv, "data_sources"), c("ds_7", "ds_8"))
    expect_identical(shinySAP::getSapComponent(rv(), "data_sources", "ds_7")$name, "SIDIAP")

    # A component removed from one plan cannot be undone into the next.
    session$setInputs(`source_7-remove` = 1)
    expect_identical(ids_of(rv, "data_sources"), "ds_7")
    session$returned$reset()
    rv(draft_sap())
    session$flushReact()
    session$setInputs(source_undo = 2)
    expect_identical(ids_of(rv, "data_sources"), character(0))
  })
})

test_that("drawing the cards of a loaded plan leaves the plan untouched", {
  loaded <- fixture_sap()
  sections <- list(
    data_sources_server = list(), data_source_modifications_server = list(),
    codelists_server = list(), cohorts_server = list(), analyses_server = list()
  )
  for (server in names(sections)) {
    rv <- shiny::reactiveVal(loaded)
    shiny::testServer(app(server), args = list(id = "x", sap = rv), {
      session$flushReact()
    })
    expect_identical(shiny::isolate(rv()), loaded, info = server)
  }
})

test_that("a codelist card writes a draft sap_codelist and imports uploads by name", {
  upload <- withr::local_tempdir()
  omopgenerics::exportCodelist(omopgenerics::newCodelist(list(diabetes = c(201826L, 443238L))),
                               path = upload, type = "json")
  rv <- shiny::reactiveVal(draft_sap())
  shiny::testServer(app("codelists_server"), args = list(id = "codelists", sap = rv), {
    session$setInputs(add = 1)
    session$setInputs(`codelist_1-name` = "Diabetes", `codelist_1-type` = "codelist")
    cl <- shinySAP::getSapComponent(rv(), "codelists", "cl_1")
    expect_s3_class(cl, "sap_codelist")
    expect_identical(unclass(cl)[c("id", "name", "type")], list(id = "cl_1", name = "Diabetes", type = "codelist"))
    expect_null(cl$content)
    session$setInputs(`codelist_1-upload` = list(name = "diabetes.json",
                                                datapath = file.path(upload, "diabetes.json")))
    cl <- shinySAP::getSapComponent(rv(), "codelists", "cl_1")
    expect_s3_class(cl$content, "codelist")
    expect_identical(names(cl$content), "diabetes")
    expect_length(shinySAP::checkSap(rv())[vapply(shinySAP::checkSap(rv()), function(p) startsWith(p$path, "codelists"),
                                                 logical(1))], 0)
  })
})

test_that("a cohort card writes schema-shaped draft parameters", {
  rv <- shiny::reactiveVal(draft_sap())
  shiny::testServer(app("cohorts_server"), args = list(id = "cohorts", sap = rv), {
    session$setInputs(add = 1)
    session$setInputs(`cohort_1-name` = "General population", `cohort_1-type` = "denominator",
                      `cohort_1-data_source_id` = c("ds_1", "ds_2"),
                      `cohort_1-age_group` = "0, 17\n18, 150", `cohort_1-sex` = "Both",
                      `cohort_1-days_prior_observation` = 365, `cohort_1-requirement_interactions` = TRUE)
    co <- shinySAP::getSapComponent(rv(), "cohorts", "coh_1")
    expect_s3_class(co, "sap_cohort")
    expect_identical(co$type, "denominator")
    expect_identical(co$data_source_id, c("ds_1", "ds_2"))
    expect_identical(co$parameters,
                     list(age_group = list(c(0, 17), c(18, 150)), sex = "Both",
                          days_prior_observation = 365, requirement_interactions = TRUE))
    # A type switch collects only the new type's ids.
    session$setInputs(`cohort_1-type` = "concept_cohort", `cohort_1-codelist_id` = "cl_1",
                      `cohort_1-exit` = "event_end_date", `cohort_1-overlap` = "merge")
    expect_identical(shinySAP::getSapComponent(rv(), "cohorts", "coh_1")$parameters,
                     list(codelist_id = "cl_1", exit = "event_end_date", overlap = "merge"))
    # Duplicate keeps the parameters under a new id.
    session$setInputs(`cohort_1-duplicate` = 1)
    copy <- shinySAP::getSapComponent(rv(), "cohorts", "coh_2")
    expect_identical(copy$name, "General population (copy)")
    expect_identical(copy$parameters, list(codelist_id = "cl_1", exit = "event_end_date", overlap = "merge"))
  })
})

test_that("a loaded cohort keeps its parameters until its own inputs report", {
  loaded <- shinySAP::createSap(
    shinySAP::newSapStudy(validate = FALSE),
    cohorts = list(shinySAP::newSapCohort("coh_9", "Loaded", "ds_1", "denominator",
                                          list(age_group = list(c(0, 150)), sex = "Both",
                                               days_prior_observation = 0, requirement_interactions = TRUE))),
    validate = FALSE)
  rv <- shiny::reactiveVal(loaded)
  shiny::testServer(app("cohorts_server"), args = list(id = "cohorts", sap = rv), {
    session$flushReact()
    # The card exists, no input has reported: the SAP still holds what was loaded.
    expect_identical(shinySAP::getSapComponent(rv(), "cohorts", "coh_9")$parameters$age_group, list(c(0, 150)))
    expect_identical(shinySAP::getSapComponent(rv(), "cohorts", "coh_9")$name, "Loaded")
    # Once the block reports, the inputs are the truth.
    session$setInputs(`cohort_1-sex` = "Female", `cohort_1-age_group` = "0, 150",
                      `cohort_1-days_prior_observation` = 0, `cohort_1-requirement_interactions` = TRUE)
    expect_identical(shinySAP::getSapComponent(rv(), "cohorts", "coh_9")$parameters$sex, "Female")
  })
})

test_that("an analysis card with no type collects no parameters", {
  rv <- shiny::reactiveVal(draft_sap())
  shiny::testServer(app("analyses_server"), args = list(id = "analyses", sap = rv), {
    session$setInputs(add = 1)
    session$setInputs(`analysis_1-name` = "Something")
    a <- shinySAP::getSapComponent(rv(), "analyses", "an_1")
    expect_s3_class(a, "sap_analysis")
    expect_identical(a$name, "Something")
    expect_null(a$type)
    expect_identical(a$parameters, list())
    session$setInputs(`analysis_1-type` = "incidence", `analysis_1-denominator_cohort_id` = "coh_2",
                      `analysis_1-washout` = NA, `analysis_1-washout_unbounded` = TRUE,
                      `analysis_1-interval` = "years")
    p <- shinySAP::getSapComponent(rv(), "analyses", "an_1")$parameters
    expect_identical(p$denominator_cohort_id, "coh_2")
    expect_identical(p$washout, Inf)
    expect_identical(p$interval, "years")
    codes <- vapply(shinySAP::checkSap(rv()), function(x) x$code, character(1))
    expect_true("missing_reference" %in% codes)   # coh_2 is not defined
  })
})
