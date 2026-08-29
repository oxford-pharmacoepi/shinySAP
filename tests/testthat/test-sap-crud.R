mkSap <- function() {
  description <- structure(list(), class = "data_source_description")
  createSap(
    newSapStudy("study_001", "My study"),
    dataSources = list(newSapDataSource("ds_001", "CPRD GOLD", description)),
    codelists = list(newSapCodelist(
      "cl_001", "Acetaminophen", "codelist", structure(list(), class = "codelist")
    )),
    cohorts = list(newSapCohort(
      "coh_001", "Cohort", "ds_001", "concept_cohort",
      list(codelist_id = "cl_001", exit = "e", overlap = "o")
    )),
    analyses = list(newSapAnalysis(
      "an_001", "Incidence", "ds_001", "incidence",
      list(denominator_cohort_id = "coh_001")
    ))
  )
}

test_that("get and ids read what add wrote", {
  sap <- mkSap()
  expect_identical(sapComponentIds(sap, "cohorts"), "coh_001")
  expect_identical(getSapComponent(sap, "cohorts", "coh_001")$name, "Cohort")
  expect_error(getSapComponent(sap, "cohorts", "nope"), "not defined")
  expect_error(getSapComponent(sap, "banana", "coh_001"), "choice")
})

test_that("update replaces in place and revalidates", {
  sap <- mkSap()
  renamed <- newSapCohort(
    "coh_001", "Renamed", "ds_001", "concept_cohort",
    list(codelist_id = "cl_001", exit = "e", overlap = "o")
  )
  sap2 <- updateSapComponent(sap, renamed)
  expect_identical(getSapComponent(sap2, "cohorts", "coh_001")$name, "Renamed")
  expect_identical(sapComponentIds(sap2, "cohorts"), sapComponentIds(sap, "cohorts"))

  dangling <- newSapCohort(
    "coh_001", "Broken", "ds_001", "concept_cohort",
    list(codelist_id = "cl_MISSING", exit = "e", overlap = "o")
  )
  expect_error(updateSapComponent(sap, dangling), "missing_reference")
})

test_that("remove blocks while referenced and works in dependency order", {
  sap <- mkSap()
  expect_error(removeSapComponent(sap, "data_sources", "ds_001"), "missing_reference")
  expect_error(removeSapComponent(sap, "codelists", "cl_001"), "missing_reference")
  sap <- removeSapComponent(sap, "analyses", "an_001")
  sap <- removeSapComponent(sap, "cohorts", "coh_001")
  sap <- removeSapComponent(sap, "codelists", "cl_001")
  sap <- removeSapComponent(sap, "data_sources", "ds_001")
  expect_length(sapComponentIds(sap, "data_sources"), 0)
  expect_error(removeSapComponent(sap, "cohorts", "coh_001"), "not defined")
})

test_that("a failed remove leaves the original sap untouched", {
  sap <- mkSap()
  try(removeSapComponent(sap, "data_sources", "ds_001"), silent = TRUE)
  expect_length(checkSap(sap), 0)
  expect_identical(sapComponentIds(sap, "data_sources"), "ds_001")
})

test_that("ids are unique across collections", {
  sap <- mkSap()
  description <- structure(list(), class = "data_source_description")
  expect_error(
    addSapComponent(sap, newSapDataSource("coh_001", "Clash", description)),
    "already exists"
  )
  broken <- unclass(sap)
  broken$data_sources <- c(broken$data_sources,
                           list(list(id = "coh_001", name = "Clash",
                                     description = description)))
  codes <- purrr::map_chr(checkSap(broken), "code")
  expect_true("duplicate_id" %in% codes)
})

test_that("updateStudy replaces the singleton", {
  sap <- mkSap()
  sap2 <- updateStudy(sap, newSapStudy("study_001", "New title"))
  expect_identical(sap2$study$title, "New title")
  expect_error(updateStudy(sap, list(title = "x")), "sap_study")
})

test_that("classless components are rejected", {
  sap <- mkSap()
  expect_error(addSapComponent(sap, list(id = "x")), "sap_\\*")
  expect_error(updateSapComponent(sap, list(id = "coh_001")), "sap_\\*")
})

test_that("the add* wrappers still work after re-signing", {
  description <- structure(list(), class = "data_source_description")
  sap <- createSap(newSapStudy("study_001", "My study"))
  sap <- addDataSource(sap, newSapDataSource("ds_001", "CPRD", description))
  sap <- addDataSourceModification(sap, newSapDataSourceModification(
    "mod_001", "Trim", "trim_observation_period", "ds_001"
  ))
  sap <- addCodelist(sap, newSapCodelist(
    "cl_001", "Codes", "codelist", structure(list(), class = "codelist")
  ))
  sap <- addCohort(sap, newSapCohort(
    "coh_001", "Cohort", "ds_001", "concept_cohort",
    list(codelist_id = "cl_001", exit = "e", overlap = "o")
  ))
  sap <- addAnalysis(sap, newSapAnalysis(
    "an_001", "Incidence", "ds_001", "incidence",
    list(denominator_cohort_id = "coh_001")
  ))
  expect_length(checkSap(sap), 0)
})
