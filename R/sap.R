
#' Create an OMOP Standardised Analysis Plan
#'
#' @param x An R list to create a `sap` object.
#'
#' @returns A sap object.
#' @export
#'
newSap <- function(x) {
  x <- constructSap(x)
  x <- validateSap(x)
  return(x)
}

constructSap <- function(x, call = parent.frame()) {
  omopgenerics::assertList(x, named = TRUE, call = call)
  if (!"sap_schema_version" %in% names(x)) {
    x$sap_schema_version <- sapVersions[length(sapVersions)]
  }
  if (!"generated_at" %in% names(x)) {
    x$generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }
  structure(x, class = "omop_sap")
}

validateSap <- function(x, call = parent.frame()) {
  # sap version
  # uniqueness of keys
}

changeSapVersion <- function(sap, newVersion) {

}

sapVersions <- "v0.1.0"

createSap <- function(studyName,
                      databases = list(),
                      databases_modifications = list(),
                      codelists = list(),
                      cohorts = list(),
                      analyses = list()) {

}

newDatabaseModification <- function(dataSourceId,
                                    changeType,
                                    parameters = list()) {

}

appendElement <- function(sap, element) {
  sap <- validateSapArgument(sap)

  # append element
  if (inherits(element, "data_source_description")) {
    sap$data_sources <- append(sap$data_sources, element)
  } else if (inherits(element, "data_source_modification")) {
    sap$data_source_modifications <- append(sap$data_source_modifications, element)
  } else if (inherits(element, "cohort_definition")) {
    sap$cohort_definitions <- append(sap$cohort_definitions, element)
  } else if (inherits(element, "codelist_definition")) {
    sap$codelist_definitions <- append(sap$codelist_definitions, element)
  } else if (inherits(element, "analysis_definition")) {
    sap$analyses_definitions <- append(sap$analyses_definitions, element)
  }

  # validation
  sap <- validateSap(sap)

  return(sap)
}

newCohortDefinition <- function(x) {

}

newAnalysis <- function(x) {

}
