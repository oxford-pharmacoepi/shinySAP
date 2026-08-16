
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
  #
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

newDatabaseModification <- function(x) {

}

newCohortDefinition <- function(x) {

}

newAnalysis <- function(x) {

}
