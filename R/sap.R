
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

getTypeId <- function(x, version = currentVersion) {
  sapTypes[[version]] |>
    dplyr::filter(.data$object == .env$x) |>
    dplyr::pull("type_id")
}

createSap <- function(study,
                      databases = list(),
                      databaseModifications = list(),
                      codelists = list(),
                      cohorts = list(),
                      analyses = list()) {
  # initial check
  omopgenerics::assertClass(study, "sap_study")
  omopgenerics::assertList(databases, class = "data_source_description")
  omopgenerics::assertList(databaseModifications, class = "sap_database_modification")
  omopgenerics::assertList(codelists, class = "sap_codelist")
  omopgenerics::assertList(cohorts, class = "sap_cohort")
  omopgenerics::assertList(analyses, class = "sap_analysis")

  list(
    study = study,
    databases = databases,
    databaseModifications = databaseModifications,
    codelists = codelists,
    cohorts = cohorts,
    analyses = analyses
  ) |>
    newSap()
}

newSapStudy <- function(studyKey = "0",
                        title = "My study",
                        authors = character(),
                        version = "v1.0.0",
                        description = "") {
  # input check
  omopgenerics::assertCharacter(studyKey, length = 1)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertCharacter(authors)
  omopgenerics::assertCharacter(version, length = 1)
  omopgenerics::assertCharacter(description)

  list(
    study_key = studyKey,
    title = title,
    authors = authors,
    version = version,
    description = description
  ) |>
    structure(class = "sap_study")
}

newSapDatabaseModification <- function(key,
                                       type,
                                       dataSource,
                                       parameters = list()) {
  # input check
  omopgenerics::assertCharacter(key, length = 1)
  omopgenerics::assertChoice(type, getTypeId("data_source_modification"))
  omopgenerics::assertCharacter(dataSource)
  parameters <- validateParameters(parameters, "data_source_modification", type)

  list(
    key = key,
    data_source = dataSource,
    type = type,
    parameters = parameters
  ) |>
    structure(class = "sap_database_modification")
}

newSapCodelists <- function(key,
                            type,
                            content = list()) {
  # input check
  omopgenerics::assertCharacter(key, length = 1)
  omopgenerics::assertChoice(type, getTypeId("codelist"))

  if (type == "codelist") {
    content <- omopgenerics::newCodelist(content)
  } else if (type == "codelist_with_details") {
    content <- omopgenerics::newCodelistWithDetails(content)
  } else if (type == "concept_set_expression") {
    content <- omopgenerics::newConceptSetExpression(content)
  }

  if (length(conent) != 1) {
    cli::cli_abort(c(x = "Please provide only one object."))
  }

  list(
    key = key,
    type = type,
    content = content
  ) |>
    structure(class = "sap_codelist")
}

newSapCohort <- function() {

}

newSapAnalysis <- function() {

}

# appendElement <- function(sap, element) {
#   sap <- validateSapArgument(sap)
#
#   # append element
#   if (inherits(element, "data_source_description")) {
#     sap$data_sources <- append(sap$data_sources, element)
#   } else if (inherits(element, "data_source_modification")) {
#     sap$data_source_modifications <- append(sap$data_source_modifications, element)
#   } else if (inherits(element, "cohort_definition")) {
#     sap$cohort_definitions <- append(sap$cohort_definitions, element)
#   } else if (inherits(element, "codelist_definition")) {
#     sap$codelist_definitions <- append(sap$codelist_definitions, element)
#   } else if (inherits(element, "analysis_definition")) {
#     sap$analyses_definitions <- append(sap$analyses_definitions, element)
#   }
#
#   # validation
#   sap <- validateSap(sap)
#
#   return(sap)
# }
