# SAP constructors and public operations -------------------------------------

#' Create a structured Statistical Analysis Plan
#'
#' @param x A named R list containing the SAP fields.
#' @param validate Whether to validate the complete SAP before returning it.
#'
#' @return An object of class `sap`.
#' @export
newSap <- function(x, validate = TRUE) {
  sap <- constructSap(x)
  if (isTRUE(validate)) validateSap(sap)
  sap
}

constructSap <- function(x,
                         version = currentSapSchemaVersion(),
                         generatedAt = NULL) {
  omopgenerics::assertList(x, named = TRUE)

  if (is.null(x$sap_schema_version)) {
    x$sap_schema_version <- version
  }
  if (!identical(as.character(x$sap_schema_version), version)) {
    cli::cli_abort(c(x = paste0("sap_schema_version must be ", version, ".")))
  }
  if (is.null(x$generated_at)) {
    x$generated_at <- generatedAt %||% format(
      Sys.time(), "%Y-%m-%dT%H:%M:%S%z"
    )
  }

  sapFields <- schemaFields("sap", version)
  collectionNames <- sapFields$path[sapFields$node_type == "collection"]
  for (collectionName in collectionNames) {
    if (is.null(x[[collectionName]])) x[[collectionName]] <- list()
  }

  structure(x, class = c("sap", "list"))
}

#' Create the study metadata component of a SAP
#'
#' @param studyId Immutable study identifier.
#' @param title Study title.
#' @param authors Character vector of study authors.
#' @param version Study version label.
#' @param description Optional study description.
#'
#' @return An object of class `sap_study`.
#' @export
newSapStudy <- function(studyId,
                        title,
                        authors = character(),
                        version = "v1.0.0",
                        description = NULL) {
  omopgenerics::assertCharacter(
    studyId, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "studyId"
  )
  omopgenerics::assertCharacter(
    title, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "title"
  )
  omopgenerics::assertCharacter(authors, na = FALSE, nm = "authors")
  omopgenerics::assertCharacter(
    version, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "version"
  )
  if (!is.null(description)) {
    omopgenerics::assertCharacter(
      description, length = 1, na = FALSE, null = FALSE, empty = FALSE,
      minNumCharacter = 1, nm = "description"
    )
  }

  structure(
    list(
      study_id = studyId,
      title = title,
      authors = authors,
      version = version,
      description = description
    ),
    class = c("sap_study", "list")
  )
}

#' Create a SAP data-source component
#'
#' @param id Immutable data-source identifier.
#' @param name Display name of the data source.
#' @param description A `data_source_description` object.
#'
#' @return An object of class `sap_data_source`.
#' @export
newSapDataSource <- function(id, name, description) {
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "id"
  )
  omopgenerics::assertCharacter(
    name, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "name"
  )
  omopgenerics::assertClass(
    description, class = "data_source_description", null = FALSE,
    all = TRUE, nm = "description"
  )
  structure(
    list(id = id, name = name, description = description),
    class = c("sap_data_source", "list")
  )
}

#' Create a data-source modification component
#'
#' @param id Immutable modification identifier.
#' @param name Display name of the modification.
#' @param type Data-source modification type.
#' @param dataSourceId Identifier of the affected data source(s).
#' @param parameters Type-specific modification parameters.
#'
#' @return An object of class `sap_data_source_modification`.
#' @export
newSapDataSourceModification <- function(id,
                                         name,
                                         type,
                                         dataSourceId,
                                         parameters = list()) {
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "id"
  )
  omopgenerics::assertCharacter(
    name, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "name"
  )
  omopgenerics::assertChoice(
    type, schemaTypes("data_source_modification"), length = 1,
    na = FALSE, null = FALSE, empty = FALSE, nm = "type"
  )
  omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
  parameters <- validateParameters(
    parameters, "data_source_modification", type
  )
  structure(
    list(
      id = id,
      name = name,
      type = type,
      data_source_id = dataSourceId,
      parameters = parameters
    ),
    class = c("sap_data_source_modification", "list")
  )
}

#' Create a SAP codelist component from an external codelist object
#'
#' @param id Immutable codelist identifier.
#' @param name Display name of the codelist.
#' @param type Codelist object type.
#' @param content External codelist object matching `type`.
#'
#' @return An object of class `sap_codelist`.
#' @export
newSapCodelist <- function(id, name, type, content) {
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "id"
  )
  omopgenerics::assertCharacter(
    name, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "name"
  )
  omopgenerics::assertChoice(
    type, schemaTypes("codelist"), length = 1,
    na = FALSE, null = FALSE, empty = FALSE, nm = "type"
  )
  expectedClass <- switch(
    type,
    codelist = "codelist",
    codelist_with_details = "codelist_with_details",
    concept_set_expression = "concept_set_expression"
  )
  omopgenerics::assertClass(
    content, class = expectedClass, all = TRUE, nm = "content"
  )
  structure(
    list(id = id, name = name, type = type, content = content),
    class = c("sap_codelist", "list")
  )
}

#' Create a SAP cohort component
#'
#' @param id Immutable cohort identifier.
#' @param name Display name of the cohort.
#' @param dataSourceId Identifier of the data source(s) used by the cohort.
#' @param type Cohort type.
#' @param parameters Type-specific cohort parameters.
#'
#' @return An object of class `sap_cohort`.
#' @export
newSapCohort <- function(id,
                         name,
                         dataSourceId,
                         type,
                         parameters = list()) {
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "id"
  )
  omopgenerics::assertCharacter(
    name, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "name"
  )
  omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
  omopgenerics::assertChoice(
    type, schemaTypes("cohort"), length = 1,
    na = FALSE, null = FALSE, empty = FALSE, nm = "type"
  )
  parameters <- validateParameters(parameters, "cohort", type)
  structure(
    list(
      id = id,
      name = name,
      data_source_id = dataSourceId,
      type = type,
      parameters = parameters
    ),
    class = c("sap_cohort", "list")
  )
}

#' Create a SAP analysis component
#'
#' @param id Immutable analysis identifier.
#' @param name Display name of the analysis.
#' @param dataSourceId Identifier of the data source(s) used by the analysis.
#' @param type Analysis type.
#' @param parameters Type-specific analysis parameters.
#'
#' @return An object of class `sap_analysis`.
#' @export
newSapAnalysis <- function(id,
                           name,
                           dataSourceId,
                           type,
                           parameters = list()) {
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "id"
  )
  omopgenerics::assertCharacter(
    name, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = "name"
  )
  omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
  omopgenerics::assertChoice(
    type, schemaTypes("analysis"), length = 1,
    na = FALSE, null = FALSE, empty = FALSE, nm = "type"
  )
  parameters <- validateParameters(parameters, "analysis", type)
  structure(
    list(
      id = id,
      name = name,
      data_source_id = dataSourceId,
      type = type,
      parameters = parameters
    ),
    class = c("sap_analysis", "list")
  )
}

#' Create a complete SAP from its components
#'
#' @param study A `sap_study` object.
#' @param dataSources List of `sap_data_source` objects.
#' @param dataSourceModifications List of `sap_data_source_modification` objects.
#' @param codelists List of `sap_codelist` objects.
#' @param cohorts List of `sap_cohort` objects.
#' @param analyses List of `sap_analysis` objects.
#'
#' @return An object of class `sap`.
#' @export
createSap <- function(study,
                      dataSources = list(),
                      dataSourceModifications = list(),
                      codelists = list(),
                      cohorts = list(),
                      analyses = list()) {
  omopgenerics::assertClass(
    study, class = "sap_study", all = TRUE, nm = "study"
  )
  assertComponentList(dataSources, "sap_data_source", "dataSources")
  assertComponentList(
    dataSourceModifications, "sap_data_source_modification",
    "dataSourceModifications"
  )
  assertComponentList(codelists, "sap_codelist", "codelists")
  assertComponentList(cohorts, "sap_cohort", "cohorts")
  assertComponentList(analyses, "sap_analysis", "analyses")

  newSap(list(
    study = study,
    data_sources = dataSources,
    data_source_modifications = dataSourceModifications,
    codelists = codelists,
    cohorts = cohorts,
    analyses = analyses
  ))
}

# Which collection a classed component belongs to.
componentCollection <- function(component) {
  hit <- which(paste0("sap_", SAP_COLLECTIONS) %in% class(component))
  if (length(hit) != 1L) {
    cli::cli_abort(c(x = "The component must be a single sap_* object."))
  }
  names(SAP_COLLECTIONS)[[hit]]
}

# Index of `id` within a collection; aborts on unknown collection or id.
locateSapComponent <- function(sap, collectionName, id) {
  omopgenerics::assertChoice(
    collectionName, names(SAP_COLLECTIONS), length = 1, nm = "collection"
  )
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, empty = FALSE, nm = "id"
  )
  ids <- purrr::map_chr(
    sap[[collectionName]] %||% list(),
    function(value) as.character(value$id %||% "")
  )
  index <- which(ids == id)
  if (!length(index)) {
    cli::cli_abort(c(x = sprintf(
      "The id '%s' is not defined in %s.", id, collectionName
    )))
  }
  index[[1]]
}

#' Add a component to a SAP
#'
#' @param sap A `sap` object.
#' @param component A `sap_*` component object.
#'
#' @return The updated `sap` object.
#' @export
addSapComponent <- function(sap, component) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  collectionName <- componentCollection(component)
  existingIds <- unlist(purrr::map(names(SAP_COLLECTIONS), function(name) {
    purrr::map_chr(sap[[name]] %||% list(),
                   function(value) as.character(value$id %||% ""))
  }))
  if (component$id %in% existingIds) {
    cli::cli_abort(c(x = sprintf("The id '%s' already exists.", component$id)))
  }
  sap[[collectionName]] <- c(sap[[collectionName]] %||% list(), list(component))
  newSap(sap)
}

#' Add a data source to a SAP
#'
#' @param sap A `sap` object.
#' @param dataSource A `sap_data_source` object.
#'
#' @return The updated `sap` object.
#' @export
addDataSource <- function(sap, dataSource) {
  addSapComponent(sap, dataSource)
}

#' Add a data-source modification to a SAP
#'
#' @param sap A `sap` object.
#' @param dataSourceModification A `sap_data_source_modification` object.
#'
#' @return The updated `sap` object.
#' @export
addDataSourceModification <- function(sap, dataSourceModification) {
  addSapComponent(sap, dataSourceModification)
}

#' Add a codelist to a SAP
#'
#' @param sap A `sap` object.
#' @param codelist A `sap_codelist` object.
#'
#' @return The updated `sap` object.
#' @export
addCodelist <- function(sap, codelist) {
  addSapComponent(sap, codelist)
}

#' Add a cohort to a SAP
#'
#' @param sap A `sap` object.
#' @param cohort A `sap_cohort` object.
#'
#' @return The updated `sap` object.
#' @export
addCohort <- function(sap, cohort) {
  addSapComponent(sap, cohort)
}

#' Add an analysis to a SAP
#'
#' @param sap A `sap` object.
#' @param analysis A `sap_analysis` object.
#'
#' @return The updated `sap` object.
#' @export
addAnalysis <- function(sap, analysis) {
  addSapComponent(sap, analysis)
}

assertComponentList <- function(value, className, argumentName) {
  omopgenerics::assertList(value, nm = argumentName)
  purrr::walk(value, function(component) {
    omopgenerics::assertClass(
      component, class = className, all = TRUE, nm = argumentName
    )
  })
}

#' Get a component from a SAP by id
#'
#' @param sap A `sap` object.
#' @param collection One of the SAP collection names.
#' @param id The component id.
#'
#' @return The component.
#' @export
getSapComponent <- function(sap, collection, id) {
  sap[[collection]][[locateSapComponent(sap, collection, id)]]
}

#' List the ids in a SAP collection
#'
#' @inheritParams getSapComponent
#'
#' @return A character vector of ids.
#' @export
sapComponentIds <- function(sap, collection) {
  omopgenerics::assertChoice(
    collection, names(SAP_COLLECTIONS), length = 1, nm = "collection"
  )
  purrr::map_chr(sap[[collection]] %||% list(),
                 function(value) as.character(value$id %||% ""))
}

#' Replace a component in a SAP, matched by id
#'
#' @param sap A `sap` object.
#' @param component A `sap_*` component whose id already exists in the SAP.
#'
#' @return The updated `sap` object.
#' @export
updateSapComponent <- function(sap, component) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  collectionName <- componentCollection(component)
  index <- locateSapComponent(sap, collectionName, component$id)
  sap[[collectionName]][[index]] <- component
  newSap(sap)
}

#' Remove a component from a SAP by id
#'
#' Removal is refused (with a `missing_reference` validation error) while any
#' other component still references the id.
#'
#' @inheritParams getSapComponent
#'
#' @return The updated `sap` object.
#' @export
removeSapComponent <- function(sap, collection, id) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  index <- locateSapComponent(sap, collection, id)
  sap[[collection]][[index]] <- NULL
  newSap(sap)
}

#' Replace the study metadata of a SAP
#'
#' @param sap A `sap` object.
#' @param study A `sap_study` object.
#'
#' @return The updated `sap` object.
#' @export
updateStudy <- function(sap, study) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  omopgenerics::assertClass(study, class = "sap_study", all = TRUE, nm = "study")
  sap$study <- study
  newSap(sap)
}
