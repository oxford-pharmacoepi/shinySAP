# SAP constructors and public operations -------------------------------------
#
# Two modes run through every constructor and CRUD function here:
#
#   validate = TRUE   (the default) the component or document must be complete
#                     and consistent, or the call aborts. This is the package's
#                     contract for a finished SAP.
#   validate = FALSE  a DRAFT: the classed object is built from whatever is
#                     given -- a NULL name, a type not yet chosen, half the
#                     parameters -- and nothing is asserted beyond the id and the
#                     shape of `parameters`. checkSap() then reports what is
#                     missing. This is how the Shiny app holds a plan while it is
#                     being written: every card is a draft component, and the
#                     document is mutated only through the CRUD below.
#
# Neither mode changes what a SAP IS: the schema (data-raw/) is the only
# definition, and validate = TRUE is simply that definition enforced.

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

  sapFields <- schemaFields("sap", version = version)
  collectionNames <- sapFields$path[sapFields$node_type == "collection"]
  for (collectionName in collectionNames) {
    if (is.null(x[[collectionName]])) x[[collectionName]] <- list()
    # Every item is a classed component whatever the entry point -- a
    # constructor, readSap() or a hand-built list -- so the CRUD can dispatch
    # on the class and the app never has to re-wrap what it reads.
    object <- SAP_COLLECTIONS[collectionName]
    if (is.list(x[[collectionName]]) && !is.na(object)) {
      x[[collectionName]] <- lapply(x[[collectionName]], asSapComponent, object = unname(object))
    }
  }
  x$study <- asSapComponent(x$study, "study")

  structure(x, class = c("sap", "list"))
}

# Add the sap_<object> class to a list that lacks it; anything that is not a
# list is left alone so checkSap() still reports it as not_an_object.
asSapComponent <- function(x, object) {
  className <- paste0("sap_", object)
  if (!is.list(x) || inherits(x, className)) return(x)
  structure(x, class = c(className, "list"))
}

# The one assertion every id shares: a single non-empty string.
assertId <- function(id, nm = "id") {
  omopgenerics::assertCharacter(
    id, length = 1, na = FALSE, null = FALSE, empty = FALSE,
    minNumCharacter = 1, nm = nm
  )
}

# Components -------------------------------------------------------------------

#' Create the study metadata component of a SAP
#'
#' @param studyId Immutable study identifier.
#' @param title Study title.
#' @param authors Character vector of study authors.
#' @param version Study version label.
#' @param description Optional study description.
#' @param validate Whether to require a complete study. With `FALSE` a draft is
#'   built from whatever is given and [checkSap()] reports what is missing.
#'
#' @return An object of class `sap_study`.
#' @export
newSapStudy <- function(studyId = NULL,
                        title = NULL,
                        authors = character(),
                        version = "v1.0.0",
                        description = NULL,
                        validate = TRUE) {
  if (isTRUE(validate)) {
    assertId(studyId, "studyId")
    assertId(title, "title")
    omopgenerics::assertCharacter(authors, na = FALSE, nm = "authors")
    assertId(version, "version")
    if (!is.null(description)) {
      omopgenerics::assertCharacter(
        description, length = 1, na = FALSE, null = FALSE, empty = FALSE,
        minNumCharacter = 1, nm = "description"
      )
    }
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
#' @param description An optional `data_source_description` object.
#' @inheritParams newSapStudy
#'
#' @return An object of class `sap_data_source`.
#' @export
newSapDataSource <- function(id, name = NULL, description = NULL, validate = TRUE) {
  assertId(id)
  if (isTRUE(validate)) {
    assertId(name, "name")
    if (!is.null(description)) {
      omopgenerics::assertClass(
        description, class = "data_source_description", all = TRUE, nm = "description"
      )
    }
  }
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
#' @inheritParams newSapStudy
#'
#' @return An object of class `sap_data_source_modification`.
#' @export
newSapDataSourceModification <- function(id,
                                         name = NULL,
                                         type = NULL,
                                         dataSourceId = character(),
                                         parameters = list(),
                                         validate = TRUE) {
  assertId(id)
  omopgenerics::assertList(parameters, named = TRUE, nm = "parameters")
  if (isTRUE(validate)) {
    assertId(name, "name")
    omopgenerics::assertChoice(
      type, schemaTypes("data_source_modification"), length = 1,
      na = FALSE, null = FALSE, empty = FALSE, nm = "type"
    )
    omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
    parameters <- validateParameters(parameters, "data_source_modification", type)
  }
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
#' @inheritParams newSapStudy
#'
#' @return An object of class `sap_codelist`.
#' @export
newSapCodelist <- function(id, name = NULL, type = NULL, content = NULL, validate = TRUE) {
  assertId(id)
  if (isTRUE(validate)) {
    assertId(name, "name")
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
  }
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
#' @inheritParams newSapStudy
#'
#' @return An object of class `sap_cohort`.
#' @export
newSapCohort <- function(id,
                         name = NULL,
                         dataSourceId = character(),
                         type = NULL,
                         parameters = list(),
                         validate = TRUE) {
  assertId(id)
  omopgenerics::assertList(parameters, named = TRUE, nm = "parameters")
  if (isTRUE(validate)) {
    assertId(name, "name")
    omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
    omopgenerics::assertChoice(
      type, schemaTypes("cohort"), length = 1,
      na = FALSE, null = FALSE, empty = FALSE, nm = "type"
    )
    parameters <- validateParameters(parameters, "cohort", type)
  }
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
#' @inheritParams newSapStudy
#'
#' @return An object of class `sap_analysis`.
#' @export
newSapAnalysis <- function(id,
                           name = NULL,
                           dataSourceId = character(),
                           type = NULL,
                           parameters = list(),
                           validate = TRUE) {
  assertId(id)
  omopgenerics::assertList(parameters, named = TRUE, nm = "parameters")
  if (isTRUE(validate)) {
    assertId(name, "name")
    omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
    omopgenerics::assertChoice(
      type, schemaTypes("analysis"), length = 1,
      na = FALSE, null = FALSE, empty = FALSE, nm = "type"
    )
    parameters <- validateParameters(parameters, "analysis", type)
  }
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

# The document -----------------------------------------------------------------

#' Create a complete SAP from its components
#'
#' @param study A `sap_study` object.
#' @param dataSources List of `sap_data_source` objects.
#' @param dataSourceModifications List of `sap_data_source_modification` objects.
#' @param codelists List of `sap_codelist` objects.
#' @param cohorts List of `sap_cohort` objects.
#' @param analyses List of `sap_analysis` objects.
#' @param validate Whether to validate the complete SAP. With `FALSE` the
#'   document is assembled from drafts and [checkSap()] reports its problems.
#'
#' @return An object of class `sap`.
#' @export
createSap <- function(study,
                      dataSources = list(),
                      dataSourceModifications = list(),
                      codelists = list(),
                      cohorts = list(),
                      analyses = list(),
                      validate = TRUE) {
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
  ), validate = validate)
}

assertComponentList <- function(value, className, argumentName) {
  omopgenerics::assertList(value, nm = argumentName)
  purrr::walk(value, function(component) {
    omopgenerics::assertClass(
      component, class = className, all = TRUE, nm = argumentName
    )
  })
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

# Ids ----------------------------------------------------------------------------

# The id prefix each collection's items are minted with.
SAP_ID_PREFIXES <- c(
  data_sources              = "ds",
  data_source_modifications = "mod",
  codelists                 = "cl",
  cohorts                   = "coh",
  analyses                  = "an"
)

#' Mint the next id for a SAP collection
#'
#' Ids are `<prefix>_<n>` (`ds_1`, `coh_3`, `an_2`). The next one is one past
#' the highest number in use anywhere in the SAP -- ids are unique across
#' collections -- and in `taken`, never the lowest free one, so a deleted
#' item's id is never reissued to a new item while the caller remembers it.
#'
#' @param sap A `sap` object.
#' @param collection One of the SAP collection names.
#' @param taken Further ids to treat as in use, e.g. ids removed earlier in a
#'   session.
#'
#' @return A single string.
#' @export
newSapId <- function(sap, collection, taken = character()) {
  omopgenerics::assertChoice(
    collection, names(SAP_COLLECTIONS), length = 1, nm = "collection"
  )
  omopgenerics::assertCharacter(taken, na = FALSE, nm = "taken")
  prefix <- SAP_ID_PREFIXES[[collection]]
  inUse <- c(
    unlist(purrr::map(names(SAP_COLLECTIONS), function(name) sapComponentIds(sap, name))),
    taken
  )
  pattern <- sprintf("^%s_(\\d+)$", prefix)
  used <- suppressWarnings(as.integer(sub(pattern, "\\1", grep(pattern, inUse, value = TRUE))))
  used <- used[!is.na(used)]
  sprintf("%s_%d", prefix, if (length(used)) max(used) + 1L else 1L)
}

# CRUD ---------------------------------------------------------------------------

#' Add a component to a SAP
#'
#' @param sap A `sap` object.
#' @param component A `sap_*` component object.
#' @param validate Whether to validate the whole SAP after the change. An id
#'   already in use is refused either way.
#'
#' @return The updated `sap` object.
#' @export
addSapComponent <- function(sap, component, validate = TRUE) {
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
  newSap(sap, validate = validate)
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
#' @inheritParams addSapComponent
#'
#' @return The updated `sap` object.
#' @export
updateSapComponent <- function(sap, component, validate = TRUE) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  collectionName <- componentCollection(component)
  index <- locateSapComponent(sap, collectionName, component$id)
  sap[[collectionName]][[index]] <- component
  newSap(sap, validate = validate)
}

#' Remove a component from a SAP by id
#'
#' With `validate = TRUE` removal is refused (with a `missing_reference`
#' validation error) while any other component still references the id. With
#' `validate = FALSE` the component is removed and [checkSap()] reports the
#' dangling reference.
#'
#' @inheritParams getSapComponent
#' @inheritParams addSapComponent
#'
#' @return The updated `sap` object.
#' @export
removeSapComponent <- function(sap, collection, id, validate = TRUE) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  index <- locateSapComponent(sap, collection, id)
  sap[[collection]][[index]] <- NULL
  newSap(sap, validate = validate)
}

#' Replace the study metadata of a SAP
#'
#' @param sap A `sap` object.
#' @param study A `sap_study` object.
#' @inheritParams addSapComponent
#'
#' @return The updated `sap` object.
#' @export
updateStudy <- function(sap, study, validate = TRUE) {
  omopgenerics::assertClass(sap, class = "sap", all = TRUE, nm = "sap")
  omopgenerics::assertClass(study, class = "sap_study", all = TRUE, nm = "study")
  sap$study <- study
  newSap(sap, validate = validate)
}
