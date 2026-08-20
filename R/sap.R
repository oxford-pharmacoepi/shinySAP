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

#' Check a SAP and return structured problems
#'
#' @param x A SAP object or named list.
#' @return A list of problems. An empty list means that no problems were found.
#' @export
checkSap <- function(x) {
  problems <- list()
  addProblem <- function(path, code, message) {
    problems[[length(problems) + 1L]] <<- list(
      path = path, code = code, message = message
    )
  }

  if (!is.list(x) || is.null(names(x))) {
    addProblem("", "not_a_list", "A SAP must be a named list.")
    return(problems)
  }

  version <- as.character(x$sap_schema_version %||% "")
  if (!nzchar(version) || is.null(sapSchema[[version]])) {
    addProblem(
      "sap_schema_version", "unknown_schema_version",
      sprintf("Unknown SAP schema version '%s'.", version)
    )
    return(problems)
  }

  checkObject <- function(value, object, path, typeId = NULL) {
    if (!is.list(value) || is.null(names(value))) {
      addProblem(path, "not_an_object", "The value must be a named list.")
      return(invisible(NULL))
    }

    fields <- schemaFields(object, typeId, version)
    allowedNames <- schemaObjectNames(object, typeId, version)
    unknownNames <- setdiff(names(value), allowedNames)
    purrr::walk(unknownNames, function(unknownName) {
      addProblem(
        paste0(path, if (nzchar(path)) "." else "", unknownName),
        "unknown_field", "The field is not defined by the SAP schema."
      )
    })

    directFields <- fields[!grepl("\\.", fields$path), , drop = FALSE]
    purrr::walk(seq_len(nrow(directFields)), function(fieldIndex) {
      field <- directFields[fieldIndex, , drop = FALSE]
      fieldName <- field$path[[1]]
      fieldValue <- value[[fieldName]]
      fieldPath <- paste0(path, if (nzchar(path)) "." else "", fieldName)

      if (isTRUE(field$required[[1]]) && isMissingSapValue(fieldValue)) {
        addProblem(fieldPath, "missing_required_field", "A value is required.")
        return(invisible(NULL))
      }
      if (isMissingSapValue(fieldValue)) return(invisible(NULL))

      checkValue(fieldValue, field, fieldPath)
    })

    if (!is.null(value$type) && !isMissingSapValue(value$type)) {
      validTypes <- schemaTypes(object, version)
      typeValue <- as.character(value$type)
      if (length(typeValue) != 1L || !typeValue %in% validTypes) {
        addProblem(
          paste0(path, ".type"), "unknown_type",
          sprintf("Type '%s' is not defined for %s.", paste(typeValue, collapse = ", "), object)
        )
      }
    }

    if (!is.null(value$parameters) && is.list(value$parameters)) {
      checkParameters(value$parameters, object, value$type, path)
    }

    if (identical(object, "codelist") && !isMissingSapValue(value$content)) {
      expectedClass <- switch(
        as.character(value$type %||% ""),
        codelist = "codelist",
        codelist_with_details = "codelist_with_details",
        concept_set_expression = "concept_set_expression",
        NULL
      )
      if (!is.null(expectedClass) && !inherits(value$content, expectedClass)) {
        addProblem(
          paste0(path, ".content"), "invalid_external_object",
          sprintf("The value must inherit from '%s'.", expectedClass)
        )
      }
    }

    invisible(NULL)
  }

  checkValue <- function(value, field, path) {
    valueType <- field$value_type[[1]]

    if (valueType %in% c("character", "id", "type_id", "datetime")) {
      if (!is.character(value) || length(value) != 1L) {
        addProblem(path, "invalid_value_type", "A single character value is required.")
      }
    } else if (valueType %in% c("character_vector", "id_vector")) {
      if (!is.character(value)) {
        addProblem(path, "invalid_value_type", "A character vector is required.")
      }
    } else if (valueType == "date_range") {
      if (!inherits(value, "Date") || length(value) != 2L) {
        addProblem(path, "invalid_value_type", "A two-element Date vector is required.")
      }
    } else if (valueType == "data_source_description") {
      if (!inherits(value, "data_source_description")) {
        addProblem(
          path, "invalid_external_object",
          "The value must be a data_source_description object."
        )
      }
    }
  }

  checkParameters <- function(parameters, object, typeId, path) {
    if (is.null(typeId) || isMissingSapValue(typeId)) return(invisible(NULL))
    fields <- schemaFields(object, as.character(typeId), version)
    parameterFields <- fields[grepl("^parameters\\.", fields$path), , drop = FALSE]
    parameterNames <- sub("^parameters\\.", "", parameterFields$path)

    unknownNames <- setdiff(names(parameters), parameterNames)
    purrr::walk(unknownNames, function(unknownName) {
      addProblem(
        paste0(path, ".parameters.", unknownName),
        "unknown_parameter", "The parameter is not defined for this type."
      )
    })

    purrr::walk(seq_len(nrow(parameterFields)), function(fieldIndex) {
      field <- parameterFields[fieldIndex, , drop = FALSE]
      parameterName <- sub("^parameters\\.", "", field$path[[1]])
      parameterValue <- parameters[[parameterName]]
      parameterPath <- paste0(path, ".parameters.", parameterName)
      if (isTRUE(field$required[[1]]) && isMissingSapValue(parameterValue)) {
        addProblem(parameterPath, "missing_required_parameter", "A value is required.")
      } else if (!isMissingSapValue(parameterValue)) {
        checkValue(parameterValue, field, parameterPath)
      }
    })
    invisible(NULL)
  }

  checkIds <- function(values, collectionName) {
    if (!length(values)) return(character())
    ids <- purrr::map_chr(values, function(value) {
      if (!is.list(value)) return("")
      as.character(value$id %||% "")
    })
    missing <- which(!nzchar(ids))
    if (length(missing)) {
      purrr::walk(missing, function(index) {
        addProblem(
          sprintf("%s[%d].id", collectionName, index),
          "missing_id", "Every item must have a non-empty id."
        )
      })
    }
    duplicatedIds <- unique(ids[duplicated(ids) & nzchar(ids)])
    purrr::walk(duplicatedIds, function(id) {
      addProblem(
        collectionName, "duplicate_id",
        sprintf("The id '%s' is used more than once.", id)
      )
    })
    unique(ids[nzchar(ids)])
  }

  checkReferences <- function(values, collectionName, fieldName, availableIds) {
    purrr::walk(seq_along(values), function(index) {
      if (!is.list(values[[index]])) return(invisible(NULL))
      references <- as.character(values[[index]][[fieldName]] %||% character())
      references <- references[nzchar(references)]
      missingReferences <- setdiff(references, availableIds)
      purrr::walk(missingReferences, function(reference) {
        addProblem(
          sprintf("%s[%d].%s", collectionName, index, fieldName),
          "missing_reference",
          sprintf("The id '%s' is not defined.", reference)
        )
      })
    })
  }

  checkObject(x, "sap", "")
  checkObject(x$study, "study", "study")

  collectionDefinitions <- list(
    data_sources = "data_source",
    data_source_modifications = "data_source_modification",
    codelists = "codelist",
    cohorts = "cohort",
    analyses = "analysis"
  )
  collectionIds <- purrr::map(
    names(collectionDefinitions),
    function(collectionName) {
    values <- x[[collectionName]]
    if (!is.list(values)) {
      addProblem(collectionName, "invalid_collection", "The value must be a list.")
      return(character())
    }
    ids <- checkIds(values, collectionName)
    object <- collectionDefinitions[[collectionName]]
    purrr::walk(seq_along(values), function(index) {
      value <- values[[index]]
      typeId <- if (is.list(value) && length(value$type) == 1L) value$type else NULL
      checkObject(value, object, sprintf("%s[%d]", collectionName, index), typeId)
    })
    ids
  }
  )
  names(collectionIds) <- names(collectionDefinitions)

  checkReferences(
    x$data_source_modifications %||% list(),
    "data_source_modifications", "data_source_id",
    collectionIds$data_sources
  )
  checkReferences(
    x$cohorts %||% list(), "cohorts", "data_source_id",
    collectionIds$data_sources
  )
  checkReferences(
    x$analyses %||% list(), "analyses", "data_source_id",
    collectionIds$data_sources
  )

  purrr::walk(seq_along(x$cohorts %||% list()), function(index) {
    cohort <- x$cohorts[[index]]
    if (!is.list(cohort)) return(invisible(NULL))
    if (identical(cohort$type, "concept_cohort")) {
      id <- cohort$parameters$codelist_id %||% ""
      if (nzchar(as.character(id)) && !id %in% collectionIds$codelists) {
        addProblem(
          sprintf("cohorts[%d].parameters.codelist_id", index),
          "missing_reference", sprintf("The id '%s' is not defined.", id)
        )
      }
    }
  })

  purrr::walk(seq_along(x$analyses %||% list()), function(index) {
    analysis <- x$analyses[[index]]
    if (!is.list(analysis)) return(invisible(NULL))
    purrr::walk(c("denominator_cohort_id", "outcome_cohort_id"), function(parameterName) {
      id <- analysis$parameters[[parameterName]] %||% ""
      if (nzchar(as.character(id)) && !id %in% collectionIds$cohorts) {
        addProblem(
          sprintf("analyses[%d].parameters.%s", index, parameterName),
          "missing_reference", sprintf("The id '%s' is not defined.", id)
        )
      }
    })
  })

  problems
}

#' Validate a SAP, throwing an error when problems are found
#'
#' @param x A SAP object or named list.
#' @return The validated SAP, invisibly.
#' @export
validateSap <- function(x) {
  problems <- checkSap(x)
  if (length(problems)) {
    messageText <- paste(purrr::map_chr(
      problems,
      function(problem) sprintf(
        "%s [%s]: %s", problem$path, problem$code, problem$message
      )
    ), collapse = "\n")
    cli::cli_abort(c(x = paste0("Invalid SAP:\n", messageText)))
  }
  invisible(x)
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

addSapComponent <- function(sap, component, collectionName, className) {
  omopgenerics::assertClass(
    sap, class = "sap", all = TRUE, nm = "sap"
  )
  omopgenerics::assertClass(
    component, class = className, all = TRUE, nm = "component"
  )
  existingIds <- purrr::map_chr(
    sap[[collectionName]] %||% list(),
    function(value) as.character(value$id %||% "")
  )
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
  addSapComponent(sap, dataSource, "data_sources", "sap_data_source")
}

#' Add a data-source modification to a SAP
#'
#' @param sap A `sap` object.
#' @param dataSourceModification A `sap_data_source_modification` object.
#'
#' @return The updated `sap` object.
#' @export
addDataSourceModification <- function(sap, dataSourceModification) {
  addSapComponent(
    sap, dataSourceModification,
    "data_source_modifications", "sap_data_source_modification"
  )
}

#' Add a codelist to a SAP
#'
#' @param sap A `sap` object.
#' @param codelist A `sap_codelist` object.
#'
#' @return The updated `sap` object.
#' @export
addCodelist <- function(sap, codelist) {
  addSapComponent(sap, codelist, "codelists", "sap_codelist")
}

#' Add a cohort to a SAP
#'
#' @param sap A `sap` object.
#' @param cohort A `sap_cohort` object.
#'
#' @return The updated `sap` object.
#' @export
addCohort <- function(sap, cohort) {
  addSapComponent(sap, cohort, "cohorts", "sap_cohort")
}

#' Add an analysis to a SAP
#'
#' @param sap A `sap` object.
#' @param analysis A `sap_analysis` object.
#'
#' @return The updated `sap` object.
#' @export
addAnalysis <- function(sap, analysis) {
  addSapComponent(sap, analysis, "analyses", "sap_analysis")
}

validateParameters <- function(parameters, object, typeId) {
  omopgenerics::assertList(parameters, named = TRUE, nm = "parameters")
  if (!length(parameters)) return(parameters)
  fields <- schemaFields(object, typeId)
  parameterFields <- fields[grepl("^parameters\\.", fields$path), , drop = FALSE]
  parameterNames <- sub("^parameters\\.", "", parameterFields$path)
  unknownNames <- setdiff(names(parameters), parameterNames)
  if (length(unknownNames)) {
    cli::cli_abort(c(x = paste0(
      "Unknown parameter(s) for ", typeId, ": ",
      paste(unknownNames, collapse = ", ")
    )))
  }
  purrr::walk(seq_len(nrow(parameterFields)), function(fieldIndex) {
    field <- parameterFields[fieldIndex, , drop = FALSE]
    parameterName <- sub("^parameters\\.", "", field$path[[1]])
    parameterValue <- parameters[[parameterName]]
    if (isTRUE(field$required[[1]]) && isMissingSapValue(parameterValue)) {
      cli::cli_abort(c(x = paste0("Missing required parameter: ", parameterName)))
    }
    if (!isMissingSapValue(parameterValue)) {
      validateParameterValue(parameterValue, field$value_type[[1]], parameterName)
    }
  })
  parameters
}

validateParameterValue <- function(value, valueType, parameterName) {
  if (valueType == "id") {
    omopgenerics::assertCharacter(
      value, length = 1, na = FALSE, null = FALSE, empty = FALSE,
      minNumCharacter = 1, nm = parameterName
    )
  } else if (valueType == "character") {
    omopgenerics::assertCharacter(
      value, length = 1, na = FALSE, null = FALSE, empty = FALSE,
      nm = parameterName
    )
  } else if (valueType == "date_range") {
    omopgenerics::assertDate(value, length = 2, na = TRUE, nm = parameterName)
  }
}

assertComponentList <- function(value, className, argumentName) {
  omopgenerics::assertList(value, nm = argumentName)
  purrr::walk(value, function(component) {
    omopgenerics::assertClass(
      component, class = className, all = TRUE, nm = argumentName
    )
  })
}

isMissingSapValue <- function(value) {
  is.null(value) || (length(value) == 0L && !is.list(value)) ||
    (length(value) == 1L && is.atomic(value) && is.na(value)) ||
    (is.character(value) && length(value) == 1L && !nzchar(trimws(value)))
}
