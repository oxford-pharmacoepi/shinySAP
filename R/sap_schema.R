# The SAP schema contract ------------------------------------------------------
#
# Everything that changes when the schema does: version and field accessors over
# the baked schema data (R/sysdata.rda, built by data-raw/sysdata.R), the value
# vocabularies, and the two validation paths -- checkSap()/validateSap() over a
# whole document, validateParameters() behind the component constructors.
TIME_INTERVALS <- c("weeks", "months", "quarters", "years", "overall")
TIME_POINTS <- c("start", "middle", "end")
LEVELS <- c("person", "record")
SAP_COLLECTIONS <- c(
  data_sources              = "data_source",
  data_source_modifications = "data_source_modification",
  codelists                 = "codelist",
  cohorts                   = "cohort",
  analyses                  = "analysis"
)


currentSapSchemaVersion <- function() {
  versions <- names(sapSchema)
  if (!length(versions)) {
    cli::cli_abort(c(x = "No SAP schemas are installed."))
  }
  currentVersion <- versions[[1]]
  if (length(versions) > 1L) {
    for (candidateVersion in versions[-1]) {
      if (utils::compareVersion(candidateVersion, currentVersion) > 0) {
        currentVersion <- candidateVersion
      }
    }
  }
  currentVersion
}

schemaFields <- function(object,
                         typeId = NULL,
                         version = currentSapSchemaVersion()) {
  fields <- sapSchema[[version]]
  if (is.null(fields)) {
    cli::cli_abort(c(x = paste0("Unknown SAP schema version: ", version, ".")))
  }

  fields <- fields[fields$object == object, , drop = FALSE]
  common <- is.na(fields$type_id) | !nzchar(fields$type_id)
  typed <- rep(FALSE, nrow(fields))
  if (!is.null(typeId) && length(typeId) == 1L && !is.na(typeId)) {
    typed <- !is.na(fields$type_id) & fields$type_id == typeId
  }
  fields[common | typed, , drop = FALSE]
}

schemaReferences <- function(version = currentSapSchemaVersion()) {
  fields <- sapSchema[[version]]
  if (is.null(fields)) {
    cli::cli_abort(c(x = paste0("Unknown SAP schema version: ", version, ".")))
  }
  fields[!is.na(fields$ref), , drop = FALSE]
}

schemaTypes <- function(object,
                        version = currentSapSchemaVersion()) {
  types <- sapTypes[[version]]
  if (is.null(types)) {
    cli::cli_abort(c(x = paste0("Unknown SAP schema version: ", version, ".")))
  }
  types$type_id[types$object == object]
}

schemaObjectNames <- function(object,
                              typeId = NULL,
                              version = currentSapSchemaVersion()) {
  fields <- schemaFields(object, typeId, version)
  unique(sub("\\..*$", "", fields$path))
}



timeIntervalChoices <- function(typeId) {
  if (identical(as.character(typeId), "incidence")) {
    TIME_INTERVALS
  } else {
    setdiff(TIME_INTERVALS, "overall")
  }
}

#' Check a SAP and return structured problems
#'
#' @param x A SAP object or named list.
#' @return A list of problems. An empty list means that no problems were found.
#' @export
checkSap <- function(x) {
  collected <- new.env(parent = emptyenv())
  collected$problems <- list()
  addProblem <- function(path, code, message) {
    collected$problems[[length(collected$problems) + 1L]] <- list(
      path = path, code = code, message = message
    )
  }

  if (!is.list(x) || is.null(names(x))) {
    addProblem("", "not_a_list", "A SAP must be a named list.")
    return(collected$problems)
  }

  version <- as.character(x$sap_schema_version %||% "")
  if (!nzchar(version) || is.null(sapSchema[[version]])) {
    addProblem(
      "sap_schema_version", "unknown_schema_version",
      sprintf("Unknown SAP schema version '%s'.", version)
    )
    return(collected$problems)
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
    } else if (valueType == "logic") {
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        addProblem(path, "invalid_value_type", "A single logical value is required.")
      }
    } else if (valueType == "time_point") {
      checkChoice(value, TIME_POINTS, path)
    } else if (valueType == "time_interval") {
      checkChoice(value, timeIntervalChoices(field$type_id[[1]]), path)
    } else if (valueType == "level") {
      checkChoice(value, LEVELS, path)
    } else if (valueType == "integer") {
      if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
        addProblem(path, "invalid_value_type", "A single numeric value is required.")
      }
    } else if (valueType %in% c("age_group", "time_at_risk")) {
      ok <- is.list(value) && all(purrr::map_lgl(value, function(bounds) {
        bounds <- suppressWarnings(as.numeric(unlist(bounds)))
        length(bounds) == 2L && !anyNA(bounds) && bounds[1] >= 0 &&
          bounds[2] >= bounds[1] &&
          (valueType == "time_at_risk" || is.finite(bounds[2]))
      }))
      if (!ok) {
        addProblem(path, "invalid_value_type", paste0(
          "A list of c(lower, upper) is required, 0 <= lower <= upper",
          if (valueType == "age_group") " and upper is finite", "."
        ))
      }
    } else if (valueType == "strata") {
      ok <- is.list(value) && all(purrr::map_lgl(value, function(group) {
        is.character(group) && length(group) > 0L
      }))
      if (!ok) {
        addProblem(path, "invalid_value_type", "A list of character vectors is required.")
      }
    } else if (!valueType %in% c("object", "external_object")) {
      addProblem(path, "unknown_value_type", sprintf(
        "The value_type '%s' is not permitted.", valueType
      ))
    }
  }


  checkChoice <- function(value, choices, path) {
    if (!is.character(value) || length(value) != 1L) {
      addProblem(path, "invalid_value_type", "A single character value is required.")
    } else if (!value %in% choices) {
      addProblem(path, "invalid_value", sprintf(
        "The value must be one of %s.", paste(choices, collapse = ", ")
      ))
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

  checkObject(x, "sap", "")
  checkObject(x$study, "study", "study")

  collectionIds <- purrr::map(
    names(SAP_COLLECTIONS),
    function(collectionName) {
    values <- x[[collectionName]]
    if (!is.list(values)) {
      addProblem(collectionName, "invalid_collection", "The value must be a list.")
      return(character())
    }
    ids <- checkIds(values, collectionName)
    object <- SAP_COLLECTIONS[[collectionName]]
    purrr::walk(seq_along(values), function(index) {
      value <- values[[index]]
      typeId <- if (is.list(value) && length(value$type) == 1L) value$type else NULL
      checkObject(value, object, sprintf("%s[%d]", collectionName, index), typeId)
    })
    ids
  }
  )
  names(collectionIds) <- names(SAP_COLLECTIONS)

  allIds <- unlist(collectionIds, use.names = FALSE)
  crossDuplicates <- unique(allIds[duplicated(allIds)])
  purrr::walk(crossDuplicates, function(id) {
    where <- names(collectionIds)[
      purrr::map_lgl(collectionIds, function(ids) id %in% ids)
    ]
    addProblem(
      paste(where, collapse = ", "), "duplicate_id",
      sprintf("The id '%s' is used in more than one collection.", id)
    )
  })

  objectCollections <- stats::setNames(
    names(SAP_COLLECTIONS), unlist(SAP_COLLECTIONS)
  )
  references <- schemaReferences(version)

  purrr::walk(seq_len(nrow(references)), function(rowIndex) {
    field <- references[rowIndex, , drop = FALSE]
    collectionName <- unname(objectCollections[field$object[[1]]])
    if (is.na(collectionName)) return(invisible(NULL))
    fieldPath <- strsplit(field$path[[1]], ".", fixed = TRUE)[[1]]
    availableIds <- collectionIds[[field$ref[[1]]]]
    typeId <- field$type_id[[1]]
    values <- x[[collectionName]] %||% list()

    purrr::walk(seq_along(values), function(index) {
      value <- values[[index]]
      if (!is.list(value)) return(invisible(NULL))
      if (!is.na(typeId) &&
          !identical(as.character(value$type %||% ""), typeId)) {
        return(invisible(NULL))
      }
      ids <- as.character(purrr::pluck(value, !!!fieldPath) %||% character())
      purrr::walk(setdiff(ids[nzchar(ids)], availableIds), function(id) {
        addProblem(
          sprintf("%s[%d].%s", collectionName, index, field$path[[1]]),
          "missing_reference", sprintf("The id '%s' is not defined.", id)
        )
      })
    })
  })

  collected$problems
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
      validateParameterValue(
        parameterValue, field$value_type[[1]], parameterName, field$type_id[[1]]
      )
    }
  })
  parameters
}

validateParameterValue <- function(value, valueType, parameterName,
                                   typeId = NA_character_) {
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
  } else if (valueType %in% c("character_vector", "id_vector")) {
    omopgenerics::assertCharacter(value, na = FALSE, nm = parameterName)
  } else if (valueType == "logic") {
    omopgenerics::assertLogical(value, length = 1, na = FALSE, nm = parameterName)
  }  else if (valueType == "date_range") {
    omopgenerics::assertDate(value, length = 2, na = TRUE, nm = parameterName)
  } else if (valueType %in% c("age_group", "time_at_risk")) {
    omopgenerics::assertList(value, nm = parameterName)
    purrr::walk(value, function(bounds) {
      bounds <- as.numeric(unlist(bounds))
      omopgenerics::assertNumeric(
        bounds, length = 2, min = 0, na = FALSE, nm = parameterName
      )
      boundsAscend <- bounds[2] >= bounds[1]
      omopgenerics::assertTrue(boundsAscend, msg = paste0(
        "Each ", parameterName, " pair must have upper >= lower."
      ))
      if (valueType == "age_group") {
        upperIsFinite <- is.finite(bounds[2])
        omopgenerics::assertTrue(upperIsFinite, msg = paste0(
          "Each ", parameterName, " pair must have a finite upper bound."
        ))
      }
    })
  }  else if (valueType == "time_point") {
    omopgenerics::assertChoice(
      value, choices = TIME_POINTS, length = 1, nm = parameterName
    )
  } else if (valueType == "time_interval") {
    omopgenerics::assertChoice(
      value, choices = timeIntervalChoices(typeId), length = 1, nm = parameterName
    )
  } else if (valueType == "level") {
    omopgenerics::assertChoice(
      value, choices = LEVELS, length = 1, nm = parameterName
    )
  } else if (valueType == "integer") {
    omopgenerics::assertNumeric(value, length = 1, na = FALSE, nm = parameterName)
  } else if (valueType == "strata") {
    omopgenerics::assertList(value, nm = parameterName)
    purrr::walk(value, function(group) {
      omopgenerics::assertCharacter(group, na = FALSE, nm = parameterName)
    })
  } else if (!valueType %in% c("object", "external_object")) {
    cli::cli_abort(c(x = paste0(
      "Unknown value type '", valueType, "' for ", parameterName, "."
    )))
  }
}

isMissingSapValue <- function(value) {
  is.null(value) || (length(value) == 0L && !is.list(value)) ||
    (length(value) == 1L && is.atomic(value) && is.na(value)) ||
    (is.character(value) && length(value) == 1L && !nzchar(trimws(value)))
}


# Internal helpers for the SAP core -------------------------------------------

# NOT base R's `%||%` (added in 4.4.0), which only tests is.null(). A SAP read
# back from JSON carries absent values as NA and empty collections as length-0,
# and every call site here means "missing" in that wider sense -- so shadowing
# base is deliberate. Dropping it silently changes constructSap()'s defaulting.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}
