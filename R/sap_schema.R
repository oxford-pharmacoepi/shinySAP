# The SAP schema contract ------------------------------------------------------
#
# Everything that changes when the schema does: version and field accessors over
# the baked schema data (R/sysdata.rda, built by data-raw/sysdata.R), the value
# vocabularies, the component-level checkers (checkComponent(), checkParameters(),
# checkValue()) that return problems as data, and the two entry points built on
# them -- checkSap()/validateSap() over a whole document, validateParameters()
# behind the component constructors.
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


# Exported, read-only views of the schema. The Shiny app in inst/app builds its
# dropdowns and parameter blocks from these rather than restating the schema, so
# a row added to schema.csv / types.csv reaches the UI without an app change.

#' The current SAP schema version
#'
#' @return A single string, e.g. `"0.1.0"`.
#' @export
sapSchemaVersion <- function() {
  currentSapSchemaVersion()
}

#' Types defined for a SAP object
#'
#' @param object One of `"data_source_modification"`, `"codelist"`, `"cohort"`
#'   or `"analysis"`.
#' @return A named character vector: names are the human labels from the
#'   schema, values are the type ids. Ready for a `selectInput()` `choices`.
#' @export
sapSchemaTypes <- function(object) {
  version <- currentSapSchemaVersion()
  types <- sapTypes[[version]]
  types <- types[types$object == object, , drop = FALSE]
  stats::setNames(types$type_id, types$label)
}

#' Schema fields for a SAP object
#'
#' @param object The SAP object name (`"sap"`, `"study"`, `"data_source"`,
#'   `"data_source_modification"`, `"codelist"`, `"cohort"`, `"analysis"`).
#' @param typeId Optional type id; when given, the type-specific rows are
#'   included alongside the common ones.
#' @return A data frame with one row per field: `path`, `node_type`,
#'   `value_type`, `json_type`, `required`, `default_json`, `order`, `ref`.
#' @export
sapSchemaFields <- function(object, typeId = NULL) {
  fields <- schemaFields(object, typeId)
  fields <- fields[order(fields$order), , drop = FALSE]
  rownames(fields) <- NULL
  fields
}

#' Controlled vocabularies used by SAP fields
#'
#' @return A list with `time_points`, `levels`, `time_intervals` (a function of
#'   the analysis type id, since `"overall"` is not valid for every type) and
#'   `collections` (collection name -> object name).
#' @export
sapSchemaVocabulary <- function() {
  list(
    time_points    = TIME_POINTS,
    levels         = LEVELS,
    time_intervals = timeIntervalChoices,
    collections    = SAP_COLLECTIONS
  )
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
  if (!is.list(x) || is.null(names(x))) {
    return(list(sapProblem("", "not_a_list", "A SAP must be a named list.")))
  }

  version <- as.character(x$sap_schema_version %||% "")
  if (!nzchar(version) || is.null(sapSchema[[version]])) {
    return(list(sapProblem(
      "sap_schema_version", "unknown_schema_version",
      sprintf("Unknown SAP schema version '%s'.", version)
    )))
  }

  collected <- new.env(parent = emptyenv())
  collected$problems <- list()
  addProblems <- function(problems) {
    collected$problems <- c(collected$problems, problems)
  }

  checkIds <- function(values, collectionName) {
    if (!length(values)) return(character())
    ids <- purrr::map_chr(values, function(value) {
      if (!is.list(value)) return("")
      as.character(value$id %||% "")
    })
    purrr::walk(which(!nzchar(ids)), function(index) {
      addProblems(list(sapProblem(
        sprintf("%s[%d].id", collectionName, index),
        "missing_id", "Every item must have a non-empty id."
      )))
    })
    purrr::walk(unique(ids[duplicated(ids) & nzchar(ids)]), function(id) {
      addProblems(list(sapProblem(
        collectionName, "duplicate_id",
        sprintf("The id '%s' is used more than once.", id)
      )))
    })
    unique(ids[nzchar(ids)])
  }

  addProblems(checkComponent(x, "sap", "", version = version))
  addProblems(checkComponent(x$study, "study", "study", version = version))

  collectionIds <- purrr::map(names(SAP_COLLECTIONS), function(collectionName) {
    values <- x[[collectionName]]
    if (!is.list(values)) {
      addProblems(list(sapProblem(
        collectionName, "invalid_collection", "The value must be a list."
      )))
      return(character())
    }
    ids <- checkIds(values, collectionName)
    object <- SAP_COLLECTIONS[[collectionName]]
    purrr::walk(seq_along(values), function(index) {
      value <- values[[index]]
      typeId <- if (is.list(value) && length(value$type) == 1L) value$type else NULL
      addProblems(checkComponent(
        value, object, sprintf("%s[%d]", collectionName, index), typeId, version
      ))
    })
    ids
  })
  names(collectionIds) <- names(SAP_COLLECTIONS)

  allIds <- unlist(collectionIds, use.names = FALSE)
  purrr::walk(unique(allIds[duplicated(allIds)]), function(id) {
    where <- names(collectionIds)[
      purrr::map_lgl(collectionIds, function(ids) id %in% ids)
    ]
    addProblems(list(sapProblem(
      paste(where, collapse = ", "), "duplicate_id",
      sprintf("The id '%s' is used in more than one collection.", id)
    )))
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
        addProblems(list(sapProblem(
          sprintf("%s[%d].%s", collectionName, index, field$path[[1]]),
          "missing_reference", sprintf("The id '%s' is not defined.", id)
        )))
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
    cli::cli_abort(c(x = paste0("Invalid SAP:\n", formatProblems(problems))))
  }
  invisible(x)
}

validateParameters <- function(parameters, object, typeId) {
  omopgenerics::assertList(parameters, named = TRUE, nm = "parameters")
  problems <- checkParameters(parameters, object, typeId, path = "")
  if (length(problems)) {
    cli::cli_abort(c(x = paste0(
      "Invalid parameters for ", typeId, ":\n", formatProblems(problems)
    )))
  }
  parameters
}

# Component-level checkers ----------------------------------------------------
#
# Each checker returns a list of problems and never throws. checkSap() walks a
# document with them and validateParameters() throws on their output, so a
# schema rule is written once and both entry points agree.

sapProblem <- function(path, code, message) {
  list(path = path, code = code, message = message)
}

formatProblems <- function(problems) {
  paste(purrr::map_chr(problems, function(problem) {
    sprintf("%s [%s]: %s", problem$path, problem$code, problem$message)
  }), collapse = "\n")
}

joinPath <- function(path, name) {
  paste0(path, if (nzchar(path)) "." else "", name)
}

flattenProblems <- function(problemLists) {
  as.list(unlist(problemLists, recursive = FALSE))
}

checkComponent <- function(value,
                           object,
                           path,
                           typeId = NULL,
                           version = currentSapSchemaVersion()) {
  if (!is.list(value) || is.null(names(value))) {
    return(list(sapProblem(path, "not_an_object", "The value must be a named list.")))
  }

  fields <- schemaFields(object, typeId, version)
  allowedNames <- schemaObjectNames(object, typeId, version)
  problems <- purrr::map(setdiff(names(value), allowedNames), function(name) {
    sapProblem(
      joinPath(path, name), "unknown_field",
      "The field is not defined by the SAP schema."
    )
  })

  directFields <- fields[!grepl("\\.", fields$path), , drop = FALSE]
  problems <- c(problems, flattenProblems(purrr::map(
    seq_len(nrow(directFields)),
    function(fieldIndex) {
      field <- directFields[fieldIndex, , drop = FALSE]
      fieldName <- field$path[[1]]
      fieldValue <- value[[fieldName]]
      fieldPath <- joinPath(path, fieldName)
      if (isTRUE(field$required[[1]]) && isMissingSapValue(fieldValue)) {
        return(list(sapProblem(
          fieldPath, "missing_required_field", "A value is required."
        )))
      }
      if (isMissingSapValue(fieldValue)) return(list())
      checkValue(fieldValue, field$value_type[[1]], fieldPath, field$type_id[[1]])
    }
  )))

  if (!is.null(value$type) && !isMissingSapValue(value$type)) {
    validTypes <- schemaTypes(object, version)
    typeValue <- as.character(value$type)
    if (length(typeValue) != 1L || !typeValue %in% validTypes) {
      problems <- c(problems, list(sapProblem(
        joinPath(path, "type"), "unknown_type",
        sprintf("Type '%s' is not defined for %s.", paste(typeValue, collapse = ", "), object)
      )))
    }
  }

  if (!is.null(value$parameters) && is.list(value$parameters)) {
    problems <- c(problems, checkParameters(
      value$parameters, object, value$type, path, version
    ))
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
      problems <- c(problems, list(sapProblem(
        joinPath(path, "content"), "invalid_external_object",
        sprintf("The value must inherit from '%s'.", expectedClass)
      )))
    }
  }

  problems
}

checkParameters <- function(parameters,
                            object,
                            typeId,
                            path,
                            version = currentSapSchemaVersion()) {
  if (is.null(typeId) || isMissingSapValue(typeId)) return(list())
  fields <- schemaFields(object, as.character(typeId), version)
  parameterFields <- fields[grepl("^parameters\\.", fields$path), , drop = FALSE]
  parameterNames <- sub("^parameters\\.", "", parameterFields$path)
  parameterPath <- function(name) joinPath(path, paste0("parameters.", name))

  unknown <- purrr::map(setdiff(names(parameters), parameterNames), function(name) {
    sapProblem(
      parameterPath(name), "unknown_parameter",
      "The parameter is not defined for this type."
    )
  })

  known <- flattenProblems(purrr::map(seq_len(nrow(parameterFields)), function(fieldIndex) {
    field <- parameterFields[fieldIndex, , drop = FALSE]
    parameterName <- sub("^parameters\\.", "", field$path[[1]])
    parameterValue <- parameters[[parameterName]]
    if (isTRUE(field$required[[1]]) && isMissingSapValue(parameterValue)) {
      return(list(sapProblem(
        parameterPath(parameterName), "missing_required_parameter", "A value is required."
      )))
    }
    if (isMissingSapValue(parameterValue)) return(list())
    checkValue(
      parameterValue, field$value_type[[1]], parameterPath(parameterName),
      field$type_id[[1]]
    )
  }))

  c(unknown, known)
}

checkValue <- function(value, valueType, path, typeId = NA_character_) {
  problem <- function(code, message) list(sapProblem(path, code, message))

  if (valueType %in% c("character", "id", "type_id", "datetime")) {
    if (!is.character(value) || length(value) != 1L) {
      return(problem("invalid_value_type", "A single character value is required."))
    }
  } else if (valueType %in% c("character_vector", "id_vector")) {
    if (!is.character(value)) {
      return(problem("invalid_value_type", "A character vector is required."))
    }
  } else if (valueType == "date_range") {
    if (!inherits(value, "Date") || length(value) != 2L) {
      return(problem("invalid_value_type", "A two-element Date vector is required."))
    }
  } else if (valueType == "date") {
    if (!inherits(value, "Date") || length(value) != 1L || is.na(value)) {
      return(problem("invalid_value_type", "A single Date value is required."))
    }
  } else if (valueType == "data_source_description") {
    if (!inherits(value, "data_source_description")) {
      return(problem(
        "invalid_external_object",
        "The value must be a data_source_description object."
      ))
    }
  } else if (valueType == "logic") {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(problem("invalid_value_type", "A single logical value is required."))
    }
  } else if (valueType == "time_point") {
    return(checkChoice(value, TIME_POINTS, path))
  } else if (valueType == "time_interval") {
    return(checkChoice(value, timeIntervalChoices(typeId), path))
  } else if (valueType == "level") {
    return(checkChoice(value, LEVELS, path))
  } else if (valueType == "integer") {
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
      return(problem("invalid_value_type", "A single numeric value is required."))
    }
  } else if (valueType %in% c("age_group", "time_at_risk")) {
    ok <- is.list(value) && all(purrr::map_lgl(value, function(bounds) {
      bounds <- suppressWarnings(as.numeric(unlist(bounds)))
      length(bounds) == 2L && !anyNA(bounds) && bounds[1] >= 0 &&
        bounds[2] >= bounds[1] &&
        (valueType == "time_at_risk" || is.finite(bounds[2]))
    }))
    if (!ok) {
      return(problem("invalid_value_type", paste0(
        "A list of c(lower, upper) is required, 0 <= lower <= upper",
        if (valueType == "age_group") " and upper is finite", "."
      )))
    }
  } else if (valueType == "strata") {
    ok <- is.list(value) && all(purrr::map_lgl(value, function(group) {
      is.character(group) && length(group) > 0L
    }))
    if (!ok) {
      return(problem("invalid_value_type", "A list of character vectors is required."))
    }
  } else if (!valueType %in% c("object", "external_object")) {
    return(problem("unknown_value_type", sprintf(
      "The value_type '%s' is not permitted.", valueType
    )))
  }

  list()
}

checkChoice <- function(value, choices, path) {
  if (!is.character(value) || length(value) != 1L) {
    return(list(sapProblem(
      path, "invalid_value_type", "A single character value is required."
    )))
  }
  if (!value %in% choices) {
    return(list(sapProblem(path, "invalid_value", sprintf(
      "The value must be one of %s.", paste(choices, collapse = ", ")
    ))))
  }
  list()
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
