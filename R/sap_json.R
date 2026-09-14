# JSON read/write for a SAP -----------------------------------------------------
#
# The file a SAP is saved to is the package schema, nothing more: every key is a
# schema path, every value is encoded by its schema value_type. Both directions
# are driven by schemaFields(), so a row added to schema.csv is serialised
# without touching this file. Conventions:
#
#   * dates are ISO strings; a date_range with one NA bound writes null for that
#     bound, a range with both bounds NA is omitted;
#   * age_group / time_at_risk are arrays of [lower, upper] pairs; an infinite
#     upper bound is null inside the pair and reads back as Inf;
#   * a scalar integer field set to Inf (washout, follow_up_days, ...) is
#     omitted: every such field's estimator default IS Inf, so the generated
#     call is the same either way;
#   * optional fields that are NULL are omitted, never written as null; an
#     empty `parameters` object is {};
#   * character_vector / id_vector / each strata group is always an array, even
#     with one element;
#   * codelist `content` is encoded by the codelist `type`: a codelist is
#     {"set": [concept ids]}, a codelist_with_details or concept_set_expression
#     is {"set": [ {row}, ... ]}; they read back through the omopgenerics
#     constructors, so the object that comes out has the class the schema wants;
#   * keys the schema does not define are passed through untouched, so
#     checkSap() can report them as unknown_field after a read.

#' Serialise a SAP to JSON
#'
#' @param sap A `sap` object or a named list in SAP shape.
#' @param pretty Whether to indent the output.
#' @return A JSON string (class `json`).
#' @export
sapToJson <- function(sap, pretty = TRUE) {
  omopgenerics::assertList(sap, named = TRUE, nm = "sap")
  version <- as.character(sap$sap_schema_version %||% currentSapSchemaVersion())
  if (is.null(sapSchema[[version]])) {
    cli::cli_abort(c(x = paste0("Unknown SAP schema version: ", version, ".")))
  }
  jsonlite::toJSON(
    encodeSap(unclass(sap), version),
    auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = pretty
  )
}

#' Write a SAP to a JSON file
#'
#' @inheritParams sapToJson
#' @param path File to write; its directory is created if needed.
#' @return The path, invisibly.
#' @export
writeSap <- function(sap, path) {
  omopgenerics::assertCharacter(path, length = 1, na = FALSE, nm = "path")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(sapToJson(sap), path, useBytes = TRUE)
  invisible(path)
}

#' Read a SAP from JSON
#'
#' @param json A JSON string.
#' @param validate Whether to run [validateSap()] on the result. The default is
#'   `FALSE` so that a work-in-progress SAP can be loaded and its problems
#'   reported with [checkSap()].
#' @return A `sap` object.
#' @export
sapFromJson <- function(json, validate = FALSE) {
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if (!is.list(x) || is.null(names(x))) {
    cli::cli_abort(c(x = "A SAP file must hold a JSON object."))
  }
  version <- as.character(x$sap_schema_version %||% "")
  if (nzchar(version) && !is.null(sapSchema[[version]])) {
    x <- decodeSap(x, version)
  }
  # An unknown version is left undecoded: newSap() aborts with the version
  # message, which is the error the reader should see.
  newSap(x, validate = validate)
}

#' Read a SAP from a JSON file
#'
#' @inheritParams sapFromJson
#' @param path File to read.
#' @return A `sap` object.
#' @export
readSap <- function(path, validate = FALSE) {
  omopgenerics::assertCharacter(path, length = 1, na = FALSE, nm = "path")
  if (!file.exists(path)) {
    cli::cli_abort(c(x = paste0("File not found: ", path)))
  }
  json <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  sapFromJson(json, validate = validate)
}

# Encoding ---------------------------------------------------------------------

encodeSap <- function(x, version) {
  sapFields <- schemaFields("sap", version = version)
  # Schema order first (version, generated_at, study, collections), then any
  # key the schema does not know, in the order it was given.
  known <- sapFields$path[order(sapFields$order)]
  keys <- c(intersect(known, names(x)), setdiff(names(x), known))
  out <- list()
  for (name in keys) {
    value <- x[[name]]
    field <- sapFields[sapFields$path == name, , drop = FALSE]
    if (nrow(field) && field$node_type[[1]] == "collection") {
      object <- SAP_COLLECTIONS[[name]]
      out[[name]] <- lapply(unname(value %||% list()), function(item) {
        encodeObject(item, object, version)
      })
    } else if (identical(name, "study")) {
      out[[name]] <- encodeObject(value, "study", version)
    } else if (!isDroppedValue(value)) {
      out[[name]] <- if (nrow(field)) encodeValue(value, field$value_type[[1]], NULL) else value
    }
  }
  out
}

encodeObject <- function(value, object, version) {
  if (!is.list(value)) return(value)
  value <- unclass(value)
  typeId <- if (length(value$type) == 1L) as.character(value$type) else NULL
  fields <- schemaFields(object, typeId, version)
  out <- list()
  for (name in names(value)) {
    v <- value[[name]]
    if (identical(name, "parameters")) {
      out[[name]] <- encodeParameters(v, fields, typeId)
      next
    }
    if (isDroppedValue(v)) next
    field <- fields[fields$path == name, , drop = FALSE]
    out[[name]] <- if (nrow(field)) encodeValue(v, field$value_type[[1]], typeId) else v
  }
  out
}

encodeParameters <- function(parameters, fields, typeId) {
  parameters <- unclass(parameters %||% list())
  out <- structure(list(), names = character(0))   # {} not []
  for (name in names(parameters)) {
    v <- parameters[[name]]
    if (isDroppedValue(v)) next
    field <- fields[fields$path == paste0("parameters.", name), , drop = FALSE]
    encoded <- if (nrow(field)) encodeValue(v, field$value_type[[1]], typeId) else v
    if (is.null(encoded)) next
    out[[name]] <- encoded
  }
  out
}

# NULL and zero-length atomics are "not authored" and are left out of the file.
isDroppedValue <- function(value) {
  is.null(value) || (length(value) == 0L && !is.list(value))
}

encodeValue <- function(value, valueType, typeId) {
  switch(
    valueType,
    character = , id = , type_id = , datetime = , time_point = ,
    time_interval = , level = as.character(value),
    character_vector = , id_vector = I(as.character(value)),
    logic = as.logical(value),
    integer = if (is.infinite(value)) NULL else as.numeric(value),
    date = format(as.Date(value), "%Y-%m-%d"),
    date_range = encodeDateRange(value),
    age_group = , time_at_risk = lapply(value, function(bounds) {
      bounds <- as.numeric(unlist(bounds))
      bounds[is.infinite(bounds)] <- NA_real_
      I(bounds)
    }),
    strata = lapply(value, function(group) I(as.character(group))),
    external_object = encodeExternalObject(value, typeId),
    data_source_description = unclass(value),
    value
  )
}

encodeDateRange <- function(value) {
  value <- as.Date(value)
  if (all(is.na(value))) return(NULL)
  I(format(value, "%Y-%m-%d"))
}

encodeExternalObject <- function(value, typeId) {
  value <- unclass(value)
  switch(
    as.character(typeId %||% ""),
    codelist = lapply(value, function(ids) I(as.integer(ids))),
    codelist_with_details = , concept_set_expression =
      lapply(value, function(tbl) as.data.frame(tbl, stringsAsFactors = FALSE)),
    value
  )
}

# Decoding ---------------------------------------------------------------------

decodeSap <- function(x, version) {
  sapFields <- schemaFields("sap", version = version)
  for (name in names(x)) {
    value <- x[[name]]
    field <- sapFields[sapFields$path == name, , drop = FALSE]
    if (nrow(field) && field$node_type[[1]] == "collection") {
      object <- SAP_COLLECTIONS[[name]]
      if (is.list(value)) {
        x[[name]] <- lapply(value, function(item) decodeObject(item, object, version))
      }
    } else if (identical(name, "study")) {
      x[[name]] <- decodeObject(value, "study", version)
    } else if (nrow(field) && !is.null(value)) {
      x[[name]] <- decodeValue(value, field$value_type[[1]], NULL)
    }
  }
  x
}

decodeObject <- function(value, object, version) {
  if (!is.list(value) || is.null(names(value))) return(value)
  typeId <- if (length(value$type) == 1L) as.character(value$type) else NULL
  fields <- schemaFields(object, typeId, version)
  for (name in names(value)) {
    v <- value[[name]]
    if (identical(name, "parameters")) {
      value[[name]] <- decodeParameters(v, fields, typeId)
      next
    }
    if (is.null(v)) next
    field <- fields[fields$path == name, , drop = FALSE]
    if (nrow(field)) value[[name]] <- decodeValue(v, field$value_type[[1]], typeId)
  }
  value
}

decodeParameters <- function(parameters, fields, typeId) {
  if (!is.list(parameters)) return(list())
  for (name in names(parameters)) {
    v <- parameters[[name]]
    if (is.null(v)) next
    field <- fields[fields$path == paste0("parameters.", name), , drop = FALSE]
    if (nrow(field)) parameters[[name]] <- decodeValue(v, field$value_type[[1]], typeId)
  }
  parameters
}

decodeValue <- function(value, valueType, typeId) {
  switch(
    valueType,
    character = , id = , type_id = , datetime = , time_point = ,
    time_interval = , level = as.character(unlist(value)),
    character_vector = , id_vector = as.character(unlist(value)),
    logic = as.logical(unlist(value)),
    integer = as.numeric(unlist(value)),
    date = as.Date(as.character(unlist(value))),
    date_range = as.Date(vapply(value, function(bound) {
      if (is.null(bound)) NA_character_ else as.character(bound)
    }, character(1))),
    age_group = , time_at_risk = lapply(value, function(bounds) {
      lower <- bounds[[1]]
      upper <- if (length(bounds) > 1L) bounds[[2]] else NULL
      c(if (is.null(lower)) NA_real_ else as.numeric(lower),
        if (is.null(upper)) Inf else as.numeric(upper))
    }),
    strata = lapply(value, function(group) as.character(unlist(group))),
    external_object = decodeExternalObject(value, typeId),
    data_source_description = structure(value, class = "data_source_description"),
    value
  )
}

decodeExternalObject <- function(value, typeId) {
  if (!is.list(value)) return(value)
  switch(
    as.character(typeId %||% ""),
    codelist = omopgenerics::newCodelist(
      lapply(value, function(ids) as.integer(unlist(ids)))
    ),
    codelist_with_details = omopgenerics::newCodelistWithDetails(
      lapply(value, rowsToTibble)
    ),
    concept_set_expression = omopgenerics::newConceptSetExpression(
      lapply(value, rowsToTibble)
    ),
    value
  )
}

# jsonlite writes a data frame as an array of row objects; this is the inverse.
rowsToTibble <- function(rows) {
  if (!length(rows)) return(dplyr::tibble())
  columns <- unique(unlist(lapply(rows, names)))
  dplyr::as_tibble(lapply(stats::setNames(columns, columns), function(column) {
    unlist(lapply(rows, function(row) {
      cell <- row[[column]]
      if (is.null(cell)) NA else cell
    }))
  }))
}
