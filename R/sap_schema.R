# Internal accessors for the versioned SAP schema ----------------------------

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
