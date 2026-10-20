
#' Current SAP schema version
#'
#' This is the latest supported and recommended SAP schema version
#'
#' @returns The version of the SAP that is supported
#' @export
#'
#' @examples
#' library(shinySAP)
#'
#' currentSapSchemaVersion()
#'
currentSapSchemaVersion <- function() {
  currentVersion
}

#' Supported SAP schema versions
#'
#' These are all the supported SAP schema versions, although the latest is
#' always recommended. See latest with `currentSapSchemaVersion()`. Use
#' `changeSapSchemaVersion()` to change the version of a `sap` object.
#'
#' @returns All versions of the SAP that are supported
#' @export
#'
#' @examples
#' library(shinySAP)
#'
#' supportedSapSchemaVersions()
#'
supportedSapSchemaVersions <- function() {
  names(sapTypes)
}

#' Change the SAP schema version of a sap obejct
#'
#' @inheritParams sapDoc
#' @inheritParams sapSchemaVersionDoc
#'
#' @returns The original `sap` object addapted to the new schema.
#' @export
#'
changeSapSchemaVersion <- function(sap,
                                   sapSchemaVersion = currentSapSchemaVersion()) {
  # input validation
  sap <- .validateSap(sap)
  sapVersion <- validateSapSchemaVersion(sapSchemaVersion)

  # TODO implement version change when multiple SAP versions are supported
  # currently there is just one version supported

  return(sap)
}

#' Get the SAP schema version of a given `sap` object
#'
#' @inheritParams sapDoc
#'
#' @returns The SAP schema version of the `sap` obejct.
#' @export
#'
sapSchemaVersion <- function(sap) {
  # input validation
  sap <- .validateSap(sap)

  return(sap$sap_schema_version)
}
