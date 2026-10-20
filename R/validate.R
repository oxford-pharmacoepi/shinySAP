
# TODO update the name when validateSap have been deprecated
.validateSap <- function(sap, call = parent.frame()) {
  # TODO add validation
  return(sap)
}

validateSapSchemaVersion <- function(sapSchemaVersion, call = parent.frame()) {
  if (is.null(sapSchemaVersion)) {
    sapSchemaVersion <- currentSapSchemaVersion()
  }
  omopgenerics::assertChoice(
    sapSchemaVersion,
    choices = supportedSapSchemaVersions(),
    length = 1
  )
}
