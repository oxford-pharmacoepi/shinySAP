
# TODO update the name when validateSap have been deprecated
validateSapArgument <- function(sap, call = parent.frame()) {
  if (!inherits(sap, "omop_sap")) {
    cli::cli_abort(c(
      x = "`sap` must be an `omop_sap` object.",
      i = "Use `createSap()` to create a valid `omop_sap` object."
    ), call = call)
  }
  sap
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
