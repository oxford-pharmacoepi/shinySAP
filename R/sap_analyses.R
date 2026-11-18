#' Create a SAP analysis component
#'
#' @param id Immutable analysis identifier.
#' @param name Display name of the analysis.
#' @param dataSourceId Identifier of the data source(s) used by the analysis.
#' @param type Analysis type.
#' @param parameters Type-specific analysis parameters.
#' @inheritParams createStudySap
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
