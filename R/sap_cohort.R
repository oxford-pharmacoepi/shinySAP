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
