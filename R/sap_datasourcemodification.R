#' Create a data-source modification component
#'
#' @param id Immutable modification identifier.
#' @param name Display name of the modification.
#' @param type Data-source modification type.
#' @param dataSourceId Identifier of the affected data source(s).
#' @param parameters Type-specific modification parameters.
#' @inheritParams newSapStudy
#'
#' @return An object of class `sap_data_source_modification`.
#' @export
newSapDataSourceModification <- function(id,
                                         name = NULL,
                                         type = NULL,
                                         dataSourceId = character(),
                                         parameters = list(),
                                         validate = TRUE) {
  assertId(id)
  omopgenerics::assertList(parameters, named = TRUE, nm = "parameters")
  if (isTRUE(validate)) {
    assertId(name, "name")
    omopgenerics::assertChoice(
      type, schemaTypes("data_source_modification"), length = 1,
      na = FALSE, null = FALSE, empty = FALSE, nm = "type"
    )
    omopgenerics::assertCharacter(dataSourceId, na = FALSE, nm = "dataSourceId")
    parameters <- validateParameters(parameters, "data_source_modification", type)
  }
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
