
#' Create the study metadata component of a SAP
#'
#' @param studyId Immutable study identifier.
#' @param title Study title.
#' @param authors Character vector of study authors.
#' @param version Study version label.
#' @param description Optional string containing the study description.
#'
#' @return An object of class `sap_study`.
#' @export
newSapStudy <- function(studyId,
                        title = NULL,
                        authors = character(),
                        version = "1.0.0",
                        description = NULL) {
  # input validation
  omopgenerics::assertCharacter(studyId, length = 1)
  omopgenerics::assertCharacter(title, length = 1, null = TRUE)
  omopgenerics::assertCharacter(authors)
  omopgenerics::assertCharacter(version, length = 1)
  if (isTRUE(validate)) {
    assertId(studyId, "studyId")
    assertId(title, "title")
    omopgenerics::assertCharacter(authors, na = FALSE, nm = "authors")
    assertId(version, "version")
    if (!is.null(description)) {
      omopgenerics::assertCharacter(
        description, length = 1, na = FALSE, null = FALSE, empty = FALSE,
        minNumCharacter = 1, nm = "description"
      )
    }
  }
  structure(
    list(
      study_id = studyId,
      title = title,
      authors = authors,
      version = version,
      description = description
    ),
    class = c("sap_study", "list")
  )
}


validateSapStudy <- function(study) {

}
