
#' Create the study metadata component of a SAP
#'
#' @param studyId Immutable study identifier.
#' @param title Study title.
#' @param authors Character vector of study authors.
#' @param version Study version label.
#' @param description Optional study description.
#' @param validate Whether to require a complete study. With `FALSE` a draft is
#'   built from whatever is given and [checkSap()] reports what is missing.
#'
#' @return An object of class `sap_study`.
#' @export
newSapStudy <- function(studyId = NULL,
                        title = NULL,
                        authors = character(),
                        version = "v1.0.0",
                        description = NULL,
                        validate = TRUE) {
  omopgenerics::assertCharacter(studyId)
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

createStudySap <- function() {

}
