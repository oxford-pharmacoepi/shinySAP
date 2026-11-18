
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
#'
#' @examples
#' library(shinySAP)
#'
#' newSapStudy("EUPAS00000000000")
#'
#' study <- newSapStudy(
#'   studyId = "EUPAS00000000000",
#'   title = "Effectiveness of drug A",
#'   authors = c("Martí Català", "Shihao Shenzhang", "Niko Moller-Grell"),
#'   version = "1.2.10",
#'   description = "This study assesses the effectiveness of drug A in the UK."
#' )
#'
#' study
#'
#' print(study, description = FALSE)
#'
newSapStudy <- function(studyId,
                        title = NULL,
                        authors = character(),
                        version = "1.0.0",
                        description = NULL) {
  # input validation
  studyValidations$study_id(studyId)
  studyValidations$title(title)
  studyValidations$authors(authors)
  studyValidations$version(version)
  studyValidations$description(description)

  structure(
    list(
      study_id = studyId,
      title = title,
      authors = authors,
      version = version,
      description = description
    ),
    class = c("omop_sap_study", "list")
  )
}

studyValidations <- list(
  study_id = \(studyId) omopgenerics::assertCharacter(studyId, length = 1),
  title = \(title) omopgenerics::assertCharacter(title, length = 1, null = TRUE),
  authors = \(authors) omopgenerics::assertCharacter(authors),
  version = \(version) {
    omopgenerics::assertCharacter(version, length = 1)
    if (!grepl("^[0-9.-]+$", version)) {
      cli::cli_abort(c(x = "{.arg version} can only contain numbers and `.` or `-`."))
    }
  },
  description = \(description) omopgenerics::assertCharacter(description, length = 1, null = TRUE)
)

#' @export
print.omop_sap_study <- function(x, description = TRUE, ...) {
  x <- x |>
    purrr::imap(\(x, nm) {
      if (length(x) == 0) {
        x <- "─"
      }
      if (nm == "version") {
        x <- paste0("‘", as.character(x), "’")
      } else if (nm == "authors") {
        x <- paste0(x, collapse = " & ")
      }
      return(x)
    })
  cli_title("OMOP Study metadata")
  cli::cli_inform(c(
    "*" = paste0("{.emph Study ID}: {.strong ", x$study_id, "}"),
    "*" = paste0("{.emph Title}: ", x$title),
    "*" = paste0("{.emph Authors}: ", x$authors),
    "*" = paste0("{.emph Version}: ", x$version),
    if (description) c("*" = paste0("{.emph Description}: ", x$description))
  ))
  invisible(x)
}
cli_title <- function(title) {
  width <- max(0, cli::console_width() - nchar(title) - 7)
  cli::cli_inform(paste0("── {.pkg ", title, "} ──", strrep('─', width)))
}

#' @export
`[[<-.omop_sap_study` <- function(x, i, value) {
  omopgenerics::assertCharacter(i, length = 1)
  if (i %in% names(studyValidations)) {
    studyValidations[[i]](value)
  } else {
    cli::cli_abort(c(x = "Only fields {names(studyValidations)} can be edited."))
  }
  if (is.null(value)) {
    fields <- unclass(x)
    fields[i] <- list(NULL)
    return(structure(fields, class = class(x)))
  }
  NextMethod()
}

#' @export
`$<-.omop_sap_study` <- function(x, i, value) {
  x[[i]] <- value
  x
}

#' @export
`[<-.omop_sap_study` <- function(x, i, value) {
  cli::cli_abort(c(x = "Use `$` or `[[` to edit individual study fields."))
}
