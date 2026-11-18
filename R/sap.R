
#' Create a structured Statistical Analysis Plan
#'
#' @param x A named R list containing the SAP fields.
#' @inheritParams sapSchemaVersionDoc
#' @param validate Whether to validate the complete SAP before returning it.
#'
#' @return An object of class `sap`.
#' @export
#'
newSap <- function(x,
                   sapSchemaVersion = currentSapSchemaVersion(),
                   validate = TRUE) {
  # input validation
  omopgenerics::assertList(x, named = TRUE)
  sapSchemaVersion <- validateSapSchemaVersion(sapSchemaVersion)
  omopgenerics::assertLogical(validate, length = 1)

  sap <- constructSap(x, version = sapSchemaVersion)

  if (validate) {
    # TODO
    #validateSap(sap)
  }

  sap
}

constructSap <- function(x,
                         version,
                         call = parent.frame()) {

  if (is.null(x$sap_schema_version)) {
    x$sap_schema_version <- version
  }
  if (!identical(as.character(x$sap_schema_version), version)) {
    cli::cli_abort(c(x = paste0("sap_schema_version must be ", version, ".")))
  }
  if (is.null(x$generated_at)) {
    x$generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  }

  # names
  nms <- names(x)
  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0) {
    cli::cli_abort(c(x = "Duplicated fields not allowed in {.cls omop_sap}: {.var {dup}}."))
  }
  notPresent <- setdiff(sapComponents, nms)
  if (length(notPresent) > 0) {
    cli::cli_abort(c(x = "Missing compulsory fields in {.cls omop_sap}: {.var {notPresent}}."))
  }
  extraFields <- setdiff(nms, sapComponents)
  if (length(extraFields) > 0) {
    cli::cli_abort(c(x = "Extra not allowed fields in {.cls omop_sap}: {.var {extraFields}}."))
  }

  x <- structure(x, class = c("omop_sap", "list"))

  # components so we trigger validation
  for (nm in sapComponents) {
    x[[nm]] <- x[[nm]]
  }

  return(x)
}

#' Create a complete SAP from its components
#'
#' @param study An `omop_sap_study` object.
#' @param dataSources List of `sap_data_source` objects.
#' @param dataSourceModifications List of `sap_data_source_modification` objects.
#' @param codelists List of `sap_codelist` objects.
#' @param cohorts List of `sap_cohort` objects.
#' @param analyses List of `sap_analysis` objects.
#' @param validate Whether to validate the complete SAP. With `FALSE` the
#'   document is assembled from drafts and [checkSap()] reports its problems.
#'
#' @return An object of class `sap`.
#' @export
#'
#' @examples
#' library(shinySAP)
#' library(omopgenerics)
#'
#' # create study metadata
#' study <- createStudySap(studyId = "EUPAS00000000")
#'
#' # create codelists
#' codelists <- newCodelist(list(
#'   acetaminophen = c(1125315L, 1127078L, 1127433L, 19133768L, 40162522L)
#' ))
#'
#' sap <- createSap(
#'   study = study,
#'   codelists = codelists
#' )
#'
#' sap
#'
createSap <- function(study,
                      dataSources = list(),
                      dataSourceModifications = list(),
                      codelists = list(),
                      cohorts = list(),
                      analyses = list(),
                      validate = TRUE) {
  # input validation is done inside `newSap()`
  list(
    study = study,
    data_sources = dataSources,
    data_source_modifications = dataSourceModifications,
    codelists = codelists,
    cohorts = cohorts,
    analyses = analyses
  ) |>
    newSap(validate = validate)
}

#' @export
print.omop_sap <- function(x, ...) {
  print(x$study, description = FALSE)
  print(x$data_sources)
  print(x$codelists)

  invisible(x)
}

#' @export
`[<-.omop_sap` <- function(x, i, value) {
  cli::cli_abort(c(x = "Use `$` or `[[` to edit individual sap fields."))
}

#' @export
`[[<-.omop_sap` <- function(x, i, value) {
  omopgenerics::assertChoice(i, sapComponents, length = 1)
  if (i == "codelists") {
    value <- prepareCodelists(value)
  } else if (i == "study") {
    if (!inherits(value, "omop_sap_study")) {
      value <- tryCatch({newStudySap(value)}, error = \(e) NULL)
      if (is.null(value)) {
        cli::cli_abort(c(
          x = "You can only assign to <study> a valid {.cls omop_sap_study}.",
          i = "Please use `newStudySap()` or `createStudySap()` to create a new study metadata object."
        ))
      }
    }
  } else if (i == "data_sources") {
    if (!inherits(value, "omop_sap_data_source")) {
      value <- tryCatch({newSapDataSource(value)}, error = \(e) NULL)
      if (is.null(value)) {
        cli::cli_abort(c(
          x = "You can only assign to <data_sources> a valid {.cls omop_sap_data_source}.",
          i = "Please use `newSapDataSource()` to create a data source description object."
        ))
      }
    }
  }

  # diagnostics
  # data_source_modifications = dataSourceModifications,
  # cohorts = cohorts,
  # analyses = analyses
  NextMethod()
}

#' @export
`$<-.omop_sap` <- function(x, i, value) {
  x[[i]] <- value
  return(x)
}
