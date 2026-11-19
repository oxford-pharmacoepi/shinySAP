
# This should be an independent package at some point:
# https://github.com/oxford-pharmacoepi/DataSourceDescriptions

# TODO
# updateDataSource
# appendDataSource
# removeDataSource

#' Create a Data Source Description
#'
#' @param x A description of a data source or a list of descriptions of data
#' sources
#'
#' @return An object of class `omop_sap_data_source`.
#' @export
#'
#' @examples
#' library(shinySAP)
#'
#' newSapDataSource(list(
#'   administrative_details = list(
#'     name_of_data_source = "Example data source",
#'     data_source_acronym = "EDS"
#'   ),
#'   data_collection = list(
#'     geography = "United Kingdom",
#'     population = "Adults registered in participating practices",
#'     healthcare_setting_type_of_data = "Electronic health records",
#'     data_collection_process = "Data are captured during routine care.",
#'     general_representativeness = "Representative of participating practices.",
#'     data_source_coding = "Clinical events are recorded using source codes.",
#'     source_quality_control = "Source data are checked before release.",
#'     linkage = "No external linkage is included.",
#'     mortality = "Deaths are captured from primary care records.",
#'     source_limitations = "Information recorded outside participating practices may be incomplete."
#'   ),
#'   omop_standardisation = list(
#'     omop_mapping = "Source data are mapped to the OMOP CDM.",
#'     omop_quality_control = "Mapped data are checked using OMOP quality checks."
#'   )
#' ))
#'
#' newSapDataSource(list(
#'   list(
#'     administrative_details = list(
#'       name_of_data_source = "Test 1",
#'       data_source_acronym = "T1"
#'     ),
#'     data_collection = list(),
#'     omop_standardisation = list()
#'   ),
#'   list(
#'     administrative_details = list(
#'       name_of_data_source = "Test 2",
#'       data_source_acronym = "T2"
#'     ),
#'     data_collection = list(),
#'     omop_standardisation = list()
#'   )
#' ))
#'
newSapDataSource <- function(x) {
  omopgenerics::assertList(x)
  if (inherits(x, "omop_sap_data_source")) {
    x <- unclass(x)
  }
  if (is.list(x) && any(names(dataSourceDescriptionFields()) %in% names(x))) {
    x <- list(x)
  }

  x <- validateDataSourceDescription(x)

  return(x)
}

#' Create a SAP data source from descriptive fields
#'
#' @param dataSourceName Full name of the data source. Required, non-empty
#'   character text.
#' @param dataSourceAcronym Acronym identifying the data source. Required,
#'   non-empty character text. Acronyms must be unique when combining sources.
#' @param dataSourceWebsite URL of the data source's website.
#' @param hmaEmaCatalogue URL of the data source's entry in the HMA-EMA
#'   Catalogues of real-world data sources and studies.
#' @param mainReferences Bibliographic references describing the data source.
#' @param geography Geographic coverage of the data source.
#' @param population Description of the population covered by the data source.
#' @param healthcareSettingTypeOfData Healthcare settings and types of data
#'   recorded, such as primary care electronic health records.
#' @param dataCollectionProcess How the source data are collected and processed.
#' @param generalRepresentativeness Representativeness of the source population
#'   relative to the population of interest.
#' @param dataSourceCoding Coding systems used in the source data before OMOP
#'   standardisation.
#' @param sourceQualityControl Quality checks applied to the source data before
#'   OMOP standardisation.
#' @param linkage Available linkages to other data sources and any restrictions.
#' @param mortality How deaths are captured and validated in the source data.
#' @param sourceLimitations Known limitations of the source data and coverage.
#' @param omopQualityControl Quality checks applied after mapping to the OMOP
#'   Common Data Model (CDM).
#' @param omopMapping Description of the mapping to the OMOP CDM and standard
#'   vocabularies, including relevant versions and mapping decisions.
#'
#' @details
#' All arguments accept character text; vectors are collapsed into a single
#' string separated by newlines by [newSapDataSource()]. Optional arguments
#' default to `NULL` and are stored as `NA_character_`.
#'
#' @return An object of class `omop_sap_data_source` containing one description,
#'   named by `dataSourceAcronym`. It can be passed to [createSap()] as
#'   `dataSources` or combined with other sources using [c()].
#' @seealso [newSapDataSource()] for constructing a collection from lists.
#' @export
#'
#' @examples
#' library(shinySAP)
#'
#' source1 <- createSapDataSource(
#'   dataSourceName = "Example data source",
#'   dataSourceAcronym = "EDS",
#'   geography = "United Kingdom",
#'   healthcareSettingTypeOfData = "Primary care electronic health records",
#'   sourceLimitations = "Care outside participating practices may be incomplete.",
#'   omopMapping = "Mapped to OMOP CDM v5.4 and standard vocabularies."
#' )
#' source1
#'
#' source2 <- createSapDataSource(
#'   dataSourceName = "Example data source 2",
#'   dataSourceAcronym = "EDS2"
#' )
#' source2
#'
#' sources <- c(source1, source2)
#' sources
#'
#' sap <- createSap(
#'   study = createStudySap(studyId = "EUPAS1000000000"),
#'   dataSources = sources
#' )
#' sap
#'
createSapDataSource <- function(dataSourceName,
                                dataSourceAcronym,
                                dataSourceWebsite = NULL,
                                hmaEmaCatalogue = NULL,
                                mainReferences = NULL,
                                geography = NULL,
                                population = NULL,
                                healthcareSettingTypeOfData = NULL,
                                dataCollectionProcess = NULL,
                                generalRepresentativeness = NULL,
                                dataSourceCoding = NULL,
                                sourceQualityControl = NULL,
                                linkage = NULL,
                                mortality = NULL,
                                sourceLimitations = NULL,
                                omopQualityControl = NULL,
                                omopMapping = NULL) {
  # Delegate validation and normalisation to the list constructor.
  list(
    administrative_details = list(
      name_of_data_source = dataSourceName,
      data_source_acronym = dataSourceAcronym,
      data_source_website = dataSourceWebsite,
      hma_ema_catalogue = hmaEmaCatalogue,
      main_references = mainReferences
    ),
    data_collection = list(
      geography = geography,
      population = population,
      healthcare_setting_type_of_data = healthcareSettingTypeOfData,
      data_collection_process = dataCollectionProcess,
      general_representativeness = generalRepresentativeness,
      data_source_coding = dataSourceCoding,
      source_quality_control = sourceQualityControl,
      linkage = linkage,
      mortality = mortality,
      source_limitations = sourceLimitations
    ),
    omop_standardisation = list(
      omop_mapping = omopMapping,
      omop_quality_control = omopQualityControl
    )
  ) |>
    newSapDataSource()
}

validateDataSourceDescription <- function(x,
                                          call = parent.frame()) {
  if (length(x) == 0) {
    return(structure(.Data = list(), class = c("omop_sap_data_source", "list")))
  }

  x <- x |>
    purrr::imap(\(description, id) {
      validateSingleDataSourceDescription(description, id, call = call)
    })

  nms <- x |>
    purrr::map_chr(\(description) {
      description$administrative_details$data_source_acronym
    })

  if (length(nms) != length(unique(nms))) {
    cli::cli_abort(c(x = "Data source acronyms must be unique."), call = call)
  }

  names(x) <- nms
  x <- x[order(names(x))]

  # create sap data source
  x <- structure(
    .Data = x,
    class = c("omop_sap_data_source", "list")
  )

  return(x)
}
validateSingleDataSourceDescription <- function(description,
                                                nm,
                                                call = parent.frame()) {
  if (!is.list(description)) {
    cli::cli_abort(c(x = "Data source description {.pkg {nm}} is not a list."), call = call)
  }

  if (!is.character(nm) || is.na(nm) || !nzchar(nm)) {
    nm <- description$administrative_details$data_source_acronym %||% "unknown"
  }

  spec <- dataSourceDescriptionFields()
  missingSections <- setdiff(names(spec), names(description))
  extraSections <- setdiff(names(description), names(spec))

  if (length(missingSections) > 0) {
    cli::cli_abort(c(x = "Missing section in data source {.pkg {nm}}: {.var {missingSections}}."), call = call)
  }
  if (length(extraSections) > 0) {
    cli::cli_abort(c(x = "Unexpected section in data source {.pkg {nm}}: {.var {extraSections}}."), call = call)
  }

  description <- description[names(spec)]

  for (section in names(spec)) {
    description[[section]] <- validateDataSourceDescriptionSection(
      nm = nm,
      section = description[[section]],
      sectionName = section,
      required = spec[[section]]$required,
      optional = spec[[section]]$optional,
      call = call
    )
  }

  return(description)
}
validateDataSourceDescriptionSection <- function(nm,
                                                 section,
                                                 sectionName,
                                                 required,
                                                 optional,
                                                 call = parent.frame()) {
  if (!is.list(section)) {
    cli::cli_abort(c(x = "Section {.var {sectionName}} of {.pkg {nm}} must be a list."), call = call)
  }

  expectedFields <- c(required, optional)
  missingFields <- setdiff(required, names(section))
  extraFields <- setdiff(names(section), expectedFields)

  if (length(missingFields) > 0) {
    cli::cli_abort(
      "Missing field in {.var {sectionName}} of {.pkg {nm}}: {.var {missingFields}}.",
      call = call
    )
  }
  if (length(extraFields) > 0) {
    cli::cli_abort(
      "Unexpected field in {.var {sectionName}} of {.pkg {nm}}: {.var {extraFields}}.",
      call = call
    )
  }

  for (field in expectedFields) {
    fieldName <- paste0(nm, "$", sectionName, "$", field)
    value <- section[[field]]
    fieldIsOptional <- field %in% optional

    if (is.null(value)) {
      if (fieldIsOptional) {
        value <- NA_character_
      } else {
        cli::cli_abort(c(x = "{.var {fieldName}} of {.pkg {nm}} must not be NULL."), call = call)
      }
    }

    minChar <- dataSourceDescriptionMinCharacters(field)
    omopgenerics::assertCharacter(value, na = fieldIsOptional, minNumCharacter = minChar, call = call)
    if (length(value) != 1) {
      value <- paste0(value, collapse = "\n")
    }
    section[[field]] <- value
  }

  return(section[expectedFields])
}
dataSourceDescriptionMinCharacters <- function(field) {
  ifelse(field %in% c("name_of_data_source", "data_source_acronym"), 1, 0)
}
dataSourceDescriptionFields <- function() {
  list(
    administrative_details = list(
      required = c("name_of_data_source", "data_source_acronym"),
      optional = c(
        "data_source_website", "hma_ema_catalogue", "main_references"
      )
    ),
    data_collection = list(
      required = character(),
      optional = c(
        "geography", "population", "healthcare_setting_type_of_data",
        "data_collection_process", "general_representativeness",
        "data_source_coding", "source_quality_control", "linkage",
        "mortality", "source_limitations"
      )
    ),
    omop_standardisation = list(
      required = character(),
      optional = c("omop_mapping", "omop_quality_control")
    )
  )
}

#' @export
print.omop_sap_data_source <- function(x, description = TRUE, ...) {
  cli_title("Data Sources")
  if (length(x) == 0) {
    cli::cli_inform(c("!" = "{.emph No Data Sources included in this study.}"))
  } else {
    fullName <- purrr::map_chr(x, \(x) x$administrative_details$name_of_data_source)
    acronym <- purrr::map_chr(x, \(x) x$administrative_details$data_source_acronym)
    nms <- paste0(fullName,  " ({.strong ", acronym, "})")
    names(nms) <- rep("*", length(nms))
    cli::cli_inform(nms)
  }
  invisible(x)
}

#' @export
`[[<-.omop_sap_data_source` <- function(x, i, value) {
  omopgenerics::assertCharacter(i, length = 1)
  if (!is.null(value)) {
    value <- newSapDataSource(value)
    if (length(value) != 1) {
      cli::cli_abort(c(x = "Only one data source can be assigned at a time."))
    }
    if (i != names(value)) {
      cli::cli_abort(c(x = "Data source named {names(value)} can be assigned to {i}."))
    }
    value <- value[[1]]
  }
  NextMethod()
}

#' @export
`$<-.omop_sap_data_source` <- function(x, i, value) {
  x[[i]] <- value
  x
}

#' @export
`[<-.omop_sap_data_source` <- function(x, i, value) {
  cli::cli_abort(c(x = "Use `$` or `[[` to edit individual data sources."))
}

#' @export
`c.omop_sap_data_source` <- function(...) {
  unname(list(...)) |>
    purrr::map(\(x) unclass(newSapDataSource(x))) |>
    purrr::list_flatten(name_repair = "minimal") |>
    newSapDataSource()
}
