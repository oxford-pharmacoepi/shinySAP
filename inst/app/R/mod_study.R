# Study-level metadata that heads the SAP -----------------------------------
#
# Exactly the schema's `study` object. The card writes a draft study into the
# SAP through shinySAP::updateStudy() on every edit; nothing is held here.

study_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Study information"),
    shiny::p(class = "text-muted", "Identifying details for the statistical analysis plan."),
    bslib::layout_columns(
      col_widths = c(3, 6, 3),
      shiny::div(
        shiny::textInput(ns("study_id"), "Study id", width = "100%", placeholder = "C1-001"),
        shiny::div(class = "form-text", "Immutable identifier; also names the saved file.")
      ),
      shiny::textInput(ns("title"), "Study title", width = "100%"),
      shiny::textInput(ns("version"), "SAP version", value = "v1.0.0", width = "100%")
    ),
    shiny::textInput(ns("authors"), "Authors (comma separated)", width = "100%"),
    shiny::textAreaInput(ns("description"), "Description", rows = 4, width = "100%")
  )
}

# The study as the inputs show it, for card_reader()'s fallback before the client
# has reported.
study_prefill <- function(study) {
  list(
    study_id    = study$study_id,
    title       = study$title,
    version     = study$version,
    description = study$description,
    authors     = paste(unlist(study$authors), collapse = ", ")
  )
}

study_server <- function(id, sap) {
  shiny::moduleServer(id, function(input, output, session) {
    read <- card_reader(input, study_prefill(shiny::isolate(sap())$study))

    shiny::observe({
      authors <- chr_vec(strsplit(read("authors") %||% "", ",", fixed = TRUE)[[1]])
      study <- shinySAP::newSapStudy(
        studyId     = chr_or_null(read("study_id")),
        title       = chr_or_null(read("title")),
        authors     = authors,
        version     = chr_or_null(read("version")),
        description = chr_or_null(read("description")),
        validate    = FALSE
      )
      sap(shinySAP::updateStudy(shiny::isolate(sap()), study, validate = FALSE))
    })

    # After a load: show the SAP's study. The inputs report the same values
    # back, the observer rebuilds an identical study, and the reactiveVal
    # ignores it.
    refresh <- function() {
      study <- shiny::isolate(sap())$study
      shiny::updateTextInput(session, "study_id", value = study$study_id %||% "")
      shiny::updateTextInput(session, "title", value = study$title %||% "")
      shiny::updateTextInput(session, "authors", value = paste(unlist(study$authors), collapse = ", "))
      shiny::updateTextInput(session, "version", value = study$version %||% "")
      shiny::updateTextAreaInput(session, "description", value = study$description %||% "")
    }

    list(refresh = refresh)
  })
}
