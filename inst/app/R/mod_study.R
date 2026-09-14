# Study-level metadata that heads the SAP -----------------------------------
#
# Exactly the schema's `study` object: study_id, title, authors, version,
# description. Nothing else is recorded here.

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

study_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      authors <- chr_vec(strsplit(input$authors %||% "", ",", fixed = TRUE)[[1]])
      compact(list(
        study_id    = chr_or_null(input$study_id),
        title       = chr_or_null(input$title),
        authors     = if (length(authors)) authors else NULL,
        version     = chr_or_null(input$version),
        description = chr_or_null(input$description)
      ))
    })

    load <- function(study) {
      shiny::updateTextInput(session, "study_id", value = study$study_id %||% "")
      shiny::updateTextInput(session, "title", value = study$title %||% "")
      shiny::updateTextInput(session, "authors", value = paste(unlist(study$authors), collapse = ", "))
      shiny::updateTextInput(session, "version", value = study$version %||% "v1.0.0")
      shiny::updateTextAreaInput(session, "description", value = study$description %||% "")
    }

    list(data = data, load = load)
  })
}
