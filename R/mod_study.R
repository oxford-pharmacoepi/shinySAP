# Study-level metadata that heads the SAP -----------------------------------

study_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Study information"),
    p(class = "text-muted", "Identifying details for the statistical analysis plan."),
    layout_columns(
      col_widths = c(8, 4),
      textInput(ns("title"), "Study title", width = "100%"),
      textInput(ns("acronym"), "Acronym", width = "100%")
    ),
    layout_columns(
      col_widths = c(6, 3, 3),
      textInput(ns("authors"), "Authors (comma separated)", width = "100%"),
      textInput(ns("version"), "SAP version", value = "1.0", width = "100%"),
      dateInput(ns("date"), "Date", value = Sys.Date(), width = "100%")
    ),
    textAreaInput(ns("background"), "Background", rows = 3, width = "100%"),
    textAreaInput(
      ns("objectives"), "Objectives (one per line)",
      rows = 4, width = "100%",
      placeholder = "Estimate the incidence of X in cohort Y\nCharacterise ..."
    )
  )
}

study_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(list(
      title      = blank_to_na(input$title),
      acronym    = blank_to_na(input$acronym),
      authors    = as_array(trimws(split_lines(gsub(",", "\n", input$authors %||% "")))),
      version    = blank_to_na(input$version),
      date       = as.character(input$date %||% NA),
      background = blank_to_na(input$background),
      objectives = as_array(split_lines(input$objectives))
    ))

    load <- function(study) {
      updateTextInput(session, "title", value = study$title %||% "")
      updateTextInput(session, "acronym", value = study$acronym %||% "")
      updateTextInput(session, "authors", value = paste(unlist(study$authors), collapse = ", "))
      updateTextInput(session, "version", value = study$version %||% "1.0")
      if (!is.null(study$date)) {
        updateDateInput(session, "date", value = as.Date(study$date))
      }
      updateTextAreaInput(session, "background", value = study$background %||% "")
      updateTextAreaInput(session, "objectives", value = join_lines(study$objectives))
    }

    list(data = data, load = load)
  })
}
