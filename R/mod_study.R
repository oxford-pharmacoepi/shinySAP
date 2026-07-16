# Study-level metadata that heads the SAP -----------------------------------

# One row of the amendment history: what changed in which version, and when.
amendment_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Amendment",
    layout_columns(
      col_widths = c(3, 3, 6),
      textInput(ns("version"), "Version", pf("version"), width = "100%"),
      date_input(ns("date"), "Date", pf("date")),
      div()
    ),
    textAreaInput(ns("description"), "Description of change", pf("description"),
                  rows = 2, width = "100%")
  )
}

amendment_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    item_card_label(output, reactive({
      v <- trimws(input$version %||% "")
      d <- trimws(as.character(input$date %||% ""))
      lab <- paste(c(if (nzchar(v)) paste0("v", v), if (nzchar(d)) d),
                   collapse = " — ")
      if (nzchar(lab)) lab else "Untitled"
    }))

    reactive(list(
      version     = blank_to_na(input$version),
      date        = blank_to_na(as.character(input$date %||% "")),
      description = blank_to_na(input$description)
    ))
  })
}

study_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Study information"),
    p(class = "text-muted", "Identifying details for the statistical analysis plan."),
    layout_columns(
      col_widths = c(8, 4),
      textInput(ns("title"), "Study title", width = "100%"),
      textInput(ns("study_code"), "Study code", width = "100%")
    ),
    layout_columns(
      col_widths = c(6, 3, 3),
      textInput(ns("authors"), "Authors (comma separated)", width = "100%"),
      textInput(ns("version"), "SAP version", value = "1.0", width = "100%"),
      dateInput(ns("date"), "Date", value = Sys.Date(), width = "100%")
    ),
    textAreaInput(ns("background"), "Rationale and background", rows = 4, width = "100%"),
    textAreaInput(
      ns("aim"), "Aim / research question",
      rows = 2, width = "100%",
      placeholder = "The aim of this study is to ..."
    ),
    textAreaInput(
      ns("objectives"), "Specific objectives (one per line)",
      rows = 4, width = "100%",
      placeholder = "Estimate the incidence of X in cohort Y\nCharacterise ..."
    ),
    div(
      class = "d-flex justify-content-between align-items-center mt-4 mb-3",
      div(
        h5("Amendment history", class = "mb-1"),
        p(class = "text-muted mb-0", "What changed in each version of this SAP.")
      ),
      actionButton(ns("add_amendment"), "Add amendment",
                   class = "btn btn-outline-primary btn-sm", icon = icon("plus"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n_amendments")),
      empty_state("No amendments recorded yet.")
    ),
    div(id = ns("amendments"))
  )
}

study_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    amendments <- dynamic_items("amendment", "amendments",
                                amendment_item_ui, amendment_item_server)

    # A new amendment is the next version, made today. Both stay editable.
    observeEvent(input$add_amendment, amendments$add(list(
      version = next_sap_version(input$version),
      date    = as.character(Sys.Date())
    )))

    output$n_amendments <- renderText(amendments$count())
    outputOptions(output, "n_amendments", suspendWhenHidden = FALSE)

    data <- reactive(list(
      title      = blank_to_na(input$title),
      study_code = blank_to_na(input$study_code),
      authors    = as_array(trimws(split_lines(gsub(",", "\n", input$authors %||% "")))),
      version    = blank_to_na(input$version),
      date       = as.character(input$date %||% NA),
      background = blank_to_na(input$background),
      aim        = blank_to_na(input$aim),
      objectives = as_array(split_lines(input$objectives)),
      amendments = amendments$data()
    ))

    load <- function(study) {
      updateTextInput(session, "title", value = study$title %||% "")
      updateTextInput(session, "study_code", value = study$study_code %||% "")
      updateTextInput(session, "authors", value = paste(unlist(study$authors), collapse = ", "))
      updateTextInput(session, "version", value = study$version %||% "1.0")
      if (!is.null(study$date)) {
        updateDateInput(session, "date", value = as.Date(study$date))
      }
      updateTextAreaInput(session, "background", value = study$background %||% "")
      updateTextAreaInput(session, "aim", value = study$aim %||% "")
      updateTextAreaInput(session, "objectives", value = join_lines(study$objectives))
      amendments$clear()
      for (a in study$amendments %||% list()) amendments$add(a)
    }

    list(data = data, load = load)
  })
}
