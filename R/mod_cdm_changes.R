# Section 1: CDM Changes -----------------------------------------------------

CDM_CHANGE_TYPES <- c(
  "Add table", "Add field", "Modify field", "Remove field",
  "Vocabulary / mapping update", "Source data addition", "ETL fix", "Other"
)

cdm_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "CDM change",
    layout_columns(
      col_widths = c(4, 4, 4),
      textInput(ns("cdm_table"), "CDM table", pf("cdm_table"), width = "100%"),
      textInput(ns("cdm_field"), "Field / column", pf("cdm_field"), width = "100%"),
      selectInput(
        ns("change_type"), "Change type", CDM_CHANGE_TYPES,
        selected = pf("change_type", CDM_CHANGE_TYPES[1]), width = "100%"
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      textInput(ns("cdm_version"), "CDM version affected", pf("cdm_version"), width = "100%"),
      entity_picker(ns("data_source"), "Data source", pf("data_source"),
                    placeholder = "Select or type a CDM source")
    ),
    textAreaInput(ns("description"), "Description of change", pf("description"), rows = 3, width = "100%"),
    textAreaInput(ns("rationale"), "Rationale", pf("rationale"), rows = 2, width = "100%")
  )
}

cdm_item_server <- function(id, prefill = NULL, on_remove = function() {},
                            source_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    sync_pickers(session, "data_source", source_names, prefiller(prefill))

    # A CDM change has no name -- what identifies it is the column it touches.
    item_card_label(output, reactive({
      parts <- c(trimws(input$cdm_table %||% ""), trimws(input$cdm_field %||% ""))
      parts <- parts[nzchar(parts)]
      if (length(parts)) paste(parts, collapse = ".") else "Untitled"
    }))

    reactive(list(
      cdm_table   = blank_to_na(input$cdm_table),
      cdm_field   = blank_to_na(input$cdm_field),
      change_type = input$change_type,
      cdm_version = blank_to_na(input$cdm_version),
      data_source = blank_to_na(input$data_source),
      description = blank_to_na(input$description),
      rationale   = blank_to_na(input$rationale)
    ))
  })
}

cdm_changes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h3("CDM changes", class = "mb-1"),
        p(class = "text-muted mb-0", "Changes to the common data model this study depends on.")
      ),
      div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        actionButton(ns("add"), "Add change", class = "btn btn-primary", icon = icon("plus"))
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No CDM changes recorded yet.")
    ),
    div(id = ns("items"))
  )
}

cdm_changes_server <- function(id, source_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    settled_names <- debounce(source_names, 600)

    item_server <- function(iid, prefill, on_remove) {
      cdm_item_server(iid, prefill, on_remove, settled_names)
    }
    items <- dynamic_items("cdm", "items", cdm_item_ui, item_server)

    observeEvent(input$add, items$add())

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(changes) {
      items$clear()
      for (ch in changes) items$add(ch)
    }

    list(data = items$data, load = load)
  })
}
