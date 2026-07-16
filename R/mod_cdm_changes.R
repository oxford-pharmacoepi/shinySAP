# Section 1: CDM Changes -----------------------------------------------------

# The changes a SAP applies to the CDM before analysis: extra validations, the
# common database-specific alterations, and the standard person-cleaning steps.
# The last entry is the catch-all, and where legacy change types land on load.
CDM_CHANGE_TYPES <- c(
  "Extra CDM validation",
  "Subset a CDM table",
  "Limit observation periods",
  "Remap concepts / fix vocabulary mappings",
  "Remove people with no year of birth or sex data",
  "Remove people with implausible dates",
  "Other database-specific alteration"
)

cdm_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "CDM change",
    layout_columns(
      col_widths = c(6, 6),
      selectInput(
        ns("change_type"), "Type of change", CDM_CHANGE_TYPES,
        selected = pf("change_type", CDM_CHANGE_TYPES[1]), width = "100%"
      ),
      entity_picker(ns("data_sources"), "Data sources", pf("data_sources", character(0)),
                    multiple = TRUE, placeholder = "Select or type CDM sources")
    ),
    textAreaInput(
      ns("description"), "Description of change", pf("description"),
      rows = 3, width = "100%",
      placeholder = "e.g. restrict drug_exposure to records with quantity > 0"
    )
  )
}

cdm_item_server <- function(id, prefill = NULL, on_remove = function() {},
                            source_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    sync_pickers(session, "data_sources", source_names, prefiller(prefill))

    # A CDM change has no name -- what identifies it is its type.
    item_card_label(output, reactive(input$change_type %||% "Untitled"))

    reactive(list(
      change_type  = input$change_type,
      data_sources = as_array(input$data_sources %||% character(0)),
      description  = blank_to_na(input$description)
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
        p(class = "text-muted mb-0",
          "Extra validations and database-specific alterations applied to the CDM before analysis.")
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
