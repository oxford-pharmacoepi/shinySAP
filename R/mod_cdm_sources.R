# Section 1: CDM Sources -----------------------------------------------------

CDM_DATA_TYPES <- c(
  "Electronic health records", "Administrative claims", "Primary care records",
  "Hospital records", "Disease registry", "Biobank", "Survey", "Other"
)

source_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "CDM source",
    layout_columns(
      col_widths = c(5, 3, 4),
      textInput(ns("name"), "Source name", pf("name"), width = "100%"),
      textInput(ns("source_key"), "Short key", pf("source_key"), width = "100%"),
      selectInput(ns("data_type"), "Data type", CDM_DATA_TYPES,
                  selected = pf("data_type", CDM_DATA_TYPES[1]), width = "100%")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      textInput(ns("country"), "Country / region", pf("country"), width = "100%"),
      textInput(ns("custodian"), "Custodian / data partner", pf("custodian"), width = "100%"),
      numericInput(ns("population_size"), "Population size",
                   value = pf("population_size", NULL), width = "100%")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      textInput(ns("cdm_version"), "CDM version", pf("cdm_version"), width = "100%"),
      textInput(ns("vocabulary_version"), "Vocabulary version", pf("vocabulary_version"), width = "100%"),
      textInput(ns("release_date"), "Snapshot / release", pf("release_date"),
                width = "100%", placeholder = "YYYY-MM-DD"),
      textInput(ns("data_lock"), "Data lock point", pf("data_lock"),
                width = "100%", placeholder = "YYYY-MM-DD")
    ),
    layout_columns(
      col_widths = c(6, 6),
      textInput(ns("observation_period_start"), "Observation period start",
                pf("observation_period_start"), width = "100%", placeholder = "YYYY-MM-DD"),
      textInput(ns("observation_period_end"), "Observation period end",
                pf("observation_period_end"), width = "100%", placeholder = "YYYY-MM-DD")
    ),
    textAreaInput(ns("description"), "Description", pf("description"), rows = 2, width = "100%")
  )
}

source_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    reactive(list(
      name                     = blank_to_na(input$name),
      source_key               = blank_to_na(input$source_key),
      data_type                = input$data_type,
      country                  = blank_to_na(input$country),
      custodian                = blank_to_na(input$custodian),
      population_size          = input$population_size %||% NA,
      cdm_version              = blank_to_na(input$cdm_version),
      vocabulary_version       = blank_to_na(input$vocabulary_version),
      release_date             = blank_to_na(input$release_date),
      data_lock                = blank_to_na(input$data_lock),
      observation_period_start = blank_to_na(input$observation_period_start),
      observation_period_end   = blank_to_na(input$observation_period_end),
      description              = blank_to_na(input$description)
    ))
  })
}

cdm_sources_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h3("CDM sources", class = "mb-1"),
        p(class = "text-muted mb-0", "The databases this study will run against.")
      ),
      actionButton(ns("add"), "Add source", class = "btn btn-primary", icon = icon("plus"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No CDM sources recorded yet.")
    ),
    div(id = ns("items"))
  )
}

cdm_sources_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    items <- dynamic_items("source", "items", source_item_ui, source_item_server)

    observeEvent(input$add, items$add())

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(sources) {
      items$clear()
      for (s in sources) items$add(s)
    }

    # Feeds the data-source pickers in CDM Changes and Proposed Analyses.
    names_r <- reactive({
      nms <- vapply(items$data(), function(x) as.character(x$name %||% ""), character(1))
      sort(unique(nms[nzchar(nms)]))
    })

    list(data = items$data, load = load, names = names_r)
  })
}
