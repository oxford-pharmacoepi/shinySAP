# Section 1: CDM Sources -----------------------------------------------------

source_item_ui <- function(id, prefill = NULL) {
  ns <- shiny::NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "CDM source",
    bslib::layout_columns(
      col_widths = c(5, 3, 4),
      shiny::textInput(ns("name"), "Source name", pf("name"), width = "100%"),
      shiny::textInput(ns("source_key"), "Short key", pf("source_key"), width = "100%"),
      shiny::textInput(ns("country"), "Country / region", pf("country"), width = "100%")
    )
  )
}

source_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # What a collapsed card says it is: the source's short key, falling back
    # to the full name when no key is set.
    item_card_label(output, shiny::reactive({
      key <- trimws(input$source_key %||% "")
      nm  <- trimws(input$name %||% "")
      if (nzchar(key)) key else if (nzchar(nm)) nm else "Untitled"
    }))

    shiny::reactive(list(
      name       = blank_to_na(input$name),
      source_key = blank_to_na(input$source_key),
      country    = blank_to_na(input$country)
    ))
  })
}

cdm_sources_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::h3("CDM sources", class = "mb-1"),
        shiny::p(class = "text-muted mb-0", "The databases this study will run against.")
      ),
      shiny::div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        shiny::actionButton(ns("add"), "Add source", class = "btn btn-primary", icon = shiny::icon("plus"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No CDM sources recorded yet.")
    ),
    shiny::div(id = ns("items"))
  )
}

cdm_sources_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    items <- dynamic_items("source", "items", source_item_ui, source_item_server,
                           noun = "CDM source")

    shiny::observeEvent(input$add, items$add(reveal = TRUE))

    output$n <- shiny::renderText(items$count())
    shiny::outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(sources) {
      items$clear()
      for (s in sources) items$add(s)
    }

    # Feeds the data-source pickers in CDM Changes and Analyses.
    # Sources are referred to by short key, falling back to the full name
    # for sources without one.
    names_r <- shiny::reactive({
      nms <- vapply(items$data(), function(x) {
        key <- trimws(as.character(x$source_key %||% ""))
        if (nzchar(key)) key else trimws(as.character(x$name %||% ""))
      }, character(1))
      sort(unique(nms[nzchar(nms)]))
    })

    list(data = items$data, load = load, names = names_r)
  })
}
