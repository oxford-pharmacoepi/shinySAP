# Section: Data sources -------------------------------------------------------
#
# The databases the study runs against: the schema's `data_source` is an id and
# a name (its optional `description` is an external object this app does not
# author). Every card is a draft shinySAP::newSapDataSource() written back
# through the section's CRUD.

source_item_ui <- function(id, prefill = NULL) {
  ns <- shiny::NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Data source",
    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::textInput(ns("name"), "Source name", pf("name"), width = "100%",
                       placeholder = "CPRD GOLD"),
      shiny::div(
        shiny::tags$label(class = "form-label", "Id"),
        shiny::div(class = "form-control-plaintext font-monospace", pf("id"))
      )
    )
  )
}

source_item_server <- function(id, sap_id, prefill = NULL, on_remove = function() {},
                               write = function(component) {}) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    read <- card_reader(input, prefill)

    item_card_label(output, shiny::reactive({
      nm <- trimws(input$name %||% "")
      paste0(if (nzchar(nm)) nm else "Untitled", " -- ", sap_id)
    }))

    shiny::observe(write(shinySAP::newSapDataSource(
      sap_id,
      name        = chr_or_null(read("name")),
      description = prefill$.component$description,
      validate    = FALSE
    )))
  })
}

data_sources_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::h3("Data sources", class = "mb-1"),
        shiny::p(class = "text-muted mb-0", "The databases this study will run against.")
      ),
      shiny::div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        shiny::actionButton(ns("add"), "Add data source", class = "btn btn-primary",
                            icon = shiny::icon("plus"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No data sources recorded yet.")
    ),
    shiny::div(id = ns("items"))
  )
}

data_sources_server <- function(id, sap) {
  shiny::moduleServer(id, function(input, output, session) {
    coll <- sap_collection(
      sap, "data_sources",
      new_component = function(id) shinySAP::newSapDataSource(id, validate = FALSE),
      copy = function(source, id) shinySAP::newSapDataSource(
        id, name = copy_name(source$name), description = source$description, validate = FALSE)
    )
    item_server <- function(iid, sap_id, prefill, on_remove) {
      source_item_server(iid, sap_id, prefill, on_remove,
                         write = function(component) if (items$is_live(iid, sap_id)) coll$write(component))
    }
    items <- dynamic_items("source", "items", source_item_ui, item_server,
                           ids = coll$ids, prefill_of = coll$prefill_of, noun = "Data source",
                           on_remove = coll$remove,
                           on_duplicate = function(id) items$reveal(coll$duplicate(id)),
                           on_undo = coll$undo)

    shiny::observeEvent(input$add, items$reveal(coll$add()))

    output$n <- shiny::renderText(items$count())
    shiny::outputOptions(output, "n", suspendWhenHidden = FALSE)

    list(reset = items$reset)
  })
}

# A collection's components as picker choices: label -> id. An unnamed item is
# shown by its id, so it can still be picked.
item_choices <- function(items) {
  items <- Filter(function(x) !is.null(x$id), items %||% list())
  if (!length(items)) return(character(0))
  ids <- vapply(items, function(x) as.character(x$id), character(1))
  labels <- vapply(items, function(x) {
    nm <- trimws(as.character(x$name %||% ""))
    if (nzchar(nm)) nm else as.character(x$id)
  }, character(1))
  stats::setNames(ids, labels)
}
