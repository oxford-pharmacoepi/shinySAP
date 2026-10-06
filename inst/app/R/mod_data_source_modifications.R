# Section: Data source modifications -----------------------------------------
#
# What the study does to a database before analysis, typed by the schema
# (data_source_modification types, each with its own parameters). The type
# registry is inline: with one type today it is not worth a file per type, but
# the ui/collect/flatten shape is the same as the cohort and analysis registries.

MODIFICATION_TEMPLATES <- list(
  trim_observation_period = list(
    hint = "Observation periods are trimmed to this date range before anything else runs.",
    ui = function(ns, pf) bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::div(
        date_input(ns("date_range_start"), "Trim to: earliest start", pf("date_range_start")),
        shiny::div(class = "form-text", "Blank = no lower bound.")
      ),
      shiny::div(
        date_input(ns("date_range_end"), "Trim to: latest end", pf("date_range_end")),
        shiny::div(class = "form-text", "Blank = no upper bound.")
      )
    ),
    collect = function(input) compact(list(
      date_range = date_range_value(input$date_range_start, input$date_range_end)
    )),
    flatten = function(p) {
      list(date_range_start = date_bound(p$date_range, 1),
           date_range_end   = date_bound(p$date_range, 2))
    }
  )
)

modification_template <- function(type) {
  MODIFICATION_TEMPLATES[[as.character(type %||% "")]] %||%
    list(hint = NULL, ui = function(ns, pf) NULL, collect = function(input) list(),
         flatten = function(p) list())
}

modification_to_prefill <- function(m) {
  c(m[intersect(c("id", "name", "type", "data_source_id"), names(m))],
    modification_template(m$type)$flatten(m$parameters %||% list()))
}

modification_item_ui <- function(id, prefill = NULL) {
  ns <- shiny::NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Modification",
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::textInput(ns("name"), "Name", pf("name"), width = "100%"),
      shiny::selectInput(ns("type"), "Type",
                         c("Choose a type..." = "", shinySAP::sapSchemaTypes("data_source_modification")),
                         selected = pf("type"), width = "100%"),
      entity_picker(ns("data_source_id"), "Data sources it applies to",
                    pf("data_source_id", character(0)), multiple = TRUE,
                    placeholder = "One or more data sources")
    ),
    shiny::uiOutput(ns("type_fields"))
  )
}

modification_item_server <- function(id, sap_id, prefill = NULL, on_remove = function() {},
                                     write = function(component) {},
                                     source_choices = shiny::reactive(character(0))) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    ns <- session$ns
    base_pf <- prefiller(prefill)
    read <- card_reader(input, prefill)

    item_card_label(output, shiny::reactive({
      nm <- trimws(input$name %||% "")
      paste0(if (nzchar(nm)) nm else "Untitled", " -- ", sap_id)
    }))

    live_pf <- function(key, default = NULL) {
      v <- shiny::isolate(input[[key]])
      if (is.null(v)) return(base_pf(key, default))
      if (length(v) == 1 && is.na(v)) return(NULL)
      v
    }

    type_r <- shiny::reactive(as.character(input$type %||% base_pf("type") %||% ""))

    output$type_fields <- shiny::renderUI({
      type <- type_r()
      if (!nzchar(type)) return(NULL)
      tmpl <- modification_template(type)
      shiny::tagList(
        shiny::tags$hr(class = "my-3"),
        if (!is.null(tmpl$hint)) shiny::p(class = "text-muted small mb-3", tmpl$hint),
        tmpl$ui(ns, live_pf)
      )
    })
    shiny::outputOptions(output, "type_fields", suspendWhenHidden = FALSE)

    sync_pickers(session, "data_source_id", source_choices, base_pf)

    shiny::observe({
      type <- type_r()
      write(shinySAP::newSapDataSourceModification(
        sap_id,
        name         = chr_or_null(read("name")),
        type         = chr_or_null(type),
        dataSourceId = chr_vec(read("data_source_id")),
        parameters   = card_parameters(input, type, modification_template(type), prefill),
        validate     = FALSE
      ))
    })
  })
}

data_source_modifications_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::h3("Data source modifications", class = "mb-1"),
        shiny::p(class = "text-muted mb-0",
                 "Changes applied to a database before the analyses run.")
      ),
      shiny::div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        shiny::actionButton(ns("add"), "Add modification", class = "btn btn-primary",
                            icon = shiny::icon("plus"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No modifications recorded yet.")
    ),
    shiny::div(id = ns("items"))
  )
}

data_source_modifications_server <- function(id, sap, source_choices = shiny::reactive(character(0))) {
  shiny::moduleServer(id, function(input, output, session) {
    settled_sources <- shiny::debounce(shiny::reactive(source_choices()), 600)
    coll <- sap_collection(
      sap, "data_source_modifications",
      new_component = function(id) shinySAP::newSapDataSourceModification(id, validate = FALSE),
      copy = function(source, id) shinySAP::newSapDataSourceModification(
        id, name = copy_name(source$name), type = source$type, dataSourceId = source$data_source_id,
        parameters = source$parameters, validate = FALSE),
      to_prefill = modification_to_prefill
    )
    item_server <- function(iid, sap_id, prefill, on_remove) {
      modification_item_server(iid, sap_id, prefill, on_remove,
                               write = function(component) if (items$is_live(iid, sap_id)) coll$write(component),
                               source_choices = settled_sources)
    }
    items <- dynamic_items("modification", "items", modification_item_ui, item_server,
                           ids = coll$ids, prefill_of = coll$prefill_of, noun = "Modification",
                           on_remove = coll$remove,
                           on_duplicate = function(id) items$reveal(coll$duplicate(id)),
                           on_undo = coll$undo,
                           on_reset = coll$forget)

    shiny::observeEvent(input$add, items$reveal(coll$add()))

    output$n <- shiny::renderText(items$count())
    shiny::outputOptions(output, "n", suspendWhenHidden = FALSE)

    list(reset = items$reset)
  })
}
