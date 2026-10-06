# Section: Analyses ------------------------------------------------------------
#
# The card is in two halves: the common fields (id, name, data sources, type)
# and a parameter block rendered from the registry in analysis_registry.R. Every
# edit writes a draft shinySAP::newSapAnalysis() back through the section's CRUD.

analysis_item_ui <- function(id, prefill = NULL) {
  ns <- shiny::NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Analysis",
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::textInput(ns("name"), "Analysis name", pf("name"), width = "100%"),
      # A new card starts with NO type: the type decides everything else on the
      # card, so it is the author's first decision, not a default.
      shiny::selectInput(ns("type"), "Analysis type",
                         c("Choose a type..." = "", shinySAP::sapSchemaTypes("analysis")),
                         selected = pf("type"), width = "100%"),
      entity_picker(ns("data_source_id"), "Data sources this analysis runs on",
                    pf("data_source_id", character(0)), multiple = TRUE,
                    placeholder = "One or more data sources")
    ),
    shiny::tags$hr(class = "my-3"),
    shiny::uiOutput(ns("type_fields"))
  )
}

analysis_item_server <- function(id, sap_id, prefill = NULL, on_remove = function() {},
                                 write = function(component) {},
                                 cohort_choices = shiny::reactive(list()),
                                 source_choices = shiny::reactive(character(0)),
                                 cohort_index = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    ns      <- session$ns
    base_pf <- prefiller(prefill)
    read    <- card_reader(input, prefill)

    item_card_label(output, shiny::reactive({
      nm   <- trimws(input$name %||% "")
      type <- type_r()
      paste0(if (nzchar(nm)) nm else "Untitled", " -- ",
             if (nzchar(type)) analysis_type_label(type) else "no type chosen",
             " -- ", sap_id)
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
      if (!nzchar(type)) {
        return(shiny::p(class = "text-muted small mb-0",
                        "Choose an analysis type above to see the fields it takes."))
      }
      tmpl <- analysis_template(type)
      shiny::tagList(
        if (!is.null(tmpl$hint)) shiny::p(class = "text-muted small mb-3", tmpl$hint),
        tmpl$ui(ns, live_pf)
      )
    })
    shiny::outputOptions(output, "type_fields", suspendWhenHidden = FALSE)

    sync_pickers(session, function() analysis_template(type_r())$pickers$cohorts %||% character(0),
                 cohort_choices, base_pf)
    sync_pickers(session, "data_source_id", source_choices, base_pf)

    denominator_pick <- function() {
      key <- analysis_template(type_r())$denominator
      if (is.null(key)) NULL else input[[key]]
    }
    denominator_cohort <- function() {
      id <- as.character(denominator_pick() %||% "")[1]
      if (!nzchar(id)) NULL else cohort_index()[[id]]
    }

    # The strata picker's choices come from another input on this card: the
    # columns the chosen denominator carries. It keeps whatever it holds, so a
    # free-typed column on a survival analysis survives the update.
    shiny::observe({
      field <- analysis_template(type_r())$pickers$strata
      if (is.null(field)) return()
      current <- chr_vec(shiny::isolate(input[[field]]) %||% base_pf(field, character(0)))
      shiny::updateSelectizeInput(session, field,
                                  choices = unique(c(cohort_strata_variables(denominator_cohort()), current)),
                                  selected = current)
    })

    output$denominator_summary <- shiny::renderUI({
      denominator_summary(denominator_cohort(), picked = denominator_pick())
    })

    shiny::observe({
      type <- type_r()
      write(shinySAP::newSapAnalysis(
        sap_id,
        name         = chr_or_null(read("name")),
        dataSourceId = chr_vec(read("data_source_id")),
        type         = chr_or_null(type),
        parameters   = card_parameters(input, type, analysis_template(type), prefill),
        validate     = FALSE
      ))
    })
  })
}

# One saved (or live) analysis -> the prefill its card is rebuilt from.
analysis_to_prefill <- function(a) {
  c(a[intersect(ANALYSIS_COMMON_FIELDS, names(a))],
    analysis_template(a$type)$flatten(a$parameters %||% list()))
}

analyses_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::h3("Analyses", class = "mb-1"),
        shiny::p(class = "text-muted mb-0", "What is estimated, on which cohorts and sources, and how.")
      ),
      shiny::div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        shiny::actionButton(ns("add"), "Add analysis", class = "btn btn-primary", icon = shiny::icon("plus"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No analyses proposed yet.")
    ),
    shiny::div(id = ns("items"))
  )
}

# `cohort_choices` are the grouped picker choices; `cohort_index` is id -> the
# whole cohort component, for the denominator summary and strata columns.
analyses_server <- function(id, sap,
                            cohort_choices = shiny::reactive(list()),
                            cohort_index = shiny::reactive(list()),
                            source_choices = shiny::reactive(character(0))) {
  shiny::moduleServer(id, function(input, output, session) {
    settled_sources <- shiny::debounce(shiny::reactive(source_choices()), 600)
    settled_choices <- shiny::debounce(shiny::reactive(cohort_choices()), 600)

    coll <- sap_collection(
      sap, "analyses",
      new_component = function(id) shinySAP::newSapAnalysis(id, validate = FALSE),
      copy = function(source, id) shinySAP::newSapAnalysis(
        id, name = copy_name(source$name), dataSourceId = source$data_source_id,
        type = source$type, parameters = source$parameters, validate = FALSE),
      to_prefill = analysis_to_prefill
    )
    item_server <- function(iid, sap_id, prefill, on_remove) {
      analysis_item_server(iid, sap_id, prefill, on_remove,
                           write = function(component) if (items$is_live(iid, sap_id)) coll$write(component),
                           cohort_choices = settled_choices,
                           source_choices = settled_sources,
                           cohort_index = cohort_index)
    }
    items <- dynamic_items("analysis", "items", analysis_item_ui, item_server,
                           ids = coll$ids, prefill_of = coll$prefill_of, noun = "Analysis",
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
