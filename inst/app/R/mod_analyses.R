# Section: Analyses ------------------------------------------------------------
#
# The card is in two halves: the common fields (id, name, data sources, type)
# and a parameter block rendered from the registry in analysis_registry.R.

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

analysis_item_server <- function(id, prefill = NULL, on_remove = function() {},
                                 cohort_choices = shiny::reactive(list()),
                                 source_choices = shiny::reactive(character(0)),
                                 cohort_index = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    ns      <- session$ns
    base_pf <- prefiller(prefill)
    sap_id  <- prefill$id

    item_card_label(output, shiny::reactive({
      nm   <- trimws(input$name %||% "")
      type <- type_r()
      paste0(if (nzchar(nm)) nm else "Untitled", " -- ",
             if (nzchar(type)) analysis_type_label(type) else "no type chosen",
             " -- ", sap_id)
    }))

    # Shiny keeps an input's last reported value after its node leaves the DOM,
    # so a block rebuilt after a type switch reads back what was typed into it.
    # NULL means never rendered -> the saved file. A length-1 NA means the user
    # cleared a numeric, and must stay cleared. Pickers are excluded:
    # sync_pickers() owns those.
    live_pf <- function(key, default = NULL) {
      v <- shiny::isolate(input[[key]])
      if (is.null(v)) return(base_pf(key, default))
      if (length(v) == 1 && is.na(v)) return(NULL)
      v
    }

    type_r <- shiny::reactive(as.character(input$type %||% base_pf("type") %||% ""))

    # Only the type may invalidate this; anything else would rebuild the block
    # mid-edit and steal focus.
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
    # Load-bearing: the tab is hidden until selected, and a hidden output does
    # not render, so without this a loaded SAP would save every analysis with
    # empty parameters.
    shiny::outputOptions(output, "type_fields", suspendWhenHidden = FALSE)

    sync_pickers(session, function() analysis_template(type_r())$pickers$cohorts %||% character(0),
                 cohort_choices, base_pf)
    sync_pickers(session, "data_source_id", source_choices, base_pf)

    # Which input names the denominator differs by template (the registry's
    # `denominator` slot); NULL for estimators without one.
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

    shiny::reactive({
      type <- type_r()
      sources <- chr_vec(input$data_source_id)
      compact(list(
        id             = sap_id,
        name           = chr_or_null(input$name),
        data_source_id = vec_or_null(sources),
        type           = chr_or_null(type),
        # collect() reads only its own template's input ids, so values stranded
        # by a previously selected template never reach the SAP.
        parameters     = if (nzchar(type)) analysis_template(type)$collect(input) else list()
      ))
    })
  })
}

# One saved (or live) analysis -> the prefill its card is rebuilt from. Shared by
# load() and the Duplicate button, so the two can never drift.
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

analyses_server <- function(id, cohort_choices = shiny::reactive(list()),
                            cohort_index = shiny::reactive(list()),
                            source_choices = shiny::reactive(character(0))) {
  shiny::moduleServer(id, function(input, output, session) {
    settled_sources <- shiny::debounce(source_choices, 600)

    item_server <- function(iid, prefill, on_remove) {
      analysis_item_server(iid, prefill, on_remove,
                           cohort_choices = cohort_choices,
                           source_choices = settled_sources,
                           cohort_index = cohort_index)
    }
    items <- dynamic_items("analysis", "items", analysis_item_ui, item_server,
                           to_prefill = analysis_to_prefill, noun = "Analysis", id_prefix = "an")

    shiny::observeEvent(input$add, items$add(reveal = TRUE))

    output$n <- shiny::renderText(items$count())
    shiny::outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(analyses) {
      items$clear()
      for (a in analyses) items$add(analysis_to_prefill(a))
    }

    list(data = items$data, load = load)
  })
}
