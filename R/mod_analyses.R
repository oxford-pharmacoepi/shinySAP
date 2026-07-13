# Section 4: Proposed Analyses -----------------------------------------------
#
# The card is in two halves: the common fields below, and a type block rendered
# from the registry in R/analysis_templates.R. ANALYSIS_TYPES, ANCHORS and
# ANALYSIS_COMMON_FIELDS live there too -- redefining any of them here would
# silently win, since R/ is sourced alphabetically.

analysis_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Analysis",
    layout_columns(
      col_widths = c(7, 5),
      textInput(ns("name"), "Analysis name", pf("name"), width = "100%"),
      selectInput(ns("analysis_type"), "Analysis type", ANALYSIS_TYPES,
                  selected = canonical_analysis_type(pf("analysis_type", ANALYSIS_TYPES[1])),
                  width = "100%")
    ),
    textAreaInput(ns("description"), "Description", pf("description"), rows = 2, width = "100%"),
    entity_picker(ns("data_sources"), "CDM sources this analysis runs on",
                  pf("data_sources", character(0)), multiple = TRUE,
                  placeholder = "Select or type one or more CDM sources"),
    tags$hr(class = "my-3"),
    uiOutput(ns("type_fields"))
  )
}

analysis_item_server <- function(id, prefill = NULL, on_remove = function() {},
                                 cohort_names = reactive(character(0)),
                                 source_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # session$ns, not NS(id): dynamic_items() hands the server the bare item id
    # and the fully-qualified one to the UI. They only line up because
    # moduleServer() runs under the parent's reactive domain.
    ns      <- session$ns
    base_pf <- prefiller(prefill)

    # Shiny keeps an input's last reported value after its node leaves the DOM,
    # so a template block rebuilt after a type switch can read back what the user
    # typed into it. NULL means the input has never been rendered -- fall back to
    # the saved file. A length-1 NA means the user *cleared* a numeric, and must
    # stay cleared, or the saved value would quietly reappear. Pickers are not
    # served from here: sync_pickers() owns those, and its update always lands
    # after the render, so a second opinion here could only disagree with it.
    live_pf <- function(key, default = NULL) {
      v <- isolate(input[[key]])
      if (is.null(v)) return(base_pf(key, default))
      if (length(v) == 1 && is.na(v)) return(NULL)
      v
    }

    # The default has to match analysis_item_ui()'s, or a new card paints one
    # template, binds and reports its inputs, then repaints with another --
    # leaving a set of stale values behind on the first.
    type_r <- reactive(
      canonical_analysis_type(input$analysis_type %||% base_pf("analysis_type", ANALYSIS_TYPES[1]))
    )

    # Only the type may invalidate this. A dependency on the field values would
    # rebuild the block on every keystroke, and one on the name reactives would
    # rebuild it whenever a cohort is edited in another tab; both steal focus
    # mid-edit.
    output$type_fields <- renderUI({
      tmpl <- analysis_template(type_r())
      tagList(
        if (!is.null(tmpl$hint)) p(class = "text-muted small mb-3", tmpl$hint),
        tmpl$ui(ns, live_pf)
      )
    })
    # Load-bearing. The Analyses tab is a hidden tab-pane until it is selected,
    # and a hidden output does not render -- so without this, a SAP loaded from
    # the Review tab would leave every type block unbuilt, its inputs reading
    # NULL, and every parameters object would serialise empty.
    outputOptions(output, "type_fields", suspendWhenHidden = FALSE)

    # A function, not a vector, so the observer re-runs on a type change and
    # picks up the freshly rendered pickers.
    sync_pickers(session, function() analysis_template(type_r())$pickers$cohorts %||% character(0),
                 cohort_names, base_pf)
    sync_pickers(session, function() analysis_template(type_r())$pickers$sources %||% character(0),
                 source_names, base_pf)
    sync_pickers(session, "data_sources", source_names, base_pf)

    # For one round-trip after a type switch the new block's inputs have not
    # reported yet, so parameters briefly reads as nulls. Harmless while the only
    # consumers are the Review tab (suspended while the user is on Analyses) and
    # the Save button (a later click) -- but an autosave observe({ sap(); ... })
    # would capture the gap.
    reactive({
      tmpl <- analysis_template(type_r())
      list(
        name          = blank_to_na(input$name),
        # type_r(), not input$analysis_type: the type we emit and the collector
        # that produced `parameters` must never disagree.
        analysis_type = type_r(),
        description   = blank_to_na(input$description),
        data_sources  = as_array(input$data_sources %||% character(0)),
        # collect() reads only its own template's input ids, so values stranded
        # by a previously selected template never reach the JSON.
        parameters    = tmpl$collect(input)
      )
    })
  })
}

analyses_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h3("Proposed analyses", class = "mb-1"),
        p(class = "text-muted mb-0", "What is estimated, on which cohorts and sources, and how.")
      ),
      actionButton(ns("add"), "Add analysis", class = "btn btn-primary", icon = icon("plus"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No analyses proposed yet.")
    ),
    div(id = ns("items"))
  )
}

analyses_server <- function(id, cohort_names = reactive(character(0)),
                            source_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    # Names change on every keystroke in the section that owns them; settle
    # first so the pickers are not rebuilt mid-word.
    settled_cohorts <- debounce(cohort_names, 600)
    settled_sources <- debounce(source_names, 600)

    item_server <- function(iid, prefill, on_remove) {
      analysis_item_server(iid, prefill, on_remove, settled_cohorts, settled_sources)
    }
    items <- dynamic_items("analysis", "items", analysis_item_ui, item_server)

    observeEvent(input$add, items$add())

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(analyses) {
      items$clear()
      for (a in analyses) {
        a$analysis_type <- canonical_analysis_type(a$analysis_type)
        tmpl <- analysis_template(a$analysis_type)
        # Pre-0.3.0 files kept the type-specific fields at the top level, so they
        # load with no migration step. is.null(), not %||%: an empty parameters
        # object reads back as a zero-length list, which %||% would take for
        # absent and then mistake a 0.3.0 file for an old one.
        params <- if (is.null(a$parameters)) a else a$parameters
        flat   <- tmpl$flatten(params)
        if (!is.list(flat)) flat <- list()
        # c(), not modifyList(): modifyList merges nested lists instead of
        # replacing them, drops keys whose new value is NULL, and errors on a
        # non-list -- after items$clear() has already emptied the section. And
        # because prefill[[key]] takes the first match, a template parameter that
        # collides with a common key cannot clobber it.
        items$add(c(a[intersect(ANALYSIS_COMMON_FIELDS, names(a))], flat))
      }
    }

    list(data = items$data, load = load)
  })
}
