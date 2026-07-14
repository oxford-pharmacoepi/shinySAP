# Section 2: Cohorts ---------------------------------------------------------
#
# A cohort card is in two halves, like an analysis card: the common fields below,
# and a block that depends on the cohort's `kind`. The kinds are genuinely
# different objects -- a denominator cohort set is *generated* by
# generateDenominatorCohortSet(), while an outcome cohort is *defined* by entry
# criteria -- so they are asked for different things. See R/cohort_kinds.R for
# the registry and for what each kind carries.
#
# The denominator kinds also fix what an analysis built on them inherits: the
# cohort date range, the age groups, the sex and the time at risk. Proposed
# Analyses reads those back rather than restating them (denominator_summary()).

cohort_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Cohort",
    layout_columns(
      col_widths = c(5, 4, 3),
      textInput(ns("name"), "Cohort name", pf("name"), width = "100%"),
      selectInput(ns("kind"), "Kind", COHORT_KINDS,
                  selected = canonical_cohort_kind(pf("kind", COHORT_KINDS[[1]])),
                  width = "100%"),
      # NULL, not NA: numericInput(value = NA) renders value="NA", which browsers reject.
      numericInput(ns("cohort_id"), "Cohort ID", value = pf("cohort_id", NULL), width = "100%")
    ),
    textAreaInput(ns("description"), "Description", pf("description"), rows = 2, width = "100%"),
    tags$hr(class = "my-3"),
    uiOutput(ns("kind_fields"))
  )
}

cohort_item_server <- function(id, prefill = NULL, on_remove = function() {},
                               cohort_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    ns      <- session$ns
    base_pf <- prefiller(prefill)

    # Same rule as the analysis card: Shiny keeps an input's last value after its
    # node is destroyed, so a block rebuilt after a kind switch reads back what
    # was typed into it. NULL means never rendered -> fall back to the file. A
    # length-1 NA means the user cleared a numeric, and must stay cleared.
    # Pickers are excluded: sync_pickers() owns those.
    live_pf <- function(key, default = NULL) {
      v <- isolate(input[[key]])
      if (is.null(v)) return(base_pf(key, default))
      if (length(v) == 1 && is.na(v)) return(NULL)
      v
    }

    kind_r <- reactive(
      canonical_cohort_kind(input$kind %||% base_pf("kind", COHORT_KINDS[[1]]))
    )

    # Only the kind may invalidate this -- everything else is isolated inside
    # live_pf(), or typing would rebuild the block and steal focus.
    output$kind_fields <- renderUI({
      tmpl <- cohort_template(kind_r())
      tagList(
        if (!is.null(tmpl$hint)) p(class = "text-muted small mb-3", tmpl$hint),
        tmpl$ui(ns, live_pf)
      )
    })
    # The Cohorts tab is a hidden tab-pane until selected, and a hidden output
    # does not render -- without this a loaded SAP would leave every kind block
    # unbuilt, so its inputs would read NULL and every cohort would save empty.
    outputOptions(output, "kind_fields", suspendWhenHidden = FALSE)

    # A target denominator names the cohort it is built from, so the cohort
    # pickers on a cohort card are fed by the cohort list itself.
    sync_pickers(session, function() cohort_template(kind_r())$pickers$cohorts %||% character(0),
                 cohort_names, base_pf)

    data_r <- reactive({
      tmpl <- cohort_template(kind_r())
      c(
        list(
          name        = blank_to_na(input$name),
          kind        = kind_r(),
          cohort_id   = input$cohort_id %||% NA,
          description = blank_to_na(input$description)
        ),
        # collect() reads only its own kind's input ids, so values stranded by a
        # previously selected kind never reach the JSON.
        tmpl$collect(input)
      )
    })

    reactive(list(data = data_r()))
  })
}

cohorts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h3("Cohorts", class = "mb-1"),
        p(class = "text-muted mb-0", "Populations the analyses are run against.")
      ),
      actionButton(ns("add"), "Add cohort", class = "btn btn-primary", icon = icon("plus"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No cohorts defined yet.")
    ),
    div(id = ns("items"))
  )
}

cohorts_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # The target-cohort picker on a cohort card is fed by the cohort list, which
    # is derived from the cards -- a cycle. reactiveVal only invalidates when the
    # value actually changes, which breaks it: editing anything other than a name
    # no longer re-triggers every picker on the tab.
    names_v <- reactiveVal(character(0))
    settled <- debounce(reactive(names_v()), 600)

    item_server <- function(iid, prefill, on_remove) {
      cohort_item_server(iid, prefill, on_remove, settled)
    }
    items <- dynamic_items("cohort", "items", cohort_item_ui, item_server)

    observeEvent(input$add, items$add())

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    data_r <- reactive(lapply(items$data(), function(x) x$data))

    observe({
      nms <- vapply(data_r(), function(x) as.character(x$name %||% ""), character(1))
      nms <- sort(unique(nms[nzchar(nms)]))
      if (!identical(nms, isolate(names_v()))) names_v(nms)
    })

    load <- function(cohorts) {
      items$clear()
      for (ch in cohorts) {
        # 0.2.0 called it `role`, with a different vocabulary.
        ch$kind <- canonical_cohort_kind(ch$kind %||% ch$role)
        items$add(cohort_template(ch$kind)$flatten(ch))
      }
    }

    # Feeds the cohort pickers in the Analyses section.
    names_r <- reactive(names_v())

    # Feeds the denominator summary and the analysis validators, which need the
    # whole cohort, not just its name.
    by_name_r <- reactive({
      d   <- data_r()
      nms <- vapply(d, function(x) as.character(x$name %||% ""), character(1))
      keep <- nzchar(nms)
      stats::setNames(d[keep], nms[keep])
    })

    problems_r <- reactive({
      idx   <- by_name_r()
      found <- lapply(data_r(), function(ch) {
        msgs <- tryCatch(
          as.character(cohort_template(ch$kind)$validate(ch, idx)),
          error = function(e) paste("Could not validate this cohort:", conditionMessage(e))
        )
        if (!length(msgs)) return(NULL)
        list(name = ch$name %||% "Untitled cohort", messages = msgs)
      })
      found[!vapply(found, is.null, logical(1))]
    })

    list(data = data_r, load = load, names = names_r, by_name = by_name_r,
         problems = problems_r)
  })
}
