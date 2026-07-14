# Section 2: Cohorts ---------------------------------------------------------

COHORT_ROLES <- c("Target", "Comparator", "Outcome", "Strata", "Other")

cohort_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Cohort",
    layout_columns(
      col_widths = c(5, 3, 4),
      textInput(ns("name"), "Cohort name", pf("name"), width = "100%"),
      selectInput(ns("role"), "Role", COHORT_ROLES, selected = pf("role", COHORT_ROLES[1]), width = "100%"),
      # NULL, not NA: numericInput(value = NA) renders value="NA", which browsers reject.
      numericInput(ns("cohort_id"), "Cohort ID", value = pf("cohort_id", NULL), width = "100%")
    ),
    textAreaInput(ns("description"), "Description", pf("description"), rows = 2, width = "100%"),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        ns("entry_events"), "Entry events (one per line)",
        join_lines(pf("entry_events", character(0))), rows = 4, width = "100%",
        placeholder = "First diagnosis of type 2 diabetes"
      ),
      textAreaInput(
        ns("exit_criteria"), "Exit criteria (one per line)",
        join_lines(pf("exit_criteria", character(0))), rows = 4, width = "100%",
        placeholder = "End of continuous observation"
      )
    ),
    textAreaInput(
      ns("inclusion_criteria"), "Inclusion / exclusion criteria (one per line)",
      join_lines(pf("inclusion_criteria", character(0))), rows = 4, width = "100%",
      placeholder = "Aged 18 or over at index\nNo prior insulin exposure"
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      numericInput(ns("prior_observation_days"), "Prior observation (days)",
                   value = pf("prior_observation_days", NULL), width = "100%"),
      numericInput(ns("washout_days"), "Washout (days)",
                   value = pf("washout_days", NULL), width = "100%"),
      textInput(ns("concept_set"), "Concept set / codelist", pf("concept_set"), width = "100%"),
      # Marks this cohort as a sub-cohort of another (e.g. an age band of a
      # denominator population). Analyses use it to offer a cohort set's IDs.
      entity_picker(ns("parent_cohort"), "Part of cohort set", pf("parent_cohort"),
                    placeholder = "Parent cohort (optional)")
    )
  )
}

cohort_item_server <- function(id, prefill = NULL, on_remove = function() {},
                               cohort_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    # Safe from a feedback loop: the parent picker's choices come from every
    # card's name, but updating choices never changes this card's values.
    sync_pickers(session, "parent_cohort", cohort_names, prefiller(prefill))
    reactive(list(
      name                   = blank_to_na(input$name),
      role                   = input$role,
      cohort_id              = input$cohort_id %||% NA,
      parent_cohort          = blank_to_na(input$parent_cohort),
      description            = blank_to_na(input$description),
      entry_events           = as_array(split_lines(input$entry_events)),
      inclusion_criteria     = as_array(split_lines(input$inclusion_criteria)),
      exit_criteria          = as_array(split_lines(input$exit_criteria)),
      prior_observation_days = input$prior_observation_days %||% NA,
      washout_days           = input$washout_days %||% NA,
      concept_set            = blank_to_na(input$concept_set)
    ))
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
    # settled_names is defined below items -- R only resolves it when a card is
    # actually added, long after this module body has run.
    item_server <- function(iid, prefill, on_remove) {
      cohort_item_server(iid, prefill, on_remove, settled_names)
    }
    items <- dynamic_items("cohort", "items", cohort_item_ui, item_server)

    observeEvent(input$add, items$add())

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(cohorts) {
      items$clear()
      for (ch in cohorts) items$add(ch)
    }

    # Feeds the cohort pickers in the Analyses section and, debounced, each
    # card's own parent picker.
    names_r <- reactive({
      nms <- vapply(items$data(), function(x) as.character(x$name %||% ""), character(1))
      sort(unique(nms[nzchar(nms)]))
    })
    settled_names <- debounce(names_r, 600)

    list(data = items$data, load = load, names = names_r)
  })
}
