# Section 4: Proposed Analyses -----------------------------------------------

ANALYSIS_TYPES <- c(
  "Cohort characterisation", "Incidence rate", "Prevalence",
  "Comparative cohort", "Self-controlled case series", "Case-control",
  "Survival analysis", "Patient-level prediction", "Drug utilisation", "Other"
)

ANCHORS <- c("cohort start", "cohort end")

analysis_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Analysis",
    layout_columns(
      col_widths = c(7, 5),
      textInput(ns("name"), "Analysis name", pf("name"), width = "100%"),
      selectInput(ns("analysis_type"), "Analysis type", ANALYSIS_TYPES,
                  selected = pf("analysis_type", ANALYSIS_TYPES[1]), width = "100%")
    ),
    textAreaInput(ns("description"), "Description", pf("description"), rows = 2, width = "100%"),
    layout_columns(
      col_widths = c(4, 4, 4),
      entity_picker(ns("target_cohort"), "Target cohort", pf("target_cohort"),
                    placeholder = "Select or type a cohort"),
      entity_picker(ns("comparator_cohort"), "Comparator cohort", pf("comparator_cohort"),
                    placeholder = "Select or type a cohort"),
      entity_picker(ns("outcome_cohort"), "Outcome cohort", pf("outcome_cohort"),
                    placeholder = "Select or type a cohort")
    ),
    entity_picker(ns("data_sources"), "CDM sources this analysis runs on",
                  pf("data_sources", character(0)), multiple = TRUE,
                  placeholder = "Select or type one or more CDM sources"),
    tags$label(class = "form-label fw-semibold", "Time at risk"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      numericInput(ns("tar_start_offset"), "Start (days)", value = pf("tar_start_offset", 0), width = "100%"),
      selectInput(ns("tar_start_anchor"), "Anchored on", ANCHORS,
                  selected = pf("tar_start_anchor", ANCHORS[1]), width = "100%"),
      numericInput(ns("tar_end_offset"), "End (days)", value = pf("tar_end_offset", 0), width = "100%"),
      selectInput(ns("tar_end_anchor"), "Anchored on", ANCHORS,
                  selected = pf("tar_end_anchor", ANCHORS[2]), width = "100%")
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(ns("covariates"), "Covariates / adjustment (one per line)",
                    join_lines(pf("covariates", character(0))), rows = 4, width = "100%",
                    placeholder = "Age at index\nSex\nCharlson comorbidity index"),
      textAreaInput(ns("stratifications"), "Stratifications (one per line)",
                    join_lines(pf("stratifications", character(0))), rows = 4, width = "100%",
                    placeholder = "Sex\n10-year age bands")
    ),
    layout_columns(
      col_widths = c(6, 6),
      textInput(ns("statistical_method"), "Statistical method", pf("statistical_method"), width = "100%"),
      textInput(ns("effect_measure"), "Effect measure", pf("effect_measure"), width = "100%")
    ),
    textAreaInput(ns("sensitivity_analyses"), "Sensitivity analyses (one per line)",
                  join_lines(pf("sensitivity_analyses", character(0))), rows = 3, width = "100%")
  )
}

analysis_item_server <- function(id, prefill = NULL, on_remove = function() {},
                                 cohort_names = reactive(character(0)),
                                 source_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    pf <- prefiller(prefill)

    sync_pickers(session, c("target_cohort", "comparator_cohort", "outcome_cohort"),
                 cohort_names, pf)
    sync_pickers(session, "data_sources", source_names, pf)

    reactive(list(
      name                 = blank_to_na(input$name),
      analysis_type        = input$analysis_type,
      description          = blank_to_na(input$description),
      target_cohort        = blank_to_na(input$target_cohort),
      comparator_cohort    = blank_to_na(input$comparator_cohort),
      outcome_cohort       = blank_to_na(input$outcome_cohort),
      data_sources         = as_array(input$data_sources %||% character(0)),
      time_at_risk         = list(
        start_offset_days = input$tar_start_offset %||% NA,
        start_anchor      = input$tar_start_anchor,
        end_offset_days   = input$tar_end_offset %||% NA,
        end_anchor        = input$tar_end_anchor
      ),
      covariates           = as_array(split_lines(input$covariates)),
      stratifications      = as_array(split_lines(input$stratifications)),
      statistical_method   = blank_to_na(input$statistical_method),
      effect_measure       = blank_to_na(input$effect_measure),
      sensitivity_analyses = as_array(split_lines(input$sensitivity_analyses))
    ))
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
        # time_at_risk is nested in the JSON; flatten it back onto the inputs.
        tar <- a$time_at_risk
        a$tar_start_offset <- tar$start_offset_days
        a$tar_start_anchor <- tar$start_anchor
        a$tar_end_offset   <- tar$end_offset_days
        a$tar_end_anchor   <- tar$end_anchor
        items$add(a)
      }
    }

    list(data = items$data, load = load)
  })
}
