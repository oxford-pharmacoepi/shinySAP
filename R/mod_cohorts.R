# Section 2: Cohorts ---------------------------------------------------------
#
# A cohort is not just a name: it fixes the study period, the age groups, the
# sex and the time at risk that an analysis built on it inherits. The Proposed
# Analyses section reads those back (see denominator_summary() and the template
# validators in R/analysis_registry.R), so anything an analysis must not
# contradict belongs here.

# `kind` is the machine value; the names are what the user picks from.
COHORT_KINDS <- c(
  "Denominator (general population)"    = "denominator",
  "Target denominator (exposed cohort)" = "target_denominator",
  "Outcome"                             = "outcome",
  "Comparator"                          = "comparator",
  "Censoring"                           = "censor",
  "Strata"                              = "strata",
  "Other"                               = "other"
)

# 0.3.0 replaced `role` with `kind`. Old files still carry the role vocabulary.
COHORT_KIND_ALIASES <- c(
  "Target"     = "target_denominator",
  "Comparator" = "comparator",
  "Outcome"    = "outcome",
  "Strata"     = "strata",
  "Other"      = "other"
)

COHORT_SEXES <- c("Both", "Male", "Female")

canonical_cohort_kind <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return(unname(COHORT_KINDS[[1]]))
  if (x %in% COHORT_KINDS) return(x)                                  # already a kind
  if (x %in% names(COHORT_KIND_ALIASES)) return(unname(COHORT_KIND_ALIASES[[x]]))
  unname(COHORT_KINDS[[1]])
}

# Cohorts arrive from app.R as a list; analyses need to look one up by name.
# Free text is allowed in the pickers, so an unknown name is normal -- NULL, not
# an error.
cohort_by_name <- function(cohorts, nm) {
  if (length(nm) != 1 || is.na(nm) || !nzchar(nm)) return(NULL)
  if (!nm %in% names(cohorts)) return(NULL)
  cohorts[[nm]]
}

cohort_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Cohort",
    layout_columns(
      col_widths = c(5, 4, 3),
      textInput(ns("name"), "Cohort name", pf("name"), width = "100%"),
      selectInput(ns("kind"), "Kind", COHORT_KINDS,
                  selected = canonical_cohort_kind(pf("kind", COHORT_KINDS[[1]])), width = "100%"),
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

    # What the cohort fixes for every analysis built on it -------------------
    tags$hr(class = "my-3"),
    tags$label(class = "form-label fw-semibold", "What this cohort fixes"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      textInput(ns("study_period_start"), "Study period start", pf("study_period_start"),
                width = "100%", placeholder = "YYYY-MM-DD"),
      textInput(ns("study_period_end"), "Study period end", pf("study_period_end"),
                width = "100%", placeholder = "YYYY-MM-DD"),
      selectInput(ns("sex"), "Sex", COHORT_SEXES, selected = pf("sex", COHORT_SEXES[1]),
                  width = "100%"),
      numericInput(ns("prior_observation_days"), "Prior observation (days)",
                   value = pf("prior_observation_days", NULL), width = "100%")
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        ns("age_groups"), "Age groups (one per line)",
        join_lines(pf("age_groups", character(0))), rows = 3, width = "100%",
        placeholder = "0-17\n18-64\n65-150"
      ),
      # IncidencePrevalence's strata are *columns on the denominator cohort
      # table*, so an analysis can only stratify by what the cohort carries.
      # generateDenominatorCohortSet() always produces age_group and sex, hence
      # the default; add any column your ETL puts alongside them.
      textAreaInput(
        ns("strata_variables"), "Strata variables this cohort carries (one per line)",
        join_lines(pf("strata_variables", c("age_group", "sex"))), rows = 3, width = "100%",
        placeholder = "age_group\nsex\nregion"
      )
    ),
    # 0.3.0: time at risk is a property of the cohort, not of each analysis
    # built on it, so two analyses on the same denominator cannot disagree.
    tar_ui(ns, pf),
    layout_columns(
      col_widths = c(6, 6),
      numericInput(ns("washout_days"), "Washout (days)",
                   value = pf("washout_days", NULL), width = "100%"),
      textInput(ns("concept_set"), "Concept set / codelist", pf("concept_set"), width = "100%")
    )
  )
}

cohort_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    reactive(c(
      list(
        name                   = blank_to_na(input$name),
        kind                   = canonical_cohort_kind(input$kind),
        cohort_id              = input$cohort_id %||% NA,
        description            = blank_to_na(input$description),
        entry_events           = as_array(split_lines(input$entry_events)),
        inclusion_criteria     = as_array(split_lines(input$inclusion_criteria)),
        exit_criteria          = as_array(split_lines(input$exit_criteria)),
        study_period_start     = blank_to_na(input$study_period_start),
        study_period_end       = blank_to_na(input$study_period_end),
        sex                    = input$sex %||% COHORT_SEXES[1],
        age_groups             = as_array(split_lines(input$age_groups)),
        strata_variables       = as_array(split_lines(input$strata_variables)),
        prior_observation_days = input$prior_observation_days %||% NA,
        washout_days           = input$washout_days %||% NA,
        concept_set            = blank_to_na(input$concept_set)
      ),
      tar_collect(input)
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
    items <- dynamic_items("cohort", "items", cohort_item_ui, cohort_item_server)

    observeEvent(input$add, items$add())

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(cohorts) {
      items$clear()
      for (ch in cohorts) {
        # 0.2.0 called it `role`, with a different vocabulary.
        ch$kind <- canonical_cohort_kind(ch$kind %||% ch$role)
        items$add(tar_flatten(ch))
      }
    }

    # Feeds the cohort pickers in the Analyses section.
    names_r <- reactive({
      nms <- vapply(items$data(), function(x) as.character(x$name %||% ""), character(1))
      sort(unique(nms[nzchar(nms)]))
    })

    # Feeds the denominator summary and the template validators, which need the
    # whole cohort, not just its name.
    by_name_r <- reactive({
      d   <- items$data()
      nms <- vapply(d, function(x) as.character(x$name %||% ""), character(1))
      keep <- nzchar(nms)
      stats::setNames(d[keep], nms[keep])
    })

    list(data = items$data, load = load, names = names_r, by_name = by_name_r)
  })
}
