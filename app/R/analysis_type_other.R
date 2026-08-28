# Analysis template: Other ----------------------------------------------------
#
# The generic form, and the fallback for every analysis type that has no template
# of its own yet -- so this one must always be registered. See
# R/analysis_registry.R for the rules for writing a template.

register_analysis_template(
  "Other",

  ui = function(ns, pf) shiny::tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      entity_picker(ns("target_cohort"), "Target cohort", pf("target_cohort"),
                    placeholder = "Select or type a cohort"),
      entity_picker(ns("comparator_cohort"), "Comparator cohort", pf("comparator_cohort"),
                    placeholder = "Select or type a cohort"),
      entity_picker(ns("outcome_cohort"), "Outcome cohort", pf("outcome_cohort"),
                    placeholder = "Select or type a cohort")
    ),
    tar_ui(ns, pf),
    shiny::textAreaInput(ns("covariates"), "Covariates / adjustment (one per line)",
                  join_lines(pf("covariates", character(0))), rows = 4, width = "100%",
                  placeholder = "Age at index\nSex\nCharlson comorbidity index"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::textInput(ns("statistical_method"), "Statistical method", pf("statistical_method", ""),
                width = "100%"),
      shiny::textInput(ns("effect_measure"), "Effect measure", pf("effect_measure", ""), width = "100%")
    ),
    strat_ui(ns, pf)
  ),

  collect = function(input) c(
    list(
      target_cohort      = blank_to_na(input$target_cohort),
      comparator_cohort  = blank_to_na(input$comparator_cohort),
      outcome_cohort     = blank_to_na(input$outcome_cohort),
      covariates         = as_array(split_lines(input$covariates)),
      statistical_method = blank_to_na(input$statistical_method),
      effect_measure     = blank_to_na(input$effect_measure)
    ),
    tar_collect(input),
    strat_collect(input)
  ),

  pickers = list(cohorts = c("target_cohort", "comparator_cohort", "outcome_cohort")),

  flatten = tar_flatten
)
