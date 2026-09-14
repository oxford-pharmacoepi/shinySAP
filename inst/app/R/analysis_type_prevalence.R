# Analysis templates: point and period prevalence ------------------------------
#
# Two schema types, one builder: they share the denominator/outcome pickers and
# strata, and differ in the time-granularity block -- estimatePointPrevalence()
# takes a time_point; estimatePeriodPrevalence() takes complete_database_intervals,
# full_contribution and level. Each type's collect() writes only its own keys.

prevalence_ui <- function(kind) function(ns, pf) {
  vocab <- shinySAP::sapSchemaVocabulary()
  shiny::tagList(
    denominator_outcome_ui(ns, pf),
    denominator_summary_ui(ns, pf),
    cohort_ids_of_interest_ui(ns, pf),

    section_heading("Time granularity"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::selectInput(ns("interval"), "Interval",
                         c("Not stated" = "", vocab$time_intervals(kind)),
                         selected = pf("interval"), width = "100%"),
      if (kind == "point_prevalence") {
        shiny::selectInput(ns("time_point"), "Time point within interval",
                           c("Not stated" = "", vocab$time_points),
                           selected = pf("time_point"), width = "100%")
      } else {
        shiny::selectInput(ns("level"), "Estimation level",
                           c("Not stated" = "", vocab$levels),
                           selected = pf("level"), width = "100%")
      }
    ),
    if (kind == "period_prevalence") bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::checkboxInput(ns("full_contribution"), "Require full interval contribution",
                           value = isTRUE(pf("full_contribution", FALSE)), width = "100%"),
      shiny::checkboxInput(ns("complete_database_intervals"), "Complete database intervals only",
                           value = isTRUE(pf("complete_database_intervals", TRUE)), width = "100%")
    ),

    section_heading("Stratification"),
    strata_ui(ns, pf)
  )
}

prevalence_collect <- function(kind) function(input) {
  common <- list(
    denominator_cohort_id              = chr_or_null(input$denominator_cohort_id),
    outcome_cohort_id                  = chr_or_null(input$outcome_cohort_id),
    denominator_cohort_ids_of_interest = vec_or_null(chr_vec(input$denominator_cohort_ids_of_interest)),
    outcome_cohort_ids_of_interest     = vec_or_null(chr_vec(input$outcome_cohort_ids_of_interest)),
    interval                           = chr_or_null(input$interval)
  )
  specific <- if (kind == "point_prevalence") list(
    time_point = chr_or_null(input$time_point)
  ) else list(
    complete_database_intervals = isTRUE(input$complete_database_intervals %||% TRUE),
    full_contribution           = isTRUE(input$full_contribution),
    level                       = chr_or_null(input$level)
  )
  compact(c(common, specific, list(
    strata                 = parse_strata(input$strata),
    include_overall_strata = isTRUE(input$include_overall_strata %||% TRUE)
  )))
}

prevalence_flatten <- function(p) {
  p$strata <- strata_tokens(p$strata)
  p
}

register_analysis_template(
  "point_prevalence",
  hint = "Proportion of a denominator population with the condition at a point in each interval.",
  ui = prevalence_ui("point_prevalence"),
  collect = prevalence_collect("point_prevalence"),
  pickers = list(cohorts = c("denominator_cohort_id", "outcome_cohort_id"), strata = "strata"),
  denominator = "denominator_cohort_id",
  flatten = prevalence_flatten
)

register_analysis_template(
  "period_prevalence",
  hint = "Proportion of a denominator population with the condition at any time during each interval.",
  ui = prevalence_ui("period_prevalence"),
  collect = prevalence_collect("period_prevalence"),
  pickers = list(cohorts = c("denominator_cohort_id", "outcome_cohort_id"), strata = "strata"),
  denominator = "denominator_cohort_id",
  flatten = prevalence_flatten
)
