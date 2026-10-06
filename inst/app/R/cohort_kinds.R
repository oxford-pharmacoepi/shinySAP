# Cohort type registry --------------------------------------------------------
#
# A cohort's `type` is one of the schema's cohort types, and decides which
# parameters it carries:
#
#   concept_cohort      CohortConstructor::conceptCohort(): built from a codelist
#   target              a pre-existing cohort of the population of interest,
#                       instantiated outside this script -- no parameters
#   denominator         generateDenominatorCohortSet(age_group, sex,
#                       days_prior_observation, requirement_interactions)
#   target_denominator  generateTargetDenominatorCohortSet(): the same, restricted
#                       to a target cohort's episodes, with a date range and time
#                       at risk
#
# Same shape as analysis_registry.R: a registry keyed by type id with ui/collect/
# flatten mirroring one another. Input ids ARE the schema parameter names, so
# the key written and the field that produced it cannot drift apart.
#
# Sourced before dynamic_items.R, problems.R and utils.R (alphabetical), so
# nothing here may call them at the top level.

# generateDenominatorCohortSet(sex =): one of Male, Female or Both.
COHORT_SEXES <- c("Both", "Male", "Female")

# ageGroup defaults to list(c(0, 150)); 150 is the package's open-ended upper age.
AGE_MAX <- 150

# The half of the card every cohort shares; everything else is `parameters`.
COHORT_COMMON_FIELDS <- c("id", "name", "data_source_id", "type")

# Ids a type's ui() renders that are OUTPUTS, not inputs: the live preview of the
# generated cohort set. They hold no value, so collect() never reads them.
COHORT_DISPLAY_ONLY_IDS <- "cohort_set_preview"

# Bounded intervals ------------------------------------------------------------
#
# age_group and time_at_risk are lists of numeric pairs, list(c(0, 17), c(18, 30)),
# so they share a parser. An open upper bound is Inf in R (and null in the file;
# shinySAP's JSON layer handles that).

# "0, 30" / "0-30" / "18-64" / "0, Inf" / "65+"  ->  c(lo, hi), hi = Inf if open.
parse_bounds <- function(s) {
  s <- trimws(s)
  if (!nzchar(s)) return(NULL)
  num <- function(p) {
    if (tolower(p) %in% c("inf", "infinity", "")) return(Inf)
    suppressWarnings(as.numeric(p))
  }
  if (grepl("\\+\\s*$", s)) {
    lo <- num(sub("\\+\\s*$", "", s))
    return(if (is.na(lo)) NULL else c(lo, Inf))
  }
  parts <- trimws(unlist(strsplit(s, "[,-]")))
  parts <- parts[nzchar(parts)]
  if (length(parts) != 2) return(NULL)
  pair <- c(num(parts[1]), num(parts[2]))
  if (any(is.na(pair))) NULL else pair
}

# A textarea of one interval per line -> a list of numeric pairs (NULL if none).
parse_bound_list <- function(x) {
  pairs <- lapply(split_lines(x), parse_bounds)
  pairs <- pairs[!vapply(pairs, is.null, logical(1))]
  if (length(pairs)) pairs else NULL
}

# A list of pairs -> textarea lines, for pf() on the way back in. `open` is what
# an unbounded upper bound reads as.
format_bound_list <- function(pairs, open = "Inf") {
  if (!length(pairs)) return(character(0))
  vapply(pairs, function(p) {
    p <- as.numeric(unlist(p))
    hi <- if (length(p) >= 2 && is.finite(p[[2]])) format(p[[2]]) else as.character(open)
    paste0(format(p[[1]]), ", ", hi)
  }, character(1))
}

# The registry ----------------------------------------------------------------

COHORT_TEMPLATES <- list()
cohort_registry_env <- environment()

# hint      one line shown above the block, or NULL
# ui        function(ns, pf) -> the type's inputs
# collect   function(input) -> the type's `parameters`, reading ONLY its own ids
# pickers   input ids that pick another item, by entity: list(cohorts =, codelists =)
# flatten   function(parameters) -> prefill keys; the inverse of collect()
register_cohort_type <- function(type_id, hint = NULL, ui, collect,
                                 pickers = list(), flatten = function(p) p) {
  cohort_registry_env$COHORT_TEMPLATES[[type_id]] <- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers, flatten = flatten
  )
}

# A type the schema knows but no template covers still round-trips: it renders
# a note, collects nothing, and keeps whatever parameters the file held.
EMPTY_COHORT_TEMPLATE <- list(
  hint = "This cohort type has no fields in this app.",
  ui = function(ns, pf) NULL,
  collect = function(input) list(),
  pickers = list(),
  flatten = function(p) list()
)

cohort_template <- function(type) {
  type <- as.character(type %||% "")
  if (!nzchar(type)) return(EMPTY_COHORT_TEMPLATE)
  COHORT_TEMPLATES[[type]] %||% EMPTY_COHORT_TEMPLATE
}

# One saved (or live) cohort -> the prefill its card is rebuilt from. Shared by
# load() and the Duplicate button, so the two can never drift.
cohort_to_prefill <- function(ch) {
  c(ch[intersect(COHORT_COMMON_FIELDS, names(ch))],
    cohort_template(ch$type)$flatten(ch$parameters %||% list()))
}

# The label the schema gives a type id.
cohort_type_label <- function(type) {
  types <- shinySAP::sapSchemaTypes("cohort")
  hit <- names(types)[match(as.character(type %||% ""), types)]
  if (is.na(hit)) as.character(type %||% "") else hit
}

# The cohort list as selectize optgroups, one group per type in schema order.
# `index` is id -> list(name, type). Grouped, not filtered: an author picking a
# denominator can see which entries are denominators, while the semantic checks
# on Review stay the thing that enforces it. Untyped cohorts get a trailing
# group rather than being hidden.
grouped_cohort_choices <- function(index) {
  index <- index %||% list()
  if (!length(index)) return(list())
  types <- shinySAP::sapSchemaTypes("cohort")
  label_of <- function(id) {
    nm <- trimws(as.character(index[[id]]$name %||% ""))
    if (nzchar(nm)) nm else id
  }
  type_of <- vapply(names(index), function(id) as.character(index[[id]]$type %||% ""), character(1))
  out <- list()
  for (i in seq_along(types)) {
    ids <- names(index)[type_of == types[[i]]]
    if (length(ids)) out[[names(types)[[i]]]] <- stats::setNames(ids, vapply(ids, label_of, character(1)))
  }
  untyped <- names(index)[!type_of %in% types]
  if (length(untyped)) out[["No type chosen"]] <- stats::setNames(untyped, vapply(untyped, label_of, character(1)))
  out
}

# Shared blocks ---------------------------------------------------------------

# The requirements every denominator cohort set is generated with, target or not.
denominator_requirements_ui <- function(ns, pf) shiny::tagList(
  bslib::layout_columns(
    col_widths = c(6, 6),
    # age_group = list(c(0, 17), c(18, 30)): numeric pairs, one cohort each.
    shiny::textAreaInput(ns("age_group"), "Age groups (one per line, as lower, upper)",
                         pf("age_group"), rows = 4, width = "100%",
                         placeholder = "0, 17\n18, 64\n65, 150"),
    shiny::div(
      # Nothing preselected: a prefilled "Both" would be a decision the author
      # never made. The generator's own default still applies downstream -- the
      # preview says so -- but the SAP records only what was chosen.
      shiny::selectInput(ns("sex"), "Sex", c("Not stated" = "", COHORT_SEXES),
                         selected = pf("sex"), width = "100%"),
      shiny::numericInput(ns("days_prior_observation"), "Days of prior observation required",
                          value = pf("days_prior_observation", NULL), min = 0, step = 1, width = "100%")
    )
  ),
  shiny::checkboxInput(ns("requirement_interactions"),
                       "Generate a cohort for every combination of the requirements",
                       value = isTRUE(pf("requirement_interactions", TRUE)), width = "100%")
)

denominator_requirements_collect <- function(input) compact(list(
  age_group                = parse_bound_list(input$age_group),
  sex                      = chr_or_null(input$sex),
  days_prior_observation   = num_or_null(input$days_prior_observation),
  requirement_interactions = isTRUE(input$requirement_interactions %||% TRUE)
))

denominator_requirements_flatten <- function(p) list(
  age_group                = join_lines(format_bound_list(p$age_group, open = AGE_MAX)),
  sex                      = p$sex,
  days_prior_observation   = p$days_prior_observation,
  requirement_interactions = p$requirement_interactions %||% TRUE
)

# What the generator actually produces ------------------------------------------
#
# A denominator cohort is a cohort SET: the arguments on the card are the axes
# it is crossed over. With one sex and one prior-observation value the set is
# one cohort per age group, times one per time-at-risk window for a target
# denominator. The preview spells them out so the author sees what the
# arguments generate. Unset fields use the generator's defaults.
denominator_cohort_set <- function(cohort) {
  p <- cohort$parameters %||% list()
  ages  <- if (length(p$age_group)) p$age_group else list(c(0, AGE_MAX))
  sex   <- as.character(p$sex %||% "Both")
  prior <- as.numeric(p$days_prior_observation %||% 0)
  windows <- if (identical(as.character(cohort$type %||% ""), "target_denominator")) {
    if (length(p$time_at_risk)) p$time_at_risk else list(c(0, Inf))
  } else {
    list(NULL)
  }
  out <- list()
  for (w in windows) {
    for (a in ages) {
      out[[length(out) + 1]] <- list(age_group = a, sex = sex, days_prior_observation = prior,
                                     time_at_risk = w)
    }
  }
  out
}

# One generated cohort as a line: "Age 18, 64 | Female | 365 days prior observation".
format_denominator_cohort <- function(x) {
  parts <- c(
    sprintf("Age %s", format_bound_list(list(x$age_group), open = AGE_MAX)),
    x$sex,
    sprintf("%s days prior observation", format(x$days_prior_observation))
  )
  if (!is.null(x$time_at_risk)) {
    parts <- c(parts, sprintf("time at risk %s", format_bound_list(list(x$time_at_risk))))
  }
  paste(parts, collapse = " | ")
}

# The facts grid plus the generated set, one styled block. Shared by the analysis
# card's summary and the cohort card's own preview, so the two views agree.
denominator_panel <- function(cohort, intro, lead = NULL) {
  p <- cohort$parameters %||% list()
  fact <- function(label, value) {
    shiny::div(class = "col", shiny::tags$span(class = "text-muted", label), shiny::tags$br(),
               shiny::tags$strong(value))
  }
  none <- function(x) {
    x <- as.character(unlist(x %||% character(0)))
    if (!length(x) || !any(nzchar(x))) "--" else paste(x, collapse = ", ")
  }
  bounds <- function(pairs, open = "Inf") {
    if (!length(pairs)) return("--")
    paste(format_bound_list(pairs, open = open), collapse = " | ")
  }
  is_target <- identical(as.character(cohort$type %||% ""), "target_denominator")
  shiny::div(
    class = "border rounded bg-body-tertiary p-3 mb-3 small",
    shiny::div(class = "text-muted mb-2", intro),
    shiny::div(
      class = "row row-cols-auto gap-3",
      if (is_target) fact("Cohort date range", {
        dr <- p$target_cohort_date_range
        if (is.null(dr)) "--" else paste(none(date_bound(dr, 1)), "to", none(date_bound(dr, 2)))
      }),
      fact("Age groups", bounds(p$age_group, open = AGE_MAX)),
      fact("Sex", none(p$sex)),
      fact("Prior observation", paste(none(p$days_prior_observation), "days")),
      if (is_target) fact("Time at risk (days from target entry)", bounds(p$time_at_risk))
    ),
    denominator_cohort_set_ui(cohort, lead = lead)
  )
}

denominator_cohort_set_ui <- function(cohort, lead = NULL) {
  set <- denominator_cohort_set(cohort)
  n   <- length(set)
  msg <- if (is.null(lead)) {
    sprintf("This cohort set generates %d cohort%s, and the analysis runs on %s:",
            n, if (n == 1) "" else "s", if (n == 1) "it" else "all of them")
  } else {
    lead(n)
  }
  shiny::div(
    class = "mt-3",
    shiny::div(class = "text-muted mb-1", msg),
    shiny::div(
      style = "max-height: 18rem; overflow: auto;",
      class = "border rounded bg-body px-2 py-1",
      shiny::tags$ol(class = "mb-0 ps-5 font-monospace",
                     lapply(set, function(x) shiny::tags$li(format_denominator_cohort(x))))
    )
  )
}

# The types -------------------------------------------------------------------

register_cohort_type(
  "concept_cohort",
  hint = paste("Built with CohortConstructor::conceptCohort(): every record of a codelist's",
               "concepts becomes a cohort entry."),
  ui = function(ns, pf) bslib::layout_columns(
    col_widths = c(4, 4, 4),
    entity_picker(ns("codelist_id"), "Codelist", pf("codelist_id"),
                  placeholder = "A codelist from the Codelists tab"),
    shiny::selectInput(ns("exit"), "Cohort exit",
                       c("Not stated" = "", "event_end_date", "event_start_date"),
                       selected = pf("exit"), width = "100%"),
    shiny::selectInput(ns("overlap"), "Overlapping records",
                       c("Not stated" = "", "merge", "extend"),
                       selected = pf("overlap"), width = "100%")
  ),
  collect = function(input) compact(list(
    codelist_id = chr_or_null(input$codelist_id),
    exit        = chr_or_null(input$exit),
    overlap     = chr_or_null(input$overlap)
  )),
  pickers = list(codelists = "codelist_id")
)

register_cohort_type(
  "target",
  hint = paste("A pre-existing cohort of the population of interest -- people and the",
               "episodes during which they belong to it. Instantiated outside this",
               "script; a target denominator is generated FROM it."),
  ui = function(ns, pf) NULL,
  collect = function(input) list(),
  flatten = function(p) list()
)

register_cohort_type(
  "denominator",
  hint = paste("Generated with generateDenominatorCohortSet(): a denominator cohort set",
               "built from the whole database, not defined by entry criteria."),
  ui = function(ns, pf) shiny::tagList(
    denominator_requirements_ui(ns, pf),
    shiny::uiOutput(ns("cohort_set_preview"))
  ),
  collect = denominator_requirements_collect,
  flatten = denominator_requirements_flatten
)

register_cohort_type(
  "target_denominator",
  hint = paste("Generated with generateTargetDenominatorCohortSet(): the same requirements,",
               "restricted to the time a person spends in a target cohort."),
  ui = function(ns, pf) shiny::tagList(
    entity_picker(ns("target_cohort_id"), "Target cohort to build the denominator from",
                  pf("target_cohort_id"), placeholder = "Another cohort defined on this tab"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::div(
        date_input(ns("target_cohort_date_range_start"), "Cohort date range: earliest start",
                   pf("target_cohort_date_range_start")),
        shiny::div(class = "form-text", "Blank = the earliest observation period in the database.")
      ),
      shiny::div(
        date_input(ns("target_cohort_date_range_end"), "Cohort date range: latest end",
                   pf("target_cohort_date_range_end")),
        shiny::div(class = "form-text", "Blank = the latest observation period in the database.")
      )
    ),
    # time_at_risk = list(c(0, 30), c(31, 60)): days from target cohort entry.
    shiny::textAreaInput(ns("time_at_risk"),
                         "Time at risk (one interval per line, days from target cohort entry)",
                         pf("time_at_risk"), rows = 3, width = "100%",
                         placeholder = "0, Inf\n0, 30\n31, 60"),
    denominator_requirements_ui(ns, pf),
    shiny::checkboxInput(ns("requirements_at_entry"),
                         "Requirements must be met on the target cohort start date",
                         value = isTRUE(pf("requirements_at_entry", TRUE)), width = "100%"),
    shiny::uiOutput(ns("cohort_set_preview"))
  ),
  # Keys in the schema's order.
  collect = function(input) {
    req <- denominator_requirements_collect(input)
    target <- chr_vec(input$target_cohort_id)
    compact(list(
      target_cohort_id         = if (length(target)) target else NULL,
      target_cohort_date_range = date_range_value(input$target_cohort_date_range_start,
                                                  input$target_cohort_date_range_end),
      time_at_risk             = parse_bound_list(input$time_at_risk),
      age_group                = req$age_group,
      sex                      = req$sex,
      days_prior_observation   = req$days_prior_observation,
      requirements_at_entry    = isTRUE(input$requirements_at_entry %||% TRUE),
      requirement_interactions = req$requirement_interactions
    ))
  },
  pickers = list(cohorts = "target_cohort_id"),
  flatten = function(p) c(
    list(
      target_cohort_id               = p$target_cohort_id,
      target_cohort_date_range_start = date_bound(p$target_cohort_date_range, 1),
      target_cohort_date_range_end   = date_bound(p$target_cohort_date_range, 2),
      time_at_risk                   = join_lines(format_bound_list(p$time_at_risk)),
      requirements_at_entry          = p$requirements_at_entry %||% TRUE
    ),
    denominator_requirements_flatten(p)
  )
)
