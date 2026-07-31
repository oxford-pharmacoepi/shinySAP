# Study-level metadata that heads the SAP -----------------------------------

# One row of the amendment history: what changed in which version, and when.
amendment_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Amendment",
    layout_columns(
      col_widths = c(3, 3, 6),
      textInput(ns("version"), "Version", pf("version"), width = "100%"),
      date_input(ns("date"), "Date", pf("date")),
      div()
    ),
    textAreaInput(ns("description"), "Description of change", pf("description"),
                  rows = 2, width = "100%")
  )
}

amendment_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    item_card_label(output, reactive({
      v <- trimws(input$version %||% "")
      d <- trimws(as.character(input$date %||% ""))
      lab <- paste(c(if (nzchar(v)) paste0("v", v), if (nzchar(d)) d),
                   collapse = " — ")
      if (nzchar(lab)) lab else "Untitled"
    }))

    reactive(list(
      version     = blank_to_na(input$version),
      date        = blank_to_na(as.character(input$date %||% "")),
      description = blank_to_na(input$description)
    ))
  })
}

# Problems OF THE STUDY SECTION, shaped like the per-cohort entries in
# problems_r (see cohort_kinds.R). Warn-not-block, like every other problem here.
#
# An unstated minimum cell count is the only one so far, and it is here rather
# than left to the author's memory because its failure mode is silent: no field,
# no row in the study table, no suppress() in the generated script, and nothing
# anywhere that says results will leave unsuppressed. Every other undecided
# field in this app degrades to a documented package default; this one degrades
# to exporting small counts. That asymmetry is what earns it a flag.
study_problems <- function(study) {
  n <- suppressWarnings(as.numeric(study$min_cell_count %||% NA))
  if (length(n) == 1 && !is.na(n)) return(list())
  list(list(
    name = "Study information",
    messages = paste(
      "No minimum cell count. The generated script will apply no suppression,",
      "and the plan states no export threshold -- set one, or confirm the data",
      "partners' own disclosure rules govern instead."
    )
  ))
}

# Objectives and the analyses that answer them, checked BOTH ways.
#
# An objective nothing implements is a plan that promises a result it will not
# produce; an analysis serving no objective is work nobody asked for. Neither is
# an error -- a SAP is written incrementally, and half-finished is the normal
# state of one -- so both are warnings, like every other problem here.
#
# The link is many-to-many: one objective is usually served by several analyses
# (complete, 5-year and 2-year prevalence of the same disease), so this counts
# coverage rather than expecting a one-to-one pairing.
objective_coverage_problems <- function(study, analyses) {
  objs <- study$objectives %||% list()
  analyses <- analyses %||% list()
  if (!length(objs) && !length(analyses)) return(list())

  ids  <- vapply(objs, function(o) objective_id(o) %||% "", character(1))
  cited <- unique(unlist(lapply(analyses, function(a)
    as.character(unlist(a$objectives %||% list())))))
  cited <- cited[!is.na(cited) & nzchar(cited)]
  found <- list()

  uncovered <- which(!ids %in% cited & nzchar(ids))
  if (length(uncovered)) {
    found[[length(found) + 1]] <- list(
      name = "Objectives",
      messages = vapply(uncovered, function(i) sprintf(
        "Objective %d has no analysis: %s", i, objective_text(objs[[i]])),
        character(1)))
  }

  # An id an analysis names that no objective has. Usually a reworded objective:
  # the wording changed, the id was reminted, and the analysis still points at
  # the old one -- which is exactly the break worth surfacing.
  for (a in analyses) {
    named <- as.character(unlist(a$objectives %||% list()))
    named <- named[!is.na(named) & nzchar(named)]
    if (!length(named)) {
      found[[length(found) + 1]] <- list(
        name = as.character(a$name %||% "Untitled analysis"),
        messages = "This analysis answers no objective; say which one it is for.")
      next
    }
    missing <- setdiff(named, ids)
    if (length(missing)) {
      found[[length(found) + 1]] <- list(
        name = as.character(a$name %||% "Untitled analysis"),
        messages = sprintf(paste("Names an objective that no longer exists (%s) --",
                                 "an objective reworded after this analysis was",
                                 "written gets a new id."),
                           paste(missing, collapse = ", ")))
    }
  }
  found
}

study_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Study information"),
    p(class = "text-muted", "Identifying details for the statistical analysis plan."),
    layout_columns(
      col_widths = c(8, 4),
      textInput(ns("title"), "Study title", width = "100%"),
      textInput(ns("study_code"), "Study code", width = "100%")
    ),
    layout_columns(
      col_widths = c(5, 2, 2, 3),
      textInput(ns("authors"), "Authors (comma separated)", width = "100%"),
      textInput(ns("version"), "SAP version", value = "1.0", width = "100%"),
      # omopgenerics::suppress(minCellCount =). A study-level rule, not a
      # per-analysis one: it governs what may leave the data partner at all, so
      # every result this SAP produces is suppressed at the same threshold.
      #
      # Deliberately NOT prefilled, unlike the SAP version and date. 5 is the
      # function's own default, but a threshold is a governance decision the
      # data partners own, and theirs differ -- omopgenerics defaults to 5 while
      # DARWIN EU's own notice states 100 for the catalogue. A prefilled number
      # is one an author can tab past without choosing, which is how a plan ends
      # up stating an export rule nobody actually decided.
      #
      # Blank is therefore a real state, not an oversight to paper over -- and
      # study_problems() flags it, so it cannot leave silently either.
      div(
        # NULL, not NA: numericInput() renders NA as the literal string "NA" in
        # the box. NULL omits the value attribute, which is a genuinely empty
        # field.
        numericInput(ns("min_cell_count"), "Minimum cell count",
                     value = NULL, min = 0, step = 1, width = "100%"),
        div(class = "form-text", "Counts below this are suppressed before export.")
      ),
      dateInput(ns("date"), "Date", value = Sys.Date(), width = "100%")
    ),
    textAreaInput(ns("background"), "Rationale and background", rows = 4, width = "100%"),
    textAreaInput(
      ns("aim"), "Aim / research question",
      rows = 2, width = "100%",
      placeholder = "The aim of this study is to ..."
    ),
    textAreaInput(
      ns("objectives"), "Specific objectives (one per line)",
      rows = 4, width = "100%",
      placeholder = "Estimate the incidence of X in cohort Y\nCharacterise ..."
    ),
    div(
      class = "d-flex justify-content-between align-items-center mt-4 mb-3",
      div(
        h5("Amendment history", class = "mb-1"),
        p(class = "text-muted mb-0", "What changed in each version of this SAP.")
      ),
      actionButton(ns("add_amendment"), "Add amendment",
                   class = "btn btn-outline-primary btn-sm", icon = icon("plus"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n_amendments")),
      empty_state("No amendments recorded yet.")
    ),
    div(id = ns("amendments"))
  )
}

study_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    amendments <- dynamic_items("amendment", "amendments",
                                amendment_item_ui, amendment_item_server)

    # A new amendment is the next version, made today. Both stay editable.
    observeEvent(input$add_amendment, amendments$add(list(
      version = next_sap_version(input$version),
      date    = as.character(Sys.Date())
    )))

    output$n_amendments <- renderText(amendments$count())
    outputOptions(output, "n_amendments", suspendWhenHidden = FALSE)

    # Objectives are card STATE, not just the textarea's contents, because they
    # carry ids the analyses reference. The textarea still holds one per line --
    # that is the authoring UX -- and every edit is reconciled against the ids
    # already held, so reordering keeps them (see reconcile_objectives()).
    objectives <- reactiveVal(list())
    observeEvent(input$objectives, {
      objectives(reconcile_objectives(split_lines(input$objectives), objectives()))
    }, ignoreNULL = FALSE)

    data <- reactive(list(
      title      = blank_to_na(input$title),
      study_code = blank_to_na(input$study_code),
      authors    = as_array(trimws(split_lines(gsub(",", "\n", input$authors %||% "")))),
      version    = blank_to_na(input$version),
      # A cleared field sends NA, which stays NA: "no suppression stated" is a
      # different claim from "suppress at 0", and only one of them is safe to
      # silently write into a plan that governs what leaves a data partner.
      min_cell_count = suppressWarnings(as.numeric(input$min_cell_count %||% NA)),
      date       = as.character(input$date %||% NA),
      background = blank_to_na(input$background),
      aim        = blank_to_na(input$aim),
      objectives = objectives(),
      amendments = amendments$data()
    ))

    load <- function(study) {
      updateTextInput(session, "title", value = study$title %||% "")
      updateTextInput(session, "study_code", value = study$study_code %||% "")
      updateTextInput(session, "authors", value = paste(unlist(study$authors), collapse = ", "))
      updateTextInput(session, "version", value = study$version %||% "1.0")
      # A file with no threshold loads blank, and stays blank: the preview says
      # Not stated, the script emits no suppress(), and study_problems() says so
      # on the Review tab. Substituting 5 here would invent an export rule the
      # plan never contained and hide the gap behind a plausible number -- the
      # one failure this field cannot afford, because nothing downstream would
      # ever show that the threshold was the app's guess rather than the
      # author's decision.
      #
      # "" clears the box, and it is the only value that does. NA would write the
      # string "NA" into it, and NULL is dropped from the update message
      # entirely -- which would leave the PREVIOUS SAP's threshold sitting there,
      # the one way this field could show a number belonging to another study.
      mcc <- suppressWarnings(as.numeric(study$min_cell_count %||% NA))
      updateNumericInput(session, "min_cell_count",
                         value = if (is.na(mcc)) "" else mcc)
      if (!is.null(study$date)) {
        updateDateInput(session, "date", value = as.Date(study$date))
      }
      updateTextAreaInput(session, "background", value = study$background %||% "")
      updateTextAreaInput(session, "aim", value = study$aim %||% "")
      # Seed the ids first: the textarea update fires the observer above, which
      # would otherwise mint fresh ids for objectives that already have them and
      # orphan every analysis pointing at them.
      objectives(study$objectives %||% list())
      updateTextAreaInput(session, "objectives",
                          value = join_lines(vapply(objectives(), objective_text,
                                                    character(1))))
      amendments$clear()
      for (a in study$amendments %||% list()) amendments$add(a)
    }

    list(data = data, load = load)
  })
}
