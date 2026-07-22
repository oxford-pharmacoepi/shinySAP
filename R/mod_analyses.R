# Section 4: Analyses ----------------------------------------------------------
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
      # A new card starts with NO type: the type decides everything else on the
      # card, so it is the author's first decision, not a default -- the same
      # rule as the cohort kind.
      selectInput(ns("analysis_type"), "Analysis type",
                  c("Choose a type…" = "", ANALYSIS_TYPES),
                  selected = canonical_analysis_type(pf("analysis_type")),
                  width = "100%")
    ),
    # WHY this analysis exists. Many-to-many on purpose: one objective is
    # usually answered by several analyses, and an analysis can serve more than
    # one. Ids, not text, so rewording an objective in the study card does not
    # silently repoint every analysis -- objective_coverage_problems() reports
    # the break instead.
    div(
      class = "objectives-picker",
      selectizeInput(ns("objectives"), "Objectives this analysis answers",
                     choices = character(0),
                     selected = as.character(unlist(pf("objectives", character(0)))),
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = "Objectives from the Study tab"))
    ),
    entity_picker(ns("data_sources"), "CDM sources this analysis runs on",
                  pf("data_sources", character(0)), multiple = TRUE,
                  placeholder = "Select or type one or more CDM sources"),
    tags$hr(class = "my-3"),
    uiOutput(ns("type_fields"))
  )
}

analysis_item_server <- function(id, prefill = NULL, on_remove = function() {},
                                 objective_choices = reactive(character(0)),
                                 cohort_names = reactive(character(0)),
                                 source_names = reactive(character(0)),
                                 cohort_index = reactive(list()),
                                 cohort_renames = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # session$ns, not NS(id): dynamic_items() hands the server the bare item id
    # and the fully-qualified one to the UI. They only line up because
    # moduleServer() runs under the parent's reactive domain.
    ns      <- session$ns
    base_pf <- prefiller(prefill)

    # What a collapsed card says it is: the analysis's name and type.
    item_card_label(output, reactive({
      nm   <- trimws(input$name %||% "")
      type <- type_r()
      paste0(if (nzchar(nm)) nm else "Untitled", " — ",
             if (nzchar(type)) type else "no type chosen")
    }))

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

    # "" until the author picks -- canonical_analysis_type() maps NULL/NA/""
    # there, so the ui's blank start and this reactive cannot disagree.
    type_r <- reactive(
      canonical_analysis_type(input$analysis_type %||% base_pf("analysis_type"))
    )

    # Only the type may invalidate this. A dependency on the field values would
    # rebuild the block on every keystroke, and one on the name reactives would
    # rebuild it whenever a cohort is edited in another tab; both steal focus
    # mid-edit.
    output$type_fields <- renderUI({
      type <- type_r()
      if (!nzchar(type)) {
        return(p(class = "text-muted small mb-0",
                 "Choose an analysis type above to see the fields it takes."))
      }
      tmpl <- analysis_template(type)
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
    # Grouped by kind, not filtered by it: an author picking a denominator can
    # see at a glance which entries actually are denominators, while free text
    # and not-yet-defined cohorts keep working and the template validators stay
    # the thing that enforces the rule.
    sync_pickers(session, function() analysis_template(type_r())$pickers$cohorts %||% character(0),
                 reactive(grouped_cohort_choices(cohort_index())), base_pf)
    sync_pickers(session, function() analysis_template(type_r())$pickers$sources %||% character(0),
                 source_names, base_pf)
    sync_pickers(session, "data_sources", source_names, base_pf)

    # The objectives picker is NOT a sync_pickers() one: those take free text and
    # match on the value shown, whereas this shows an objective's TEXT and stores
    # its id. Choices are therefore a named vector, rebuilt whenever the Study tab
    # changes, with the selection carried across explicitly.
    observe({
      choices <- objective_choices()
      current <- isolate(input$objectives)
      if (is.null(current)) current <- as.character(unlist(base_pf("objectives", character(0))))
      current <- current[!is.na(current) & nzchar(current)]
      # An id no objective has any more is KEPT in the choices, or shiny would
      # drop it silently on the next rebuild and the analysis would quietly stop
      # naming anything. objective_coverage_problems() reports it instead.
      orphans <- setdiff(current, as.character(choices))
      if (length(orphans)) {
        choices <- c(choices, stats::setNames(orphans,
                                              sprintf("%s (no longer an objective)", orphans)))
      }
      updateSelectizeInput(session, "objectives", choices = choices, selected = current)
    })

    # A cohort rename (see cohorts_server) lands on this card's cohort pickers
    # while they still hold the old name. Only the CURRENT template's pickers
    # exist in the DOM; a value stranded on a template switched away from keeps
    # the old name, and the validators catch it if that template comes back.
    observeEvent(cohort_renames(), {
      ev <- cohort_renames()
      apply_rename_to_pickers(session, input,
                              analysis_template(type_r())$pickers$cohorts %||% character(0),
                              ev$old, ev$new, available = cohort_names())
    }, ignoreInit = TRUE)

    # Sub-cohort ID pickers (template$subcohorts). Each maps a multi-select of
    # cohort IDs onto one of the template's cohort pickers: when the picked
    # cohort spans a set -- its own ID plus every cohort naming it as parent --
    # the multi-select renders with the whole set selected. One ID is not a
    # set, so nothing visible renders; the hidden empty picker is load-bearing,
    # though: it re-registers the input as empty, or IDs from a previously
    # picked set would linger in the input store and reach collect().
    subcohort_parent <- new.env(parent = emptyenv())
    subcohort_selected <- function(field, from, parent, values) {
      prev <- mget(field, envir = subcohort_parent, ifnotfound = list(NA))[[1]]
      assign(field, parent, envir = subcohort_parent)
      if (identical(prev, parent)) {
        current <- isolate(input[[field]])
        if (!is.null(current)) return(intersect(as.character(current), values))
      }
      # A saved selection only applies to the denominator it was saved with;
      # picking a different cohort resets to the full set.
      saved <- as.character(unlist(base_pf(field, NULL)))
      if (identical(parent, as.character(base_pf(from, ""))) && length(saved))
        return(intersect(saved, values))
      values
    }
    observe({
      specs <- analysis_template(type_r())$subcohorts
      for (field in names(specs)) local({
        f <- field
        spec <- specs[[f]]
        output[[paste0(f, "_ui")]] <- renderUI({
          choices <- subcohort_choices(input[[spec$from]], cohort_index())
          if (length(choices) < 2)
            return(div(style = "display: none;",
                       selectizeInput(ns(f), spec$label, choices = character(0),
                                      multiple = TRUE)))
          selectizeInput(ns(f), spec$label, choices = choices,
                         selected = subcohort_selected(f, spec$from, input[[spec$from]],
                                                       as.character(choices)),
                         multiple = TRUE, width = "100%")
        })
        outputOptions(output, paste0(f, "_ui"), suspendWhenHidden = FALSE)
      })
    })

    # Which input names the denominator differs by template (the registry's
    # `denominator` slot), so everything driven from it looks the id up first.
    denominator_pick <- function() {
      input[[analysis_template(type_r())$denominator %||% "denominator_cohort"]]
    }

    # Unlike the cohort and source pickers, the strata picker's choices come from
    # *another input on this same card* -- you may only stratify by a column the
    # chosen denominator cohort actually carries. So it re-syncs when the
    # denominator changes, not when the cohort list does.
    sync_pickers(session, function() analysis_template(type_r())$pickers$strata %||% character(0),
                 reactive(cohort_strata_variables(
                   cohort_by_name(cohort_index(), denominator_pick()))),
                 base_pf)

    # The denominator_summary_ui() block is a placeholder that this fills, because
    # it has to react to the picker and a static ui() cannot. Served
    # unconditionally: a template that does not use the block simply never renders
    # the placeholder, and this output goes unused.
    output$denominator_summary <- renderUI({
      # The raw pick rides along so an EMPTY pick prompts instead of warning.
      denominator_summary(cohort_by_name(cohort_index(), denominator_pick()),
                          picked = denominator_pick())
    })

    # For one round-trip after a type switch the new block's inputs have not
    # reported yet, so parameters briefly reads as nulls. Harmless while the only
    # consumers are the Review tab (suspended while the user is on Analyses) and
    # the Save button (a later click) -- but an autosave observe({ sap(); ... })
    # would capture the gap.
    data_r <- reactive({
      type <- type_r()
      common <- list(
        name          = blank_to_na(input$name),
        analysis_type = NA,   # overwritten below once a type is chosen
        objectives    = as_array(input$objectives %||% character(0)),
        data_sources  = as_array(input$data_sources %||% character(0))
      )
      # No type, no parameters: collecting through the fallback template would
      # write generic keys the author never saw. Unset saves as null.
      if (!nzchar(type)) return(common)
      tmpl <- analysis_template(type)
      # type_r(), not input$analysis_type: the type we emit and the collector
      # that produced `parameters` must never disagree. serialised_type may
      # refine the label (Prevalence -> estimatePointPrevalence), but the
      # aliases map it back to the same template on load.
      common$analysis_type <- if (is.null(tmpl$serialised_type)) type
                              else tmpl$serialised_type(input)
      # collect() reads only its own template's input ids, so values stranded
      # by a previously selected template never reach the JSON.
      c(common, list(parameters = tmpl$collect(input)))
    })

    # A template's validate() is written against a fully specified analysis. In
    # the gap after a type switch it would be looking at half-reported inputs, so
    # a thrown error here must not take the whole app down with it -- report it
    # as a problem like any other.
    problems_r <- reactive({
      d <- data_r()
      type <- canonical_analysis_type(d$analysis_type)
      # No type: no template to validate against, and the fallback would find
      # nothing wrong -- which would read as "fine".
      if (!nzchar(type)) {
        return("This analysis has no type chosen, so it carries no parameters and cannot be validated.")
      }
      tryCatch(
        as.character(analysis_template(type)$validate(d$parameters %||% list(), cohort_index())),
        error = function(e) paste("Could not validate this analysis:", conditionMessage(e))
      )
    })

    reactive(list(data = data_r(), problems = problems_r()))
  })
}

# One saved (or live) analysis -> the prefill its card is rebuilt from. Shared
# by analyses load() and the Duplicate button, so the two can never drift.
analysis_to_prefill <- function(a) {
  raw_type <- a$analysis_type
  a$analysis_type <- canonical_analysis_type(raw_type)
  tmpl <- analysis_template(a$analysis_type)
  # Pre-0.3.0 files kept the type-specific fields at the top level, so they
  # load with no migration step. is.null(), not %||%: an empty parameters
  # object reads back as a zero-length list, which %||% would take for
  # absent and then mistake a 0.3.0 file for an old one.
  params <- if (is.null(a$parameters)) a else a$parameters
  # A serialised_type template splits its type across two levels of the
  # file; hand flatten() the raw label so it can recover its inputs. The
  # canonical type in the common half shadows this key in the prefill.
  params$analysis_type <- raw_type
  flat <- tmpl$flatten(params)
  if (!is.list(flat)) flat <- list()
  # c(), not modifyList(): modifyList merges nested lists instead of
  # replacing them, drops keys whose new value is NULL, and errors on a
  # non-list -- after items$clear() has already emptied the section. And
  # because prefill[[key]] takes the first match, a template parameter that
  # collides with a common key cannot clobber it.
  c(a[intersect(ANALYSIS_COMMON_FIELDS, names(a))], flat)
}

analyses_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h3("Analyses", class = "mb-1"),
        p(class = "text-muted mb-0", "What is estimated, on which cohorts and sources, and how.")
      ),
      div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        actionButton(ns("add"), "Add analysis", class = "btn btn-primary", icon = icon("plus"))
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No analyses proposed yet.")
    ),
    div(id = ns("items"))
  )
}

analyses_server <- function(id, cohort_names = reactive(character(0)),
                            source_names = reactive(character(0)),
                            cohort_index = reactive(list()),
                            cohort_renames = reactive(NULL),
                            objective_choices = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {
    # These change on every keystroke in the section that owns them; settle first
    # so the pickers are not rebuilt, and the summary not redrawn, mid-word.
    # Renames are NOT debounced: the follow-up must land before the settled
    # name list rebuilds the pickers around the stale value.
    settled_objectives <- debounce(objective_choices, 600)
    settled_cohorts <- debounce(cohort_names, 600)
    settled_sources <- debounce(source_names, 600)
    settled_index   <- debounce(cohort_index, 600)

    item_server <- function(iid, prefill, on_remove) {
      analysis_item_server(iid, prefill, on_remove, settled_objectives,
                           settled_cohorts, settled_sources,
                           settled_index, cohort_renames)
    }
    items <- dynamic_items("analysis", "items", analysis_item_ui, item_server,
                           to_prefill = function(x) analysis_to_prefill(x$data),
                           noun = "Analysis")

    observeEvent(input$add, items$add(reveal = TRUE))

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(analyses) {
      items$clear()
      # analysis_to_prefill() also serves the Duplicate button, so load and
      # duplicate rebuild a card the same way.
      for (a in analyses) items$add(analysis_to_prefill(a))
    }

    # Each item now reports {data, problems}; the SAP only wants the data.
    data_r <- reactive(lapply(items$data(), function(x) x$data))

    # Problems, per analysis, for the Review tab to show and to refuse a save on.
    problems_r <- reactive({
      found <- lapply(items$data(), function(x) {
        msgs <- x$problems
        if (!length(msgs)) return(NULL)
        list(name = x$data$name %||% "Untitled analysis", messages = msgs)
      })
      found[!vapply(found, is.null, logical(1))]
    })

    list(data = data_r, load = load, problems = problems_r)
  })
}
