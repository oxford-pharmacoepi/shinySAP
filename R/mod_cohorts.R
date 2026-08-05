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
      col_widths = c(4, 4, 4),
      textInput(ns("name"), "Cohort name", pf("name"), width = "100%"),
      # The SAP-level counterpart of the generators' `cdm` argument: WHICH
      # databases this cohort is built against. A live handle is not a plan
      # field, so the plan names the CDM sources instead -- the same shape the
      # analyses use.
      entity_picker(ns("data_sources"), "CDM sources this cohort is generated from",
                    pf("data_sources", character(0)), multiple = TRUE,
                    placeholder = "One or more CDM sources"),
      # A new card starts with NO kind: which kind a cohort is decides everything
      # else on the card, so it is the author's first decision, not a default.
      selectInput(ns("kind"), "Kind", c("Choose a kind…" = "", COHORT_KINDS),
                  selected = canonical_cohort_kind(pf("kind")),
                  width = "100%")
    ),
    tags$hr(class = "my-3"),
    uiOutput(ns("kind_fields"))
  )
}

cohort_item_server <- function(id, prefill = NULL, on_remove = function() {},
                               cohort_names = reactive(character(0)),
                               on_rename = function(old, new) {},
                               renames = reactive(NULL),
                               source_names = reactive(character(0)),
                               cohort_index = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # Rename propagation, the EMIT half. The card knows its own previous name,
    # so a change is reported upward as {old -> new}; cohorts_server decides
    # whether it is unambiguous enough to act on. A blank emits nothing:
    # clearing a name is not a rename, and the previous name is kept so typing
    # a fresh one still links back to it.
    prev_name <- reactiveVal(NULL)
    observeEvent(input$name, {
      new <- trimws(input$name %||% "")
      old <- prev_name()
      if (nzchar(new)) prev_name(new)
      if (!is.null(old) && nzchar(old) && nzchar(new) && !identical(old, new)) {
        on_rename(old, new)
      }
    })

    ns      <- session$ns
    base_pf <- prefiller(prefill)

    # What a collapsed card says it is: the cohort's name and kind.
    item_card_label(output, reactive({
      nm   <- trimws(input$name %||% "")
      kind <- kind_r()
      kind_label <- if (!nzchar(kind)) "no kind chosen" else
        names(COHORT_KINDS)[match(kind, COHORT_KINDS)]
      paste0(if (nzchar(nm)) nm else "Untitled", " — ", kind_label)
    }))

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

    # "" until the author picks -- canonical_cohort_kind() maps NULL/NA/"" there.
    kind_r <- reactive(
      canonical_cohort_kind(input$kind %||% base_pf("kind"))
    )

    # Only the kind may invalidate this -- everything else is isolated inside
    # live_pf(), or typing would rebuild the block and steal focus.
    output$kind_fields <- renderUI({
      kind <- kind_r()
      if (!nzchar(kind)) {
        return(p(class = "text-muted small mb-0",
                 "Choose a kind above to see the fields this cohort carries."))
      }
      tmpl <- cohort_template(kind)
      tagList(
        if (!is.null(tmpl$hint)) p(class = "text-muted small mb-3", tmpl$hint),
        tmpl$ui(ns, live_pf)
      )
    })
    # The Cohorts tab is a hidden tab-pane until selected, and a hidden output
    # does not render -- without this a loaded SAP would leave every kind block
    # unbuilt, so its inputs would read NULL and every cohort would save empty.
    outputOptions(output, "kind_fields", suspendWhenHidden = FALSE)

    # The kind block above is deliberately static; the generated-set preview is
    # the one live piece, so it is its own output, filled here the same way
    # analysis_item_server() fills denominator_summary. It renders no inputs, so
    # recomputing on every keystroke steals no focus -- which is exactly the
    # point: flip requirementInteractions, or reorder the age groups, and the
    # list of cohorts it will generate changes in place.
    output$cohort_set_preview <- renderUI({
      kind <- kind_r()
      if (!is_denominator_kind(kind)) return(NULL)
      cohort <- c(list(kind = kind), cohort_template(kind)$collect(input))
      # The same panel the Analyses tab shows for this cohort, so what the
      # author sees while editing IS what an analysis will inherit. Unset
      # fields read "—" in the facts; the generator's defaults still fill
      # them in the generated set underneath.
      denominator_panel(
        cohort,
        "As the generator will read this card — unset fields use its defaults:",
        lead = function(n) sprintf(
          "These requirements generate %d denominator cohort%s; every analysis built on this cohort runs on %s:",
          n, if (n == 1) "" else "s", if (n == 1) "it" else "all of them"
        )
      )
    })

    # A target denominator names the cohort it is built from, so the cohort
    # pickers on a cohort card are fed by the cohort list itself.
    sync_pickers(session, function() cohort_template(kind_r())$pickers$cohorts %||% character(0),
                 reactive(grouped_cohort_choices(cohort_index())), base_pf)
    sync_pickers(session, "data_sources", source_names, base_pf)

    # Rename propagation, the FOLLOW half: another cohort's rename lands on any
    # of this card's cohort pickers still holding the old name (a target
    # denominator's targetCohortTable). ignoreInit: a card built after the
    # event must not replay it.
    observeEvent(renames(), {
      ev <- renames()
      apply_rename_to_pickers(session, input,
                              cohort_template(kind_r())$pickers$cohorts %||% character(0),
                              ev$old, ev$new, available = cohort_names())
    }, ignoreInit = TRUE)

    data_r <- reactive({
      kind <- kind_r()
      common <- list(
        name         = blank_to_na(input$name),
        kind         = blank_to_na(kind),   # unset saves as null, never a guess
        data_sources = as_array(input$data_sources %||% character(0))
      )
      # No kind, no kind block: collecting through the fallback template would
      # write plain-cohort keys the author never saw.
      if (!nzchar(kind)) return(common)
      # collect() reads only its own kind's input ids, so values stranded by a
      # previously selected kind never reach the JSON.
      c(common, cohort_template(kind)$collect(input))
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
      div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        actionButton(ns("add"), "Add cohort", class = "btn btn-primary", icon = icon("plus"))
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No cohorts defined yet.")
    ),
    div(id = ns("items"))
  )
}

cohorts_server <- function(id, source_names = reactive(character(0)),
                           codelist_names = reactive(character(0))) {
  moduleServer(id, function(input, output, session) {

    # The target-cohort picker on a cohort card is fed by the cohort list, which
    # is derived from the cards -- a cycle. reactiveVal only invalidates when the
    # value actually changes, which breaks it: editing anything other than a name
    # no longer re-triggers every picker on the tab.
    names_v <- reactiveVal(character(0))
    settled <- debounce(reactive(names_v()), 600)
    settled_sources <- debounce(source_names, 600)

    # The defined codelist names, shipped to the browser for the [bracket]
    # autocomplete and reference tinting in the cohort text fields
    # (www/codelist_refs.js). Debounced like the pickers: renaming a codelist
    # re-tints on the settled name, not on every keystroke.
    settled_codelists <- debounce(codelist_names, 600)
    observe({
      session$sendCustomMessage("codelist-names",
                                as.list(as.character(settled_codelists())))
    })

    # The same cycle-breaking trick for the picker's OPTGROUPS, which need each
    # cohort's kind as well as its name. Deriving this from by_name_r() would put
    # the whole cohort back in the loop, so every keystroke in an entry-event box
    # would re-trigger every picker on the tab. Name and kind are all the grouping
    # reads, so only those invalidate it.
    kinds_v <- reactiveVal(list())
    settled_index <- debounce(reactive(kinds_v()), 600)

    # Rename propagation, the GATE. A card reports {old -> new}; if no OTHER
    # card still holds the old name, one event goes out and every cohort picker
    # in the app follows it (the observers in the item servers, here and in
    # mod_analyses.R). If another card does still hold it, references now
    # legitimately point at THAT cohort -- moving them is not this card's call,
    # and the duplicate-name problem is already reported on Review. The seq
    # makes consecutive identical renames distinct, or reactiveVal would
    # swallow the second one.
    rename_seq <- 0
    rename_ev  <- reactiveVal(NULL)
    emit_rename <- function(old, new) {
      nms <- vapply(isolate(items$data()), function(x) {
        nm <- as.character(x$data$name %||% "")[1]
        if (is.na(nm)) "" else trimws(nm)
      }, character(1))
      if (any(nms == old)) return()
      rename_seq <<- rename_seq + 1
      rename_ev(list(seq = rename_seq, old = old, new = new))
    }

    item_server <- function(iid, prefill, on_remove) {
      cohort_item_server(iid, prefill, on_remove, settled,
                         on_rename = emit_rename, renames = rename_ev,
                         source_names = settled_sources,
                         cohort_index = settled_index)
    }
    items <- dynamic_items("cohort", "items", cohort_item_ui, item_server,
                           to_prefill = function(x) cohort_to_prefill(x$data),
                           noun = "Cohort")

    observeEvent(input$add, items$add(reveal = TRUE))

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    data_r <- reactive(lapply(items$data(), function(x) x$data))

    observe({
      d    <- data_r()
      raw  <- vapply(d, function(x) as.character(x$name %||% ""), character(1))
      keep <- nzchar(raw)
      nms  <- sort(unique(raw[keep]))
      if (!identical(nms, isolate(names_v()))) names_v(nms)

      idx <- stats::setNames(
        lapply(which(keep), function(i) list(kind = canonical_cohort_kind(d[[i]]$kind))),
        raw[keep])
      if (!identical(idx, isolate(kinds_v()))) kinds_v(idx)
    })

    load <- function(cohorts) {
      items$clear()
      # cohort_to_prefill() also serves the Duplicate button, so load and
      # duplicate rebuild a card the same way.
      for (ch in cohorts) items$add(cohort_to_prefill(ch))
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
    settled_names <- debounce(names_r, 600)

    problems_r <- reactive({
      idx   <- by_name_r()
      found <- lapply(data_r(), function(ch) {
        msgs <- cohort_problems(ch, idx)
        if (!length(msgs)) return(NULL)
        list(name = ch$name %||% "Untitled cohort", messages = msgs)
      })
      # Duplicated names shadow each other in every lookup, but no single card
      # can see that -- it is a problem OF THE LIST, appended alongside.
      c(found[!vapply(found, is.null, logical(1))], duplicate_name_problems(data_r()))
    })

    list(data = data_r, load = load, names = names_r, by_name = by_name_r,
         problems = problems_r, renames = rename_ev)
  })
}
