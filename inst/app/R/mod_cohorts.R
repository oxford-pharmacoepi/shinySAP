# Section: Cohorts ---------------------------------------------------------
#
# A cohort card is in two halves: the common fields (id, name, data sources,
# type) and a parameter block that depends on the type -- see cohort_kinds.R for
# the registry and what each type carries. Every edit writes a draft
# shinySAP::newSapCohort() back into the SAP through the section's CRUD.
# References between cards are by id, so renaming a cohort changes only its
# label in every picker.

cohort_item_ui <- function(id, prefill = NULL) {
  ns <- shiny::NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Cohort",
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::textInput(ns("name"), "Cohort name", pf("name"), width = "100%"),
      entity_picker(ns("data_source_id"), "Data sources this cohort is built in",
                    pf("data_source_id", character(0)), multiple = TRUE,
                    placeholder = "One or more data sources"),
      # A new card starts with NO type: the type decides everything else on the
      # card, so it is the author's first decision, not a default.
      shiny::selectInput(ns("type"), "Type",
                         c("Choose a type..." = "", shinySAP::sapSchemaTypes("cohort")),
                         selected = pf("type"), width = "100%")
    ),
    shiny::tags$hr(class = "my-3"),
    shiny::uiOutput(ns("type_fields"))
  )
}

cohort_item_server <- function(id, sap_id, prefill = NULL, on_remove = function() {},
                               write = function(component) {},
                               source_choices = shiny::reactive(character(0)),
                               codelist_choices = shiny::reactive(character(0)),
                               cohort_index = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    ns      <- session$ns
    base_pf <- prefiller(prefill)
    read    <- card_reader(input, prefill)

    item_card_label(output, shiny::reactive({
      nm   <- trimws(input$name %||% "")
      type <- type_r()
      paste0(if (nzchar(nm)) nm else "Untitled", " -- ",
             if (nzchar(type)) cohort_type_label(type) else "no type chosen",
             " -- ", sap_id)
    }))

    # Shiny keeps an input's last value after its node is destroyed, so a block
    # rebuilt after a type switch reads back what was typed into it. NULL means
    # never rendered -> fall back to the file. A length-1 NA means the user
    # cleared a numeric, and must stay cleared. Pickers are excluded:
    # sync_pickers() owns those.
    live_pf <- function(key, default = NULL) {
      v <- shiny::isolate(input[[key]])
      if (is.null(v)) return(base_pf(key, default))
      if (length(v) == 1 && is.na(v)) return(NULL)
      v
    }

    type_r <- shiny::reactive(as.character(input$type %||% base_pf("type") %||% ""))

    # Only the type may invalidate this, or typing would rebuild the block and
    # steal focus.
    output$type_fields <- shiny::renderUI({
      type <- type_r()
      if (!nzchar(type)) {
        return(shiny::p(class = "text-muted small mb-0",
                        "Choose a type above to see the fields this cohort carries."))
      }
      tmpl <- cohort_template(type)
      shiny::tagList(
        if (!is.null(tmpl$hint)) shiny::p(class = "text-muted small mb-3", tmpl$hint),
        tmpl$ui(ns, live_pf)
      )
    })
    # The tab is a hidden tab-pane until selected, and a hidden output does not
    # render -- without this a loaded SAP would leave every block unbuilt.
    shiny::outputOptions(output, "type_fields", suspendWhenHidden = FALSE)

    # The live preview of the set a denominator generates. It renders no inputs,
    # so recomputing on every keystroke steals no focus.
    output$cohort_set_preview <- shiny::renderUI({
      type <- type_r()
      if (!is_denominator_type(type)) return(NULL)
      cohort <- list(type = type, parameters = cohort_template(type)$collect(input))
      denominator_panel(
        cohort,
        "As the generator will read this card -- unset fields use its defaults:",
        lead = function(n) sprintf(
          "These requirements generate %d denominator cohort%s; every analysis built on this cohort runs on %s:",
          n, if (n == 1) "" else "s", if (n == 1) "it" else "all of them"
        )
      )
    })

    sync_pickers(session, function() cohort_template(type_r())$pickers$cohorts %||% character(0),
                 shiny::reactive(grouped_cohort_choices(cohort_index())), base_pf)
    sync_pickers(session, function() cohort_template(type_r())$pickers$codelists %||% character(0),
                 codelist_choices, base_pf)
    sync_pickers(session, "data_source_id", source_choices, base_pf)

    # The card as a draft component, written on every edit. collect() reads only
    # its own type's input ids, so values stranded by a previously selected type
    # never reach the SAP.
    shiny::observe({
      type <- type_r()
      write(shinySAP::newSapCohort(
        sap_id,
        name         = chr_or_null(read("name")),
        dataSourceId = chr_vec(read("data_source_id")),
        type         = chr_or_null(type),
        parameters   = card_parameters(input, type, cohort_template(type), prefill),
        validate     = FALSE
      ))
    })
  })
}

cohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::h3("Cohorts", class = "mb-1"),
        shiny::p(class = "text-muted mb-0", "Populations the analyses are run against.")
      ),
      shiny::div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        shiny::actionButton(ns("add"), "Add cohort", class = "btn btn-primary", icon = shiny::icon("plus"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No cohorts defined yet.")
    ),
    shiny::div(id = ns("items"))
  )
}

# `cohort_index` is the app-level view id -> list(name, type) that feeds the
# cohort pickers (a target denominator names another cohort).
cohorts_server <- function(id, sap,
                           source_choices = shiny::reactive(character(0)),
                           codelist_choices = shiny::reactive(character(0)),
                           cohort_index = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    settled_sources   <- shiny::debounce(shiny::reactive(source_choices()), 600)
    settled_codelists <- shiny::debounce(shiny::reactive(codelist_choices()), 600)
    settled_index     <- shiny::debounce(shiny::reactive(cohort_index()), 600)

    coll <- sap_collection(
      sap, "cohorts",
      new_component = function(id) shinySAP::newSapCohort(id, validate = FALSE),
      copy = function(source, id) shinySAP::newSapCohort(
        id, name = copy_name(source$name), dataSourceId = source$data_source_id,
        type = source$type, parameters = source$parameters, validate = FALSE),
      to_prefill = cohort_to_prefill
    )
    item_server <- function(iid, sap_id, prefill, on_remove) {
      cohort_item_server(iid, sap_id, prefill, on_remove,
                         write = function(component) if (items$is_live(iid, sap_id)) coll$write(component),
                         source_choices = settled_sources,
                         codelist_choices = settled_codelists,
                         cohort_index = settled_index)
    }
    items <- dynamic_items("cohort", "items", cohort_item_ui, item_server,
                           ids = coll$ids, prefill_of = coll$prefill_of, noun = "Cohort",
                           on_remove = coll$remove,
                           on_duplicate = function(id) items$reveal(coll$duplicate(id)),
                           on_undo = coll$undo)

    shiny::observeEvent(input$add, items$reveal(coll$add()))

    output$n <- shiny::renderText(items$count())
    shiny::outputOptions(output, "n", suspendWhenHidden = FALSE)

    list(reset = items$reset)
  })
}
