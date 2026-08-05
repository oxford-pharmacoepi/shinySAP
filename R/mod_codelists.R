# Section: Codelists ----------------------------------------------------------
#
# The codelists a SAP's cohorts cite in [square brackets]: named entities with a
# provenance line and an optional category. The SAP names a codelist and says
# where it comes from; the codes themselves live with the study (the CSVs a
# codelist tool produces), not in the plan.

# The three roles a codelist plays in an incidence-prevalence study: it
# defines what indexes a cohort, what counts as an outcome, or a variable
# measured alongside. Roles, not OMOP domains -- what a codelist is FOR is what
# a reviewer wants grouped; the domain its concepts live in is an executor's
# fact the vocabulary already knows. The picker still takes free text -- a
# study may have a role this list foresaw away -- and unset falls into "Other"
# in the document; it is not a decision the app forces.
CODELIST_CATEGORIES <- c("Index event", "Outcome", "Covariate")

codelist_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Codelist",
    layout_columns(
      col_widths = c(6, 6),
      textInput(ns("name"), "Codelist name", pf("name"), width = "100%",
                placeholder = "cs_influenza_vaccine"),
      selectizeInput(ns("category"), "Category", choices = CODELIST_CATEGORIES,
                     selected = pf("category"), width = "100%",
                     options = list(create = TRUE, placeholder = "e.g. Index event"))
    ),
    # Full-width on its own row: a provenance line is a sentence, not a word,
    # and a third-of-the-card input trimmed it out of view.
    textInput(ns("description"), "Description", pf("description"),
              width = "100%",
              placeholder = "e.g. CodelistGenerator, ATC J07BB")
  )
}

codelist_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # What a collapsed card says it is: the codelist's name.
    item_card_label(output, reactive({
      nm <- trimws(input$name %||% "")
      if (nzchar(nm)) nm else "Untitled"
    }))

    reactive(list(
      name        = blank_to_na(input$name),
      category    = blank_to_na(input$category),
      description = blank_to_na(input$description)
    ))
  })
}

codelists_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h3("Codelists", class = "mb-1"),
        p(class = "text-muted mb-0",
          "The concept sets the cohorts cite in [square brackets].")
      ),
      div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        actionButton(ns("add"), "Add codelist", class = "btn btn-primary", icon = icon("plus"))
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No codelists defined yet.")
    ),
    div(id = ns("items"))
  )
}

codelists_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    items <- dynamic_items("codelist", "items", codelist_item_ui, codelist_item_server,
                           noun = "Codelist")

    observeEvent(input$add, items$add(reveal = TRUE))

    output$n <- renderText(items$count())
    outputOptions(output, "n", suspendWhenHidden = FALSE)

    load <- function(codelists) {
      items$clear()
      for (cl in codelists) items$add(cl)
    }

    # For pickers that may one day reference codelists by name, mirroring
    # cdm_sources_server.
    names_r <- reactive({
      nms <- vapply(items$data(), function(x) trimws(as.character(x$name %||% "")),
                    character(1))
      sort(unique(nms[nzchar(nms) & !is.na(nms)]))
    })

    list(data = items$data, load = load, names = names_r)
  })
}
