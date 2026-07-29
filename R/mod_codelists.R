# Section: Codelists ----------------------------------------------------------
#
# The codelists a SAP's cohorts cite in [square brackets]: first-class entities
# with a name, a provenance line, and the concept set itself, uploaded from the
# files codelist tools produce (see read_concept_set in utils.R). Both live in
# the SAP JSON, so the plan stays self-contained -- a reviewer reads the
# document, an executor reads the file, and neither needs a side-channel.
#
# A card holds the concept set EXPRESSION (canonical -- what the study resolves
# against each data partner's vocabulary) and the resolved CODES (a snapshot,
# for review). See the header on read_concept_set() for why both.
#
# An upload cannot be "prefilled" back into a fileInput on load, so both are card
# STATE: seeded from the saved SAP, replaced wholesale by the next upload, and
# reported alongside the other fields.

# The roles DARWIN SAPs group their codelist appendix by (Index events,
# Comorbidities, Medicines, ...). Canonical, but the picker takes free text --
# a study may have a role no list foresees. Unset falls into "Other" in the
# document; it is not a decision the app forces.
CODELIST_CATEGORIES <- c(
  "Index event", "Medicine", "Procedure", "Condition / observation",
  "Comorbidity", "Outcome", "Other"
)

codelist_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Codelist",
    layout_columns(
      col_widths = c(4, 4, 4),
      textInput(ns("name"), "Codelist name", pf("name"), width = "100%",
                placeholder = "cs_influenza_vaccine"),
      selectizeInput(ns("category"), "Category", choices = CODELIST_CATEGORIES,
                     selected = pf("category"), width = "100%",
                     options = list(create = TRUE, placeholder = "e.g. Index event")),
      textInput(ns("description"), "Description / provenance", pf("description"),
                width = "100%",
                placeholder = "e.g. CodelistGenerator, ATC J07BB")
    ),
    fileInput(ns("upload"),
              paste("Upload a concept set (.json Atlas export keeps its",
                    "descendant / excluded / mapped flags; .csv with a concept_id",
                    "column, or .txt one code per line)"),
              accept = c(".csv", ".txt", ".json"), width = "100%"),
    # The vocabulary the snapshot below was resolved against. Typed, not
    # detected: shinySAP has no CDM connection, and a snapshot whose vocabulary
    # is unknown cannot be checked against the one a data partner runs.
    textInput(ns("vocabulary_version"), "Vocabulary version of these codes",
              pf("vocabulary_version"), width = "100%",
              placeholder = "e.g. v5.0 31-AUG-23 (optional)"),
    uiOutput(ns("codes_status"))
  )
}

# How many codes the card lists before folding; never a silent cap -- the
# remainder is counted out loud.
CODELIST_SHOWN <- 10

codelist_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # Seeded from the saved SAP; an upload replaces both wholesale. A file that
    # carries codes but no expression seeds an empty one -- the expression is
    # canonical, and nothing derives it back from the resolved snapshot.
    codes       <- reactiveVal(prefill$codes %||% list())
    expression  <- reactiveVal(prefill$concept_set_expression %||% list())
    source_file <- reactiveVal(prefill$source_file %||% NA)

    observeEvent(input$upload, {
      parsed <- tryCatch(read_concept_set(input$upload$datapath, input$upload$name),
                         error = function(e) e)
      if (inherits(parsed, "error")) {
        showNotification(paste("Could not read that codelist:", conditionMessage(parsed)),
                         type = "error")
        return()
      }
      codes(parsed$codes)
      expression(parsed$expression)
      source_file(input$upload$name)
      showNotification(
        sprintf("Loaded %d concept%s from %s.%s",
                length(parsed$expression), if (length(parsed$expression) == 1) "" else "s",
                input$upload$name,
                if (concept_set_expands(parsed$expression)) {
                  " Descendants are included, so the full codelist resolves at run time."
                } else ""),
        type = "message")
    })

    # What a collapsed card says it is: the codelist's name and its size. The
    # count is the EXPRESSION's, since that is what the author chose; a "+"
    # marks one that expands to more than it names.
    item_card_label(output, reactive({
      nm   <- trimws(input$name %||% "")
      n    <- length(expression())
      more <- if (concept_set_expands(expression())) "+" else ""
      paste0(if (nzchar(nm)) nm else "Untitled", " — ", n, more,
             " concept", if (n == 1 && !nzchar(more)) "" else "s")
    }))

    output$codes_status <- renderUI({
      all_codes <- codes()
      expr      <- expression()
      n <- length(all_codes)
      if (!n && !length(expr)) {
        return(p(class = "text-muted small mb-0", "No concept set uploaded yet."))
      }
      shown <- utils::head(all_codes, CODELIST_SHOWN)
      src   <- source_file()
      div(
        class = "small",
        div(class = "text-muted mb-1",
            sprintf("%d concept%s%s:", length(expr), if (length(expr) == 1) "" else "s",
                    if (!is.null(src) && !is.na(src)) paste0(" from ", src) else "")),
        # An expanding expression is stated on the card, not just in the
        # document: the author has to know the list they are looking at is a
        # seed before they sign anything that says how many concepts it has.
        if (concept_set_expands(expr)) {
          div(class = "text-muted fst-italic mb-1",
              "Includes descendants or mapped concepts, so these are the seed",
              "concepts -- the full codelist resolves against each data source's",
              "vocabulary when the study runs.")
        },
        tags$ul(class = "mb-0 ps-3 font-monospace",
                lapply(shown, function(cd) {
                  tags$li(paste(c(cd$code, cd$name), collapse = " — "))
                })),
        if (n > length(shown)) {
          div(class = "text-muted fst-italic mt-1",
              sprintf("… and %d more, all carried in the SAP.", n - length(shown)))
        }
      )
    })
    outputOptions(output, "codes_status", suspendWhenHidden = FALSE)

    reactive(list(
      name                   = blank_to_na(input$name),
      category               = blank_to_na(input$category),
      description            = blank_to_na(input$description),
      source_file            = blank_to_na(as.character(source_file() %||% ""))[1],
      vocabulary_version     = blank_to_na(input$vocabulary_version),
      concept_set_expression = expression(),
      codes                  = codes()
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
          "The concept sets the cohorts cite in [square brackets], codes included.")
      ),
      div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        actionButton(ns("add"), "Add codelist", class = "btn btn-primary", icon = icon("plus"))
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No codelists uploaded yet.")
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
