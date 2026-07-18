# Section: Codelists ----------------------------------------------------------
#
# The codelists a SAP's cohorts cite in [square brackets]: first-class entities
# with a name, a provenance line, and the CODES THEMSELVES, uploaded from the
# files codelist tools produce (see read_codelist in utils.R). The codes live
# in the SAP JSON, so the plan stays self-contained -- a reviewer reads the
# document, an executor reads the file, and neither needs a side-channel.
#
# An upload cannot be "prefilled" back into a fileInput on load, so the codes
# are card STATE: seeded from the saved SAP, replaced wholesale by the next
# upload, and reported alongside the other fields.

codelist_item_ui <- function(id, prefill = NULL) {
  ns <- NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Codelist",
    layout_columns(
      col_widths = c(4, 8),
      textInput(ns("name"), "Codelist name", pf("name"), width = "100%",
                placeholder = "cs_influenza_vaccine"),
      textInput(ns("description"), "Description / provenance", pf("description"),
                width = "100%",
                placeholder = "e.g. CodelistGenerator, ATC J07BB, generated 2026-07-01")
    ),
    fileInput(ns("upload"),
              "Upload codes (.csv with a concept_id column, .txt one code per line, or .json)",
              accept = c(".csv", ".txt", ".json"), width = "100%"),
    uiOutput(ns("codes_status"))
  )
}

# How many codes the card lists before folding; never a silent cap -- the
# remainder is counted out loud.
CODELIST_SHOWN <- 10

codelist_item_server <- function(id, prefill = NULL, on_remove = function() {}) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, on_remove(), ignoreInit = TRUE)

    # Seeded from the saved SAP; an upload replaces the whole list.
    codes       <- reactiveVal(prefill$codes %||% list())
    source_file <- reactiveVal(prefill$source_file %||% NA)

    observeEvent(input$upload, {
      parsed <- tryCatch(read_codelist(input$upload$datapath, input$upload$name),
                         error = function(e) e)
      if (inherits(parsed, "error")) {
        showNotification(paste("Could not read that codelist:", conditionMessage(parsed)),
                         type = "error")
        return()
      }
      codes(parsed)
      source_file(input$upload$name)
      showNotification(sprintf("Loaded %d codes from %s.", length(parsed), input$upload$name),
                       type = "message")
    })

    # What a collapsed card says it is: the codelist's name and its size.
    item_card_label(output, reactive({
      nm <- trimws(input$name %||% "")
      n  <- length(codes())
      paste0(if (nzchar(nm)) nm else "Untitled", " — ", n, " code", if (n == 1) "" else "s")
    }))

    output$codes_status <- renderUI({
      all_codes <- codes()
      n <- length(all_codes)
      if (!n) return(p(class = "text-muted small mb-0", "No codes uploaded yet."))
      shown <- utils::head(all_codes, CODELIST_SHOWN)
      src   <- source_file()
      div(
        class = "small",
        div(class = "text-muted mb-1",
            sprintf("%d code%s%s:", n, if (n == 1) "" else "s",
                    if (!is.null(src) && !is.na(src)) paste0(" from ", src) else "")),
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
      name        = blank_to_na(input$name),
      description = blank_to_na(input$description),
      source_file = blank_to_na(as.character(source_file() %||% ""))[1],
      codes       = codes()
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
