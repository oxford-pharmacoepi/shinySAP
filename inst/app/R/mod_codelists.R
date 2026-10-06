# Section: Codelists ----------------------------------------------------------
#
# The schema's codelist is {id, name, type, content}: the content is a real
# omopgenerics object -- a codelist, a codelist with details or a concept set
# expression -- and it travels INSIDE the SAP. The card takes the object from a
# file (the csv/json layouts omopgenerics itself exports) and writes a draft
# shinySAP::newSapCodelist() back through the section's CRUD.

# The class the schema wants for each codelist type.
codelist_content_class <- function(type) {
  switch(as.character(type %||% ""),
         codelist = "codelist",
         codelist_with_details = "codelist_with_details",
         concept_set_expression = "concept_set_expression",
         NULL)
}

# Read an uploaded file into the content object for `type`.
import_codelist_content <- function(type, path, file_type) {
  switch(type,
         codelist = omopgenerics::importCodelist(path, type = file_type),
         codelist_with_details = omopgenerics::importCodelistWithDetails(path, type = file_type),
         concept_set_expression = omopgenerics::importConceptSetExpression(path, type = file_type),
         stop("Choose a codelist type before uploading."))
}

codelist_item_ui <- function(id, prefill = NULL) {
  ns <- shiny::NS(id)
  pf <- prefiller(prefill)
  item_card(
    id, "Codelist",
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::textInput(ns("name"), "Codelist name", pf("name"), width = "100%",
                       placeholder = "influenza_vaccine"),
      shiny::selectInput(ns("type"), "Type",
                         c("Choose a type..." = "", shinySAP::sapSchemaTypes("codelist")),
                         selected = pf("type"), width = "100%")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::div(
        shiny::fileInput(ns("upload"), "Codes", accept = c(".csv", ".json"), width = "100%",
                         buttonLabel = "Upload", placeholder = "csv or json"),
        shiny::div(class = "form-text",
                   paste("A file as written by omopgenerics::exportCodelist(),",
                         "exportCodelistWithDetails() or exportConceptSetExpression()."))
      ),
      shiny::div(
        shiny::tags$label(class = "form-label", "Content"),
        shiny::div(class = "form-control-plaintext small", shiny::textOutput(ns("summary")))
      )
    )
  )
}

codelist_item_server <- function(id, sap_id, prefill = NULL, on_remove = function() {},
                                 write = function(component) {}) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE)
    read <- card_reader(input, prefill)
    content <- shiny::reactiveVal(prefill$content)

    item_card_label(output, shiny::reactive({
      nm <- trimws(input$name %||% "")
      paste0(if (nzchar(nm)) nm else "Untitled", " -- ", sap_id)
    }))

    shiny::observeEvent(input$upload, {
      type <- chr_or_null(input$type)
      if (is.null(type)) {
        shiny::showNotification("Choose the codelist type before uploading.", type = "warning")
        return()
      }
      ext <- tolower(tools::file_ext(input$upload$name))
      # Shiny stores an upload as 0.<ext>, and omopgenerics names each concept set
      # after its file -- so the file gets its original name back first.
      staged <- file.path(tempfile("codelist_"), input$upload$name)
      dir.create(dirname(staged), recursive = TRUE, showWarnings = FALSE)
      file.copy(input$upload$datapath, staged, overwrite = TRUE)
      obj <- tryCatch(import_codelist_content(type, staged, ext), error = function(e) e)
      if (inherits(obj, "error")) {
        shiny::showNotification(paste("Could not read that file:", conditionMessage(obj)),
                                type = "error", duration = 10)
        return()
      }
      content(obj)
    })

    # Content parsed under one type does not fit another; switching drops it
    # rather than keeping an object the schema will reject.
    shiny::observeEvent(input$type, {
      cls <- codelist_content_class(input$type)
      cur <- content()
      if (!is.null(cur) && !is.null(cls) && !inherits(cur, cls)) {
        content(NULL)
        shiny::showNotification("Codelist type changed; upload the codes again in that format.",
                                type = "message")
      }
    }, ignoreInit = TRUE)

    output$summary <- shiny::renderText(codelist_content_summary(content()))
    shiny::outputOptions(output, "summary", suspendWhenHidden = FALSE)

    shiny::observe(write(shinySAP::newSapCodelist(
      sap_id,
      name     = chr_or_null(read("name")),
      type     = chr_or_null(read("type")),
      content  = content(),
      validate = FALSE
    )))
  })
}

codelists_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::h3("Codelists", class = "mb-1"),
        shiny::p(class = "text-muted mb-0", "The concept sets the cohorts are built from.")
      ),
      shiny::div(
        class = "d-flex gap-2",
        collapse_all_button(paste0("#", ns("items"))),
        shiny::actionButton(ns("add"), "Add codelist", class = "btn btn-primary",
                            icon = shiny::icon("plus"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == 0", ns("n")),
      empty_state("No codelists defined yet.")
    ),
    shiny::div(id = ns("items"))
  )
}

codelists_server <- function(id, sap) {
  shiny::moduleServer(id, function(input, output, session) {
    coll <- sap_collection(
      sap, "codelists",
      new_component = function(id) shinySAP::newSapCodelist(id, validate = FALSE),
      copy = function(source, id) shinySAP::newSapCodelist(
        id, name = copy_name(source$name), type = source$type, content = source$content, validate = FALSE)
    )
    item_server <- function(iid, sap_id, prefill, on_remove) {
      codelist_item_server(iid, sap_id, prefill, on_remove,
                           write = function(component) if (items$is_live(iid, sap_id)) coll$write(component))
    }
    items <- dynamic_items("codelist", "items", codelist_item_ui, item_server,
                           ids = coll$ids, prefill_of = coll$prefill_of, noun = "Codelist",
                           on_remove = coll$remove,
                           on_duplicate = function(id) items$reveal(coll$duplicate(id)),
                           on_undo = coll$undo,
                           on_reset = coll$forget)

    shiny::observeEvent(input$add, items$reveal(coll$add()))

    output$n <- shiny::renderText(items$count())
    shiny::outputOptions(output, "n", suspendWhenHidden = FALSE)

    list(reset = items$reset)
  })
}
