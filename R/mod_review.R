# Review, save and reload ----------------------------------------------------

review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Review & save"),
    p(class = "text-muted", "JSON SAP for review."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("CDM sources", textOutput(ns("n_sources")), theme = "primary"),
      value_box("CDM changes", textOutput(ns("n_cdm")), theme = "primary"),
      value_box("Cohorts", textOutput(ns("n_cohorts")), theme = "primary"),
      value_box("Proposed analyses", textOutput(ns("n_analyses")), theme = "primary")
    ),
    div(
      class = "d-flex gap-2 my-3 align-items-start flex-wrap",
      actionButton(ns("save"), "Save to output/", class = "btn btn-success", icon = icon("floppy-disk")),
      downloadButton(ns("download"), "Download JSON", class = "btn btn-outline-secondary"),
      div(
        class = "ms-auto",
        fileInput(ns("load"), NULL, accept = ".json", buttonLabel = "Load a SAP...",
                  placeholder = "No file selected", width = "320px")
      )
    ),
    uiOutput(ns("problems")),
    uiOutput(ns("status")),
    tagAppendAttributes(
      verbatimTextOutput(ns("json")),
      class = "border rounded p-3 bg-body-tertiary",
      style = "max-height: 60vh; overflow: auto;"
    )
  )
}

review_server <- function(id, sap, output_dir, on_load,
                          problems = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    saved_path <- reactiveVal(NULL)

    output$problems <- renderUI({
      found <- problems()
      if (!length(found)) return(NULL)
      div(
        class = "alert alert-warning",
        tags$strong(sprintf("%d item(s) need attention before this SAP is complete:",
                            length(found))),
        tags$ul(class = "mb-0 mt-2", lapply(found, function(p) {
          tags$li(tags$strong(p$name), tags$ul(lapply(p$messages, tags$li)))
        }))
      )
    })

    output$n_sources  <- renderText(length(sap()$cdm_sources))
    output$n_cdm      <- renderText(length(sap()$cdm_changes))
    output$n_cohorts  <- renderText(length(sap()$cohorts))
    output$n_analyses <- renderText(length(sap()$proposed_analyses))

    output$json <- renderText(as.character(sap_json(sap())))

    observeEvent(input$save, {
      if (is.na(sap()$study$title)) {
        showNotification("Give the study a title before saving.", type = "warning")
        return()
      }
      path <- save_sap(sap(), output_dir)
      saved_path(path)
      # A SAP is drafted over many sittings, so an incomplete one must still be
      # saveable -- say what is outstanding rather than refusing the checkpoint.
      n <- length(problems())
      if (n > 0) {
        showNotification(
          sprintf("Saved %s, but %d item(s) still need attention.", basename(path), n),
          type = "warning", duration = 8
        )
      } else {
        showNotification(paste("Saved", basename(path)), type = "message")
      }
    })

    output$status <- renderUI({
      req(saved_path())
      div(class = "alert alert-success py-2", "Saved to ", tags$code(saved_path()))
    })

    output$download <- downloadHandler(
      filename = function() sprintf("sap-%s.json", slugify(sap()$study$title)),
      content = function(file) writeLines(sap_json(sap()), file)
    )

    observeEvent(input$load, {
      loaded <- tryCatch(read_sap(input$load$datapath), error = function(e) e)
      if (inherits(loaded, "error")) {
        showNotification(paste("Could not read that file:", conditionMessage(loaded)), type = "error")
        return()
      }
      # Each section clears itself before repopulating, so an error partway
      # through would leave the form half-wiped with nothing said about it.
      failed <- tryCatch({
        on_load(loaded)
        NULL
      }, error = function(e) e)
      if (!is.null(failed)) {
        showNotification(paste("Could not load that SAP:", conditionMessage(failed)), type = "error")
        return()
      }
      showNotification("SAP loaded.", type = "message")
    })
  })
}
