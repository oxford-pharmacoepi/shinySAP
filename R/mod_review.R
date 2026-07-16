# Review, save and preview ----------------------------------------------------
# (Loading a saved SAP lives in the navbar, in app.R: it acts on every section.)

review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Review & Save"),
    p(class = "text-muted", "Review, export, and save the statistical analysis plan."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("CDM sources",       textOutput(ns("n_sources")),  theme = "primary"),
      value_box("CDM changes",       textOutput(ns("n_cdm")),      theme = "primary"),
      value_box("Cohorts",           textOutput(ns("n_cohorts")),  theme = "primary"),
      value_box("Proposed analyses", textOutput(ns("n_analyses")), theme = "primary")
    ),
    div(
      class = "d-flex gap-2 my-3 align-items-center flex-wrap",
      actionButton(ns("save"),       "Save to output/", class = "btn btn-success",          icon = icon("floppy-disk")),
      downloadButton(ns("download"), "Download JSON",   class = "btn btn-outline-secondary"),
      downloadButton(ns("dl_word"),  "Download Word",   class = "btn btn-outline-secondary"),
      actionButton(ns("refresh"),    "Refresh preview", class = "btn btn-primary",           icon = icon("rotate"))
    ),
    uiOutput(ns("problems")),
    uiOutput(ns("status")),
    navset_tab(
      nav_panel(
        "Document preview",
        div(class = "pt-3", uiOutput(ns("preview_area")))
      ),
      nav_panel(
        "JSON",
        div(
          class = "pt-3",
          tagAppendAttributes(
            verbatimTextOutput(ns("json")),
            class = "border rounded p-3 bg-body-tertiary",
            style = "max-height: 60vh; overflow: auto;"
          )
        )
      )
    )
  )
}

review_server <- function(id, sap, output_dir,
                          problems = reactive(list())) {
  moduleServer(id, function(input, output, session) {

    # -- Save -----------------------------------------------------------------

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
      filename = function() sprintf("%s.json", sap_file_base(sap()$study)),
      content  = function(file) writeLines(sap_json(sap()), file)
    )

    # -- Preview --------------------------------------------------------------

    template <- normalizePath("inst/sap_preview.Rmd", mustWork = FALSE)

    render_html <- function(sap_val) {
      out <- tempfile(fileext = ".html")
      rmarkdown::render(
        input         = template,
        output_format = rmarkdown::html_document(
          theme          = "flatly",
          toc            = TRUE,
          toc_float      = TRUE,
          self_contained = TRUE
        ),
        output_file = out,
        params      = list(sap = sap_val),
        envir       = new.env(parent = globalenv()),
        quiet       = TRUE
      )
      out
    }

    render_word <- function(sap_val) {
      out <- tempfile(fileext = ".docx")
      rmarkdown::render(
        input         = template,
        output_format = officedown::rdocx_document(
          toc = FALSE,
          number_sections = FALSE,
          # Justified body text lives in the styles here; regenerate with
          # inst/make_reference_docx.R.
          reference_docx = normalizePath("inst/reference.docx")
        ),
        output_file   = out,
        params        = list(sap = sap_val),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
      out
    }

    html_path <- reactiveVal(NULL)

    observeEvent(input$refresh, {
      sap_val <- sap()
      withProgress(message = "Rendering preview…", value = 0.5, {
        path <- tryCatch(render_html(sap_val), error = function(e) {
          showNotification(paste("Render failed:", conditionMessage(e)), type = "error")
          NULL
        })
      })
      html_path(path)
    })

    output$preview_area <- renderUI({
      path <- html_path()
      if (is.null(path)) {
        div(class = "text-muted p-4 border rounded",
            "Click \"Refresh preview\" to render the document.")
      } else {
        addResourcePath("sap_preview", dirname(path))
        url <- paste0("sap_preview/", basename(path))
        tags$iframe(
          src   = url,
          style = "width: 100%; height: 80vh; border: 1px solid #dee2e6; border-radius: 4px;"
        )
      }
    })

    output$dl_word <- downloadHandler(
      filename = function() sprintf("%s.docx", sap_file_base(sap()$study)),
      content  = function(file) {
        withProgress(message = "Rendering Word document…", value = 0.5,
                     file.copy(render_word(sap()), file))
      }
    )
  })
}
