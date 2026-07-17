# Review, save and preview ----------------------------------------------------
# (Loading a saved SAP lives in the navbar, in app.R: it acts on every section.)

review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Review"),
    p(class = "text-muted",
      "Review and export the statistical analysis plan. Saving lives in the navbar,
       next to Load: one file per SAP, created on the first save and rewritten by
       every save and autosave after it."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("CDM sources",       textOutput(ns("n_sources")),  theme = "primary"),
      value_box("CDM changes",       textOutput(ns("n_cdm")),      theme = "primary"),
      value_box("Cohorts",           textOutput(ns("n_cohorts")),  theme = "primary"),
      value_box("Analyses",          textOutput(ns("n_analyses")), theme = "primary")
    ),
    div(
      class = "d-flex gap-2 my-3 align-items-center flex-wrap",
      downloadButton(ns("download"), "Download JSON",   class = "btn btn-outline-secondary"),
      downloadButton(ns("dl_word"),  "Download Word",   class = "btn btn-outline-secondary"),
      actionButton(ns("refresh"),    "Refresh preview", class = "btn btn-primary", icon = icon("rotate"))
    ),
    uiOutput(ns("problems")),
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

# Saving is NOT here: the navbar's Save link (app.R) owns it, via
# save_working(). This tab reviews, downloads and previews.
review_server <- function(id, sap, problems = reactive(list())) {
  moduleServer(id, function(input, output, session) {

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
