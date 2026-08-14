# Review, save and preview ----------------------------------------------------
# (Loading a saved SAP lives in the navbar, in app.R: it acts on every section.)

review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Review"),
    shiny::p(class = "text-muted",
      "Review and export the statistical analysis plan. Saving lives in the navbar,
       next to Load: one file per SAP, created on the first save and rewritten by
       every save and autosave after it."),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box("CDM sources",       shiny::textOutput(ns("n_sources")),  theme = "primary"),
      bslib::value_box("CDM changes",       shiny::textOutput(ns("n_cdm")),      theme = "primary"),
      bslib::value_box("Cohorts",           shiny::textOutput(ns("n_cohorts")),  theme = "primary"),
      bslib::value_box("Analyses",          shiny::textOutput(ns("n_analyses")), theme = "primary")
    ),
    shiny::div(
      class = "d-flex gap-2 my-3 align-items-center flex-wrap",
      shiny::downloadButton(ns("download"), "Download JSON",   class = "btn btn-outline-secondary"),
      shiny::downloadButton(ns("dl_word"),  "Download Word",   class = "btn btn-outline-secondary"),
      shiny::actionButton(ns("refresh"),    "Refresh preview", class = "btn btn-primary", icon = shiny::icon("rotate"))
    ),
    shiny::uiOutput(ns("problems")),
    bslib::navset_tab(
      bslib::nav_panel(
        "Document preview",
        shiny::div(class = "pt-3", shiny::uiOutput(ns("preview_area")))
      ),
      bslib::nav_panel(
        "JSON",
        shiny::div(
          class = "pt-3",
          shiny::tagAppendAttributes(
            shiny::verbatimTextOutput(ns("json")),
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
review_server <- function(id, sap, problems = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {

    output$problems <- shiny::renderUI({
      found <- problems()
      if (!length(found)) return(NULL)
      shiny::div(
        class = "alert alert-warning",
        shiny::tags$strong(sprintf("%d item(s) need attention before this SAP is complete:",
                            length(found))),
        shiny::tags$ul(class = "mb-0 mt-2", lapply(found, function(p) {
          shiny::tags$li(shiny::tags$strong(p$name), shiny::tags$ul(lapply(p$messages, shiny::tags$li)))
        }))
      )
    })

    output$n_sources  <- shiny::renderText(length(sap()$cdm_sources))
    output$n_cdm      <- shiny::renderText(length(sap()$cdm_changes))
    output$n_cohorts  <- shiny::renderText(length(sap()$cohorts))
    output$n_analyses <- shiny::renderText(length(sap()$proposed_analyses))

    output$json <- shiny::renderText(as.character(sap_json(sap())))

    output$download <- shiny::downloadHandler(
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

    html_path <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$refresh, {
      sap_val <- sap()
      shiny::withProgress(message = "Rendering preview...", value = 0.5, {
        path <- tryCatch(render_html(sap_val), error = function(e) {
          shiny::showNotification(paste("Render failed:", conditionMessage(e)), type = "error")
          NULL
        })
      })
      html_path(path)
    })

    output$preview_area <- shiny::renderUI({
      path <- html_path()
      if (is.null(path)) {
        shiny::div(class = "text-muted p-4 border rounded",
            "Click \"Refresh preview\" to render the document.")
      } else {
        shiny::addResourcePath("sap_preview", dirname(path))
        url <- paste0("sap_preview/", basename(path))
        shiny::tags$iframe(
          src   = url,
          style = "width: 100%; height: 80vh; border: 1px solid #dee2e6; border-radius: 4px;"
        )
      }
    })

    output$dl_word <- shiny::downloadHandler(
      filename = function() sprintf("%s.docx", sap_file_base(sap()$study)),
      content  = function(file) {
        shiny::withProgress(message = "Rendering Word document...", value = 0.5,
                     file.copy(render_word(sap()), file))
      }
    )
  })
}
