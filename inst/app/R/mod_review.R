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
      col_widths = c(2, 3, 2, 2, 3),
      bslib::value_box("Data sources",  shiny::textOutput(ns("n_sources")),   theme = "primary"),
      bslib::value_box("Modifications", shiny::textOutput(ns("n_mods")),      theme = "primary"),
      bslib::value_box("Codelists",     shiny::textOutput(ns("n_codelists")), theme = "primary"),
      bslib::value_box("Cohorts",       shiny::textOutput(ns("n_cohorts")),   theme = "primary"),
      bslib::value_box("Analyses",      shiny::textOutput(ns("n_analyses")),  theme = "primary")
    ),
    shiny::div(
      class = "d-flex gap-2 my-3 align-items-center flex-wrap",
      shiny::downloadButton(ns("download"), "Download JSON",   class = "btn btn-outline-secondary"),
      shiny::downloadButton(ns("dl_word"),  "Download Word",   class = "btn btn-outline-secondary"),
      shiny::actionButton(ns("refresh"),    "Refresh preview", class = "btn btn-primary",
                          icon = shiny::icon("rotate")),
      shiny::actionButton(ns("gen_code"),   "Generate code",   class = "btn btn-outline-primary",
                          icon = shiny::icon("code")),
      shiny::downloadButton(ns("dl_study"), "Download study",  class = "btn btn-outline-secondary",
                            icon = shiny::icon("download"))
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
      ),
      bslib::nav_panel(
        "Generated code",
        shiny::div(class = "pt-3", shiny::uiOutput(ns("code_area")))
      )
    )
  )
}

# Saving is NOT here: the navbar's Save link (app.R) owns it, via
# save_working(). This tab reviews, downloads and previews.
# `app_dir` is where the preview template and its reference document live (the
# app's own directory; app.R resolves it once at startup).
review_server <- function(id, sap, problems = shiny::reactive(list()), app_dir = normalizePath(".")) {
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

    output$n_sources   <- shiny::renderText(length(sap()$data_sources))
    output$n_mods      <- shiny::renderText(length(sap()$data_source_modifications))
    output$n_codelists <- shiny::renderText(length(sap()$codelists))
    output$n_cohorts   <- shiny::renderText(length(sap()$cohorts))
    output$n_analyses  <- shiny::renderText(length(sap()$analyses))

    output$json <- shiny::renderText(as.character(shinySAP::sapToJson(sap())))

    output$download <- shiny::downloadHandler(
      filename = function() sprintf("%s.json", sap_file_base(sap()$study)),
      content  = function(file) shinySAP::writeSap(sap(), file)
    )

    # -- Preview --------------------------------------------------------------

    template <- file.path(app_dir, "sap_preview.Rmd")

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
          # data-raw/make_reference_docx.R.
          reference_docx = file.path(app_dir, "reference.docx")
        ),
        output_file   = out,
        params        = list(sap = sap_val),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
      out
    }

    html_path  <- shiny::reactiveVal(NULL)
    study_dir  <- tempfile("shinysap_study_")  # session-scoped; one per module instance
    study_files <- shiny::reactiveVal(NULL)    # named list: relative path -> content string

    shiny::observeEvent(input$gen_code, {
      tryCatch({
        unlink(study_dir, recursive = TRUE)
        shinySAP::writeSapStudy(sap(), study_dir)
        paths <- list(
          "codelist/codelistCreation.R" = file.path(study_dir, "codelist", "codelistCreation.R"),
          "cohorts/instantiateCohorts.R" = file.path(study_dir, "cohorts", "instantiateCohorts.R"),
          "analyses/estimates.R"         = file.path(study_dir, "analyses", "estimates.R")
        )
        contents <- lapply(paths, function(p) {
          if (file.exists(p)) paste(readLines(p, warn = FALSE), collapse = "\n") else ""
        })
        study_files(contents)
      }, error = function(e) {
        shiny::showNotification(paste("Code generation failed:", conditionMessage(e)), type = "error")
      })
    })

    output$code_area <- shiny::renderUI({
      files <- study_files()
      if (is.null(files)) {
        return(shiny::div(class = "text-muted p-4 border rounded",
            "Click \"Generate code\" to produce the analysis script."))
      }
      code_panel <- function(label, content) {
        bslib::nav_panel(label,
          shiny::tags$pre(
            class = "border rounded p-3 bg-body-tertiary mt-3",
            style = "max-height: 65vh; overflow: auto; white-space: pre; font-size: 0.82em;",
            if (nzchar(content)) content else "# (nothing generated for this section)"
          )
        )
      }
      bslib::navset_tab(
        code_panel("codelist/codelistCreation.R",    files[["codelist/codelistCreation.R"]]),
        code_panel("cohorts/instantiateCohorts.R",   files[["cohorts/instantiateCohorts.R"]]),
        code_panel("analyses/estimates.R",           files[["analyses/estimates.R"]])
      )
    })

    output$dl_study <- shiny::downloadHandler(
      filename = function() sprintf("%s-study.zip", sap_file_base(sap()$study)),
      content  = function(file) {
        if (is.null(study_files())) {
          shiny::showNotification("Click \"Generate code\" first.", type = "warning")
          return(invisible(NULL))
        }
        zip::zip(zipfile = file,
                 files   = list.files(study_dir, recursive = TRUE),
                 root    = study_dir)
      }
    )

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
