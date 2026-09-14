# shinySAP -- the SAP authoring app.
#
# A Shiny app DIRECTORY inside the shinySAP package (inst/app), not package
# code: shiny::runApp() sources R/ into a shared environment (loadSupport,
# alphabetical and non-recursive) before evaluating this file, and runs with
# the working directory set here. Everything the app needs from the package it
# calls as shinySAP::<fn>(); the package schema is the only definition of a SAP.
#
# Launch with shinySAP::shinySap(), or
# shiny::runApp(system.file("app", package = "shinySAP")).

if (!requireNamespace("shinySAP", quietly = TRUE)) {
  stop("The shinySAP package must be installed to run this app: it holds the SAP schema.")
}

# Where SAPs are saved. shinySap(outputDir =) sets the option before launching;
# without it, a per-user data folder -- never the app directory itself, which
# under an installed package is inside the R library.
OUTPUT_DIR <- getOption("shinySAP.output_dir", tools::R_user_dir("shinySAP", "data"))

# The app's own directory (runApp() sets the working directory to it): where
# the preview template and its reference document live.
APP_DIR <- normalizePath(".")

# Every schema type must have a template, or a loaded analysis would lose its
# parameters. Fails at startup rather than on the first save.
assert_templates_cover_schema()

ui <- bslib::page_navbar(
  title = "shinySAP",
  id = "nav",
  theme = bslib::bs_theme(version = 5, preset = "shiny"),
  window_title = "shinySAP",
  header = shiny::tags$head(shiny::tags$style(shiny::HTML("
    /* item_card(): the header toggles the body. The chevron points down when the
       card is open and right when it is shut; Bootstrap flips aria-expanded. */
    .item-card-toggle .item-card-chevron { transition: transform .15s ease-in-out; }
    .item-card-toggle[aria-expanded='false'] .item-card-chevron { transform: rotate(-90deg); }
    .item-card-toggle:focus { box-shadow: none; }

    /* Load SAP in the navbar: strip bslib::fileInput() down to its button -- no
       filename box, no progress bar -- and dress it as a nav link in the
       theme's primary blue. It shares one right-pinned nav_item with the
       autosave status (bslib::nav_spacer() does the pinning). */
    .navbar-load { margin-bottom: 0; width: auto !important; }
    .navbar-load .form-control, .navbar-load .progress { display: none; }
    .navbar-load .btn-file {
      background: none; border: none;
      font-size: var(--bs-nav-link-font-size, 1rem);
      font-weight: 700;
      color: var(--bs-primary, #0d6efd);
      padding: var(--bs-nav-link-padding-y, .5rem) var(--bs-nav-link-padding-x, .5rem);
    }
    .navbar-load .btn-file:hover,
    .navbar-load .btn-file:focus { color: var(--bs-link-hover-color, #0a58ca); }

    /* The navbar Save link: dressed like Load SAP beside it, in the theme's
       success green. Bootstrap pins `.navbar-text a` to the navbar's active
       colour, so these selectors must OUT-RANK that rule. */
    .navbar-text a.navbar-save {
      font-size: var(--bs-nav-link-font-size, 1rem);
      font-weight: 700;
      color: var(--bs-success, #198754);
      text-decoration: none;
    }
    .navbar-text a.navbar-save:hover,
    .navbar-text a.navbar-save:focus { color: var(--bs-success-text-emphasis, #146c43); }
  "))),
  bslib::nav_panel("Study", shiny::div(class = "container-fluid py-3", study_ui("study"))),
  bslib::nav_panel("Data sources", shiny::div(class = "container-fluid py-3", data_sources_ui("sources"))),
  bslib::nav_panel("Modifications", shiny::div(class = "container-fluid py-3",
                                               data_source_modifications_ui("mods"))),
  bslib::nav_panel("Codelists", shiny::div(class = "container-fluid py-3", codelists_ui("codelists"))),
  bslib::nav_panel("Cohorts", shiny::div(class = "container-fluid py-3", cohorts_ui("cohorts"))),
  bslib::nav_panel("Analyses", shiny::div(class = "container-fluid py-3", analyses_ui("analyses"))),
  bslib::nav_panel("Review", shiny::div(class = "container-fluid py-3", review_ui("review"))),
  bslib::nav_spacer(),
  # One group at the right edge: Save beside Load, both acting on the whole SAP.
  # Save is a LINK: it reads "Save" until the working file exists, then
  # "Saved HH:MM", and clicking it either way writes THE working file -- one
  # file per SAP, rewritten in place, never a new copy.
  bslib::nav_item(
    shiny::div(
      class = "d-flex align-items-center gap-3",
      shiny::div(class = "navbar-text py-0", shiny::uiOutput("save_status", inline = TRUE)),
      shiny::div(class = "vr my-2"),
      htmltools::tagAppendAttributes(
        shiny::fileInput("load", NULL, accept = ".json",
                         buttonLabel = shiny::tagList(shiny::icon("upload"), "Load SAP"),
                         placeholder = ""),
        class = "navbar-load"
      )
    )
  )
)

server <- function(input, output, session) {
  study     <- study_server("study")
  sources   <- data_sources_server("sources")
  mods      <- data_source_modifications_server("mods", source_choices = sources$choices)
  codelists <- codelists_server("codelists")
  cohorts   <- cohorts_server("cohorts", source_choices = sources$choices,
                              codelist_choices = codelists$choices)
  analyses  <- analyses_server("analyses", cohort_choices = cohorts$choices,
                               cohort_index = cohorts$by_id, source_choices = sources$choices)

  # The single source of truth: the package's own `sap` object, built from what
  # the sections report. Unvalidated on purpose -- a SAP is written
  # incrementally -- and checkSap() reports what is still missing below.
  sap <- shiny::reactive(shinySAP::newSap(list(
    study                     = study$data(),
    data_sources              = sources$data(),
    data_source_modifications = mods$data(),
    codelists                 = codelists$data(),
    cohorts                   = cohorts$data(),
    analyses                  = analyses$data()
  ), validate = FALSE))

  # Structural problems from the schema, then the few semantic checks the schema
  # cannot express (problems.R). Warn-not-block: shown on Review and counted at
  # save time, never a barrier to saving.
  problems <- shiny::reactive(c(
    format_check_problems(shinySAP::checkSap(sap()), sap()),
    semantic_problems(sap())
  ))

  load_sap <- function(loaded) {
    study$load(loaded$study %||% list())
    sources$load(loaded$data_sources %||% list())
    mods$load(loaded$data_source_modifications %||% list())
    codelists$load(loaded$codelists %||% list())
    cohorts$load(loaded$cohorts %||% list())
    analyses$load(loaded$analyses %||% list())
    bslib::nav_select("nav", selected = "Study", session = session)
  }

  shiny::observeEvent(input$load, {
    loaded <- tryCatch(shinySAP::readSap(input$load$datapath), error = function(e) e)
    if (inherits(loaded, "error")) {
      shiny::showNotification(paste("Could not read that file:", conditionMessage(loaded)),
                              type = "error", duration = 10)
      return()
    }
    failed <- tryCatch({
      load_sap(loaded)
      NULL
    }, error = function(e) e)
    if (!is.null(failed)) {
      shiny::showNotification(paste("Could not load that SAP:", conditionMessage(failed)), type = "error")
      return()
    }
    # The loaded file IS the working file from here on: every save and autosave
    # rewrites it. The browser only surfaces the file's NAME, so it is anchored
    # in the current save folder.
    nm <- input$load$name
    if (!is.null(nm) && nzchar(nm)) {
      working_file(file.path(save_dir(), nm))
      sticky_name(TRUE)   # the user picked this name; it never auto-renames
    }
    shiny::showNotification("SAP loaded.", type = "message")
  })

  # -- The working file -------------------------------------------------------
  # A SAP lives in ONE file. It comes into existence at the FIRST of: loading a
  # SAP, the first clicked Save (the dialog asks where, once), or the first
  # autosave (the default folder). From then on EVERY write rewrites that same
  # file. Autosave is debounced: sap() invalidates on every keystroke.
  working_file <- shiny::reactiveVal(NULL)
  save_dir     <- shiny::reactiveVal(OUTPUT_DIR)
  saved_at     <- shiny::reactiveVal(NULL)
  sticky_name  <- shiny::reactiveVal(FALSE)

  # Where the next write lands. An app-derived name follows the study id and
  # version, so typing the id after the first autosave renames the file rather
  # than stranding it under a title-derived name.
  next_path <- function(s) {
    path <- working_file()
    if (is.null(path)) return(working_sap_path(s$study, save_dir()))
    if (sticky_name()) return(path)
    working_sap_path(s$study, dirname(path))
  }

  # After a successful write: adopt the path, and remove the file it superseded
  # (only ever an app-derived earlier name), so the SAP keeps living in one file.
  adopt <- function(path) {
    old <- working_file()
    if (!is.null(old) && !identical(old, path) && file.exists(old)) unlink(old)
    working_file(path)
    save_dir(dirname(path))
    saved_at(Sys.time())
  }

  persist <- function(s) {
    path <- tryCatch(shinySAP::writeSap(s, next_path(s)), error = function(e) NULL)
    if (is.null(path)) return(invisible(NULL))
    adopt(path)
    invisible(path)
  }

  sap_settled <- shiny::debounce(sap, 2000)
  shiny::observeEvent(sap_settled(), {
    s <- sap_settled()
    if (sap_is_empty(s)) return()   # the app as it starts: nothing to keep yet
    persist(s)
  })

  output$save_status <- shiny::renderUI({
    at <- saved_at()
    shiny::actionLink(
      "save_now",
      class = "navbar-save",
      title = if (is.null(at)) "Save the SAP to a file; every later save rewrites it"
              else sprintf("Every save and autosave rewrites %s. Click to save now.", working_file()),
      if (is.null(at)) shiny::tagList(shiny::icon("floppy-disk"), "Save")
      else shiny::tagList(shiny::icon("check"), sprintf("Saved %s", format(at, "%H:%M")))
    )
  })

  # A clicked Save: once the working file exists it simply rewrites it. Only the
  # FIRST save has a question to ask -- where -- and a browser app cannot open
  # the OS's own save dialog for a server-side write, so the folder is a text field.
  shiny::observeEvent(input$save_now, {
    s <- sap()
    if (!is.null(working_file())) {
      path <- save_working(s, next_path(s), length(problems()))
      if (!is.null(path)) adopt(path)
      return()
    }
    if (is.null(s$study$study_id) || is.null(s$study$title)) {
      shiny::showNotification("Give the study an id and a title before saving.", type = "warning")
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Save SAP",
      shiny::textInput("save_dir", "Folder to save into", value = save_dir(), width = "100%"),
      shiny::div(class = "form-text",
                 sprintf("Creates %s in this folder (made if missing); every later save and
                          autosave rewrites that same file. ~ is your home folder; a relative
                          path is inside %s.",
                         basename(working_sap_path(s$study, ".")), OUTPUT_DIR)),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("save_confirm", "Save", class = "btn btn-success",
                            icon = shiny::icon("floppy-disk"))
      ),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$save_confirm, {
    dir <- path.expand(trimws(input$save_dir %||% ""))
    if (!nzchar(dir)) {
      shiny::showNotification("Name a folder to save into.", type = "warning")
      return()
    }
    # The working directory is the app's own folder, so a relative path is
    # resolved against the output folder instead.
    if (!grepl("^(/|[A-Za-z]:)", dir)) dir <- file.path(OUTPUT_DIR, dir)
    s <- sap()
    path <- tryCatch(save_working(s, working_sap_path(s$study, dir), length(problems())),
                     error = function(e) {
                       shiny::showNotification(paste("Could not save there:", conditionMessage(e)),
                                               type = "error")
                       NULL
                     })
    if (is.null(path)) return()
    adopt(path)
    shiny::removeModal()
  })

  review_server("review", sap = sap, problems = problems, app_dir = APP_DIR)
}

shiny::shinyApp(ui, server)
