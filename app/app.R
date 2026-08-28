# shinySAP -- structured capture of a Statistical Analysis Plan as JSON.
#
# A Shiny app DIRECTORY, not a package: shiny::runApp("app") sources R/ into a
# shared environment (loadSupport, alphabetical and non-recursive) before
# evaluating this file, and runs with the working directory set here -- which is
# what makes the relative paths below and in mod_review.R resolve.
#
# Schema history lives in NEWS.md.

if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")))
  Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

SAP_SCHEMA_VERSION <- "0.4.30"

# Overridable so tests or a deployment can write somewhere else.
OUTPUT_DIR <- getOption("shinySAP.output_dir", "output")

ui <- bslib::page_navbar(
  title = "shinySAP",
  id = "nav",
  theme = bslib::bs_theme(version = 5, preset = "shiny"),
  window_title = "shinySAP",
  header = shiny::tags$head(shiny::tags$style(shiny::HTML("
    /* The objectives picker carries whole sentences, so its options and its
       selected chips wrap rather than overflowing on one line. Scoped: every
       other selectize on the page holds short names and is better left alone. */
    .objectives-picker .selectize-dropdown .option,
    .objectives-picker .selectize-input .item {
      white-space: normal;
      line-height: 1.35;
    }
    .objectives-picker .selectize-input { height: auto; }

    /* item_card(): the header toggles the body. The chevron points down when the
       card is open and right when it is shut; Bootstrap flips aria-expanded. */
    .item-card-toggle .item-card-chevron { transition: transform .15s ease-in-out; }
    .item-card-toggle[aria-expanded='false'] .item-card-chevron { transform: rotate(-90deg); }
    .item-card-toggle:focus { box-shadow: none; }

    /* Load SAP in the navbar: strip bslib::fileInput() down to its button -- no
       filename box, no progress bar -- and dress it as a nav link in the
       theme's primary blue. It shares one right-pinned nav_item with the
       autosave status (bslib::nav_spacer() does the pinning; a second auto margin
       here would split the free space and strand the status mid-navbar). */
    /* width: shiny-input-container defaults to 300px; shrink to the button. */
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

    /* The navbar Save link: dressed exactly like Load SAP beside it, but in
       the theme's success green -- saving is the \"go\" action of the pair.
       Bootstrap pins `.navbar-text a` (and its :hover/:focus) to the navbar's
       active colour, which is what kept this link black -- so these selectors
       must OUT-RANK that rule, not merely exist. Hover darkens, exactly as
       Load's blue does. */
    .navbar-text a.navbar-save {
      font-size: var(--bs-nav-link-font-size, 1rem);
      font-weight: 700;
      color: var(--bs-success, #198754);
      text-decoration: none;
    }
    .navbar-text a.navbar-save:hover,
    .navbar-text a.navbar-save:focus { color: var(--bs-success-text-emphasis, #146c43); }

    /* Codelist references in cohort free text (www/codelist_refs.js). The
       backdrop sits behind a transparent-background textarea and repeats its
       text; only the [tokens] carry a tint -- blue like a selectize chip when
       the codelist exists, amber when nothing defines it. The backdrop's text
       itself is invisible; the textarea's text renders on top of the tint. */
    .clref-wrap { position: relative; }
    /* !important: the theme's own textarea.form-control background rule ties on
       specificity and loads later, so without it the opaque white textarea sits
       on top of the backdrop and no tint ever shows. */
    .clref-wrap textarea.form-control {
      position: relative;
      background-color: transparent !important;
    }
    .clref-backdrop {
      position: absolute; inset: 0;
      overflow: hidden;
      white-space: pre-wrap; overflow-wrap: break-word;
      color: transparent;
      background: var(--bs-body-bg, #fff);
      pointer-events: none;
    }
    .clref-backdrop mark { color: transparent; padding: 0; border-radius: 3px; }
    .clref-backdrop mark.clref-known { background: var(--bs-primary-bg-subtle, #cfe2ff); }
    .clref-backdrop mark.clref-unknown { background: var(--bs-warning-bg-subtle, #fff3cd); }
    .clref-menu {
      position: absolute; left: 0; top: 100%; z-index: 1050;
      min-width: 14rem; max-width: 100%;
      background: var(--bs-body-bg, #fff);
      border: 1px solid var(--bs-border-color, #dee2e6);
      border-radius: var(--bs-border-radius, .375rem);
      box-shadow: var(--bs-box-shadow-sm, 0 .125rem .25rem rgba(0,0,0,.075));
    }
    .clref-item {
      display: block; width: 100%; text-align: left;
      border: none; background: none;
      padding: .25rem .75rem;
      font-family: var(--bs-font-monospace, monospace);
      font-size: .875em;
    }
    .clref-item:hover, .clref-item.active {
      background: var(--bs-primary-bg-subtle, #cfe2ff);
    }
  ")), shiny::tags$script(src = "codelist_refs.js")),
  bslib::nav_panel("Study", shiny::div(class = "container-fluid py-3", study_ui("study"))),
  bslib::nav_panel("CDM Sources", shiny::div(class = "container-fluid py-3", cdm_sources_ui("sources"))),
  bslib::nav_panel("CDM Changes", shiny::div(class = "container-fluid py-3", cdm_changes_ui("cdm"))),
  bslib::nav_panel("Codelists", shiny::div(class = "container-fluid py-3", codelists_ui("codelists"))),
  bslib::nav_panel("Cohorts", shiny::div(class = "container-fluid py-3", cohorts_ui("cohorts"))),
  bslib::nav_panel("Analyses", shiny::div(class = "container-fluid py-3", analyses_ui("analyses"))),
  bslib::nav_panel("Review", shiny::div(class = "container-fluid py-3", review_ui("review"))),
  bslib::nav_spacer(),
  # One group at the right edge: Save beside Load, both acting on the whole
  # SAP rather than one section. Save is a LINK, not just a status: it reads
  # "Save" until the working file exists, then "Saved HH:MM", and clicking it
  # either way writes THE working file -- one file per SAP, rewritten in
  # place, never a new copy.
  bslib::nav_item(
    shiny::div(
      class = "d-flex align-items-center gap-3",
      shiny::div(class = "navbar-text py-0",
          shiny::uiOutput("save_status", inline = TRUE)),
      # A quiet vertical rule: Save and Load are two whole-SAP actions side by
      # side, not one control -- the divider says where one ends.
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
  study    <- study_server("study")
  sources  <- cdm_sources_server("sources")
  cdm      <- cdm_changes_server("cdm", source_names = sources$names)
  codelists <- codelists_server("codelists")
  cohorts  <- cohorts_server("cohorts", source_names = sources$names,
                             codelist_names = codelists$names)
  # by_name, not just names: the templates echo what the denominator cohort
  # already fixes, and validate against it.
  # Objective ids, labelled with their text, for the analysis cards' picker.
  # Numbered as the document numbers them, so the label an author picks from is
  # the "Objective 3" they see in the preview.
  #
  # NOT truncated. A real objective is a full sentence ("To estimate the
  # prevalence of X between 1st January 2010 and the end of available data in
  # data sources from across Europe, stratified by age and sex"), and six of them
  # differ only in the disease named a third of the way in -- so a label cut at
  # 80 characters makes every option identical. The picker wraps instead; see
  # .objectives-picker in the header CSS.
  objective_choices <- shiny::reactive({
    objs <- study$data()$objectives %||% list()
    if (!length(objs)) return(character(0))
    stats::setNames(
      vapply(objs, function(o) objective_id(o) %||% "", character(1)),
      vapply(seq_along(objs), function(i) sprintf(
        "%d. %s", i, objective_text(objs[[i]])), character(1)))
  })

  analyses <- analyses_server("analyses",
                              cohort_names = cohorts$names,
                              cohort_index = cohorts$by_name,
                              source_names = sources$names,
                              # Renaming a cohort walks every analysis picker
                              # still holding the old name (see cohorts_server).
                              cohort_renames = cohorts$renames,
                              objective_choices = objective_choices)

  # The single source of truth for what gets serialised.
  sap <- shiny::reactive(list(
    sap_schema_version = SAP_SCHEMA_VERSION,
    generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    study              = study$data(),
    cdm_sources        = sources$data(),
    cdm_changes        = cdm$data(),
    codelists          = codelists$data(),
    cohorts            = cohorts$data(),
    proposed_analyses  = analyses$data()
  ))

  # A file is read as the current schema, with no migration step: this app is
  # pre-release and there are no SAPs in the wild to carry forward. A file
  # written by an older build loads on a best-effort basis -- keys the current
  # sections do not know are ignored, and ones they expect and do not find come
  # back empty.
  load_sap <- function(loaded) {
    study$load(loaded$study %||% list())
    sources$load(loaded$cdm_sources %||% list())
    cdm$load(loaded$cdm_changes %||% list())
    codelists$load(loaded$codelists %||% list())
    cohorts$load(loaded$cohorts %||% list())
    analyses$load(loaded$proposed_analyses %||% list())
    bslib::nav_select("nav", selected = "Study", session = session)
  }

  shiny::observeEvent(input$load, {
    loaded <- tryCatch(read_sap(input$load$datapath), error = function(e) e)
    if (inherits(loaded, "error")) {
      shiny::showNotification(paste("Could not read that file:", conditionMessage(loaded)), type = "error")
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
    # rewrites it, never a new copy. The browser only surfaces the file's NAME,
    # so it is anchored in the current save folder -- for a file loaded from
    # there (the usual case) that is exactly the file that was picked.
    nm <- input$load$name
    if (!is.null(nm) && nzchar(nm)) {
      working_file(file.path(save_dir(), nm))
      sticky_name(TRUE)   # the user picked this name; it never auto-renames
    }
    shiny::showNotification("SAP loaded.", type = "message")
  })

  # -- The working file -------------------------------------------------------
  # A SAP lives in ONE file. It comes into existence at the FIRST of: loading a
  # SAP (the loaded file itself), the first clicked Save (the dialog asks where,
  # once), or the first autosave (the default folder). From then on EVERY write
  # -- autosave or clicked Save -- rewrites that same file, and nothing else is
  # ever created. Autosave is debounced: sap() invalidates on every keystroke,
  # and two quiet seconds also ride out the one round-trip after a kind/type
  # switch in which a rebuilt block's inputs have not reported back (see data_r
  # in mod_analyses.R).
  working_file <- shiny::reactiveVal(NULL)
  save_dir     <- shiny::reactiveVal(OUTPUT_DIR)
  saved_at     <- shiny::reactiveVal(NULL)
  # TRUE only for a LOADED file: that name the user chose, so it stays. An
  # app-derived name follows the study identity instead (see next_path).
  sticky_name  <- shiny::reactiveVal(FALSE)

  # Where the next write lands. An app-derived name FOLLOWS the study identity
  # -- and sap_file_base() puts the study code before the title -- so typing
  # the code after the first autosave RENAMES the file rather than stranding
  # it under a title-derived name forever.
  next_path <- function(s) {
    path <- working_file()
    if (is.null(path)) return(working_sap_path(s$study, save_dir()))
    if (sticky_name()) return(path)
    working_sap_path(s$study, dirname(path))
  }

  # After a successful write: adopt the path, and remove the file it
  # superseded (only ever an app-derived earlier name -- never a loaded file,
  # whose name is sticky), so the SAP keeps living in exactly one file.
  adopt <- function(path) {
    old <- working_file()
    if (!is.null(old) && !identical(old, path) && file.exists(old)) unlink(old)
    working_file(path)
    save_dir(dirname(path))
    saved_at(Sys.time())
  }

  persist <- function(s) {
    path <- tryCatch(write_sap(s, next_path(s)), error = function(e) NULL)
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

  # The navbar Save link doubles as the indicator: "Save" until the working
  # file exists, then "Saved HH:MM". Re-rendering keeps the same input id, so
  # the click observer below survives every relabel. The full path rides in
  # the tooltip; the navbar only has room for the when. .navbar-save (see the
  # header CSS) dresses it exactly like Load, in green.
  output$save_status <- shiny::renderUI({
    at <- saved_at()
    shiny::actionLink(
      "save_now",
      class = "navbar-save",
      title = if (is.null(at)) "Save the SAP to a file; every later save rewrites it"
              else sprintf("Every save and autosave rewrites %s. Click to save now.",
                           working_file()),
      if (is.null(at)) shiny::tagList(shiny::icon("floppy-disk"), "Save")
      else shiny::tagList(shiny::icon("check"), sprintf("Saved %s", format(at, "%H:%M")))
    )
  })

  # The bracket convention as a contract: every [cs_x] a cohort cites must
  # resolve to a codelist on the Codelists tab, and idle codelists get a nudge.
  #
  # The last two guard the generated code rather than the JSON.
  # table_name_collisions(): two cohort names that differ only in punctuation
  # collapse to one CDM table name, and the script would then create that table
  # twice and quietly estimate everything against the second.
  # uninstantiated_cohort_problems(): an estimator pointing at a table nothing in
  # the script creates -- which validates clean here and dies at the data partner.
  problems <- shiny::reactive(c(study_problems(study$data()),
                         objective_coverage_problems(study$data(), analyses$data()),
                         analysis_role_problems(analyses$data()),
                         cohorts$problems(), analyses$problems(),
                         codelist_reference_problems(cohorts$data(), codelists$names()),
                         table_name_collisions(cohorts$data()),
                         # A bound cohort whose parts are unbuilt, listed after
                         # it, a denominator, or holding colliding cohort names:
                         # each one a bind() that dies at the data partner.
                         bound_cohort_problems(cohorts$data()),
                         uninstantiated_cohort_problems(cohorts$data(), analyses$data()),
                         # Only reachable since data_sources became a guard in
                         # the generated script: an estimate running where its
                         # cohort is never built.
                         cohort_source_coverage_problems(
                           cohorts$data(), analyses$data(),
                           sap_source_keys(list(cdm_sources = sources$data())))))

  # A clicked Save: once the working file exists it simply rewrites it (with
  # the title guard and the problems warning). Only the FIRST save has a
  # question to ask -- where -- and a browser app cannot open the OS's own
  # save dialog for a server-side write, so the folder is a text field.
  shiny::observeEvent(input$save_now, {
    s <- sap()
    if (is.na(s$study$title %||% NA)) {
      shiny::showNotification("Give the study a title before saving.", type = "warning")
      return()
    }
    if (!is.null(working_file())) {
      path <- save_working(s, next_path(s), length(problems()))
      if (!is.null(path)) adopt(path)
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Save SAP",
      shiny::textInput("save_dir", "Folder to save into", value = save_dir(), width = "100%"),
      shiny::div(class = "form-text",
          sprintf("Creates %s in this folder (made if missing); every later save and
                   autosave rewrites that same file. A relative path is inside the app
                   folder; ~ is your home folder.",
                  basename(working_sap_path(s$study, ".")))),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("save_confirm", "Save", class = "btn btn-success",
                     icon = shiny::icon("floppy-disk"))
      ),
      easyClose = TRUE
    ))
  })

  # An unwritable folder keeps the dialog open with an error, so the typed
  # path is not lost.
  shiny::observeEvent(input$save_confirm, {
    dir <- path.expand(trimws(input$save_dir %||% ""))
    if (!nzchar(dir)) {
      shiny::showNotification("Name a folder to save into.", type = "warning")
      return()
    }
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

  review_server("review", sap = sap, problems = problems)
}

shiny::shinyApp(ui, server)
