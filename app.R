# shinySAP -- structured capture of a Statistical Analysis Plan as JSON.
#
# Run with:  shiny::runApp("Documents/shinySAP")
#
# Files in R/ are sourced automatically by Shiny.

if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")))
  Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

library(shiny)
library(bslib)
library(jsonlite)

# 0.2.0 added cdm_sources and renamed analyses -> proposed_analyses.
# 0.3.0 moved the type-specific analysis fields under `parameters`.
# 0.3.1 made the Incidence parameters map 1:1 onto estimateIncidence().
# 0.4.0 added cohort sets (cohorts gained parent_cohort; prevalence gained
#       denominatorCohortId / outcomeCohortId, null = all IDs in the set) and
#       aligned prevalence with the estimators: parameters use the argument
#       names and order, strata replaces stratifications, sensitivity_analyses
#       is dropped there, and a prevalence analysis_type names the estimator
#       (estimatePointPrevalence / estimatePeriodPrevalence).
# 0.4.1 dropped the denominator's declared strata_variables: the generator makes
#       age_group and sex and nothing else, so they are fixed, not authored.
# 0.4.2 named every denominator key after the generator argument it feeds, and
#       folded the two date keys into the one cohortDateRange pair it takes.
# 0.4.3 did the same for Incidence: its parameters are now flat and named exactly
#       for estimateIncidence() (denominatorTable, outcomeTable, censorTable,
#       *CohortId, interval, completeDatabaseIntervals, outcomeWashout,
#       repeatedEvents, strata, includeOverallStrata), dropping the `estimand`
#       wrapper and the snake_case keys.
# 0.4.4 trimmed cdm_sources to name / source_key / country (pickers now refer to
#       sources by source_key), and in cdm_changes dropped cdm_version and
#       replaced the scalar data_source with a data_sources array.
# 0.4.5 retyped cdm_changes around the questions a SAP asks of the CDM (extra
#       validations, database-specific alterations, removing people with no year
#       of birth or sex data). The old table-edit taxonomy described alterations,
#       so legacy types map there and cdm_table/cdm_field fold into the
#       description on load.
# 0.4.6 promoted the common alterations to change types of their own (subset a
#       table, limit observation periods, remap concepts, implausible dates);
#       unrecognised types land on "Other database-specific alteration".
# 0.4.7 dropped a cdm_change's rationale; an old one folds into the description.
# 0.4.8 gave study an amendments array (version, date, description of change).
# 0.4.9 gave study an aim (the research question introducing the numbered
#       objectives; background doubles as rationale).
# 0.4.10 renamed study$acronym to study$study_code: what authors put there is a
#        short study name or code, not an acronym.
# 0.4.11 dropped a cohort's cohort_id: IDs are assigned at generation time, not
#        authored in the SAP. Without them the *CohortId sub-pickers stay hidden
#        and those parameters serialise null = all cohorts in the set.
# 0.4.12 dropped a cohort's description: the kind block already says what a
#        cohort is in structured form (entry events, criteria, generator
#        arguments), and free text beside it only drifted out of step.
# 0.4.13 gave cohorts a data_sources array: the SAP-level counterpart of the
#        generators' `cdm` argument, naming the CDM sources the cohort is built
#        against -- the same shape analyses already used.
# 0.4.14 gave plain cohorts an index_rule: WHICH occurrence of the entry event
#        indexes the cohort ("Index date" in protocol tables) -- previously
#        conflated into the entry-event line.
# 0.4.15 folded index_rule and concept_set back into the text fields: the
#        codelist is cited inline in the entry event that uses it, and the
#        index rule is an inclusion criterion. Old values fold in on load.
# 0.4.16 added codelists: first-class entities (name, provenance, source file,
#        and the codes themselves, uploaded from csv/txt/json) that cohorts
#        cite in [square brackets]. The codes live in the SAP, so the plan is
#        self-contained.
# 0.4.17 gave codelists an optional category (Index event, Comorbidity, ...):
#        the document groups the codelist appendix by it, the way DARWIN SAPs
#        do. Uncategorised codelists fall into "Other".
# 0.4.18 added study$min_cell_count, the omopgenerics::suppress(minCellCount =)
#        threshold every result is suppressed at before export. Study-level, not
#        per-analysis: it governs what may leave the data partner at all. A file
#        without it loads at 5 -- the function's own default and the DARWIN
#        convention -- rather than blank, which would read as no suppression.
# 0.4.19 stopped prefilling study$min_cell_count, and flags it when unset. The
#        threshold is the data partners' governance decision, not a convention
#        the app can confirm on their behalf -- omopgenerics defaults to 5, but
#        DARWIN EU's own notice states 100 for the catalogue, so there is no one
#        number to prefill. A file without the key now loads blank and stays
#        blank; study_problems() reports it on Review so the gap is visible
#        rather than papered over with a plausible guess.
# 0.4.20 made the concept set EXPRESSION canonical for a codelist, in
#        omopgenerics' own field names (concept_id / excluded / descendants /
#        mapped), and demoted `codes` to a resolved snapshot with an optional
#        vocabulary_version stamp. The two answer different questions: an
#        expression is the specification and resolves differently against each
#        data partner's vocabulary, so a study running in five countries that
#        shipped resolved codes would impose the authoring machine's vocabulary
#        on all five. Speaking the standard's field names also means the
#        generated study code can pass the expression straight to
#        omopgenerics::newConceptSetExpression() with no translation step.
#        A pre-0.4.20 file migrates losslessly: a flat list of concept ids is an
#        expression with nothing excluded and no descendants.
# 0.4.21 gave a plain cohort optional typed `operations`: an ordered list of
#        CohortConstructor verbs (see R/cohort_operations.R), which is what
#        finally lets cohort code be generated rather than described. Free text
#        is untouched and NEVER auto-converted -- prose does not carry the facts
#        a call needs, which is the whole reason for the type. A cohort with
#        operations renders its logic and its code from them; one without renders
#        exactly as before.
# 0.4.22 gave each objective a stable {id, text} and let an analysis name the
#        objectives it answers. Position was the only identity an objective had,
#        and position is the one identity that cannot be referenced: inserting an
#        objective silently repoints every reference below it. The link is
#        many-to-many -- one objective is usually answered by several analyses
#        (complete, 5-year and 2-year prevalence of the same disease) -- so an
#        analysis carries a list. objective_coverage_problems() then reports an
#        objective nothing implements and an analysis serving nothing, which is
#        the check that makes an incomplete plan visible rather than inferable.
#        A pre-0.4.22 file migrates losslessly: position WAS the identity, so
#        ids are minted in order.
# 0.4.23 retired the `comparator` and `strata` cohort kinds. Neither is a cohort
#        in an incidence-prevalence study: no estimator in IncidencePrevalence
#        takes a comparator -- that is a comparative-cohort design, and a
#        different package -- and strata are the fixed age_group / sex columns
#        the denominator generator writes (STRATA_VARIABLES), chosen on the
#        analysis, never defined as a cohort. Offering them invited a plan that
#        could not be run. Nothing is lost on load: both kinds always rendered
#        the plain-cohort block, and both now alias to `other`, which renders
#        exactly the same fields.
# 0.4.24 cut the analysis-type dropdown to Incidence, Prevalence and Other. The
#        seven it dropped -- cohort characterisation, comparative cohort,
#        self-controlled case series, case-control, survival analysis,
#        patient-level prediction, drug utilisation -- never had a template, so
#        each already rendered the generic "Other" block and serialised the same
#        generic fields. The label was the only thing that distinguished them,
#        and it promised an estimator this app cannot generate; each would need
#        its own package before it meant anything.
# 0.4.25 removed the migration layer: migrate_sap() and its six helpers, plus
#        coalesce_key(). This app is pre-release and there are no SAPs in the
#        wild, so every one of them was repairing a file that cannot exist. A
#        file is now read as the current schema and nothing rewrites it on the
#        way in.
#
#        The entries above are kept as the RATIONALE for why each field is
#        shaped the way it is -- that is still worth reading. Their "a pre-X
#        file migrates losslessly" claims are not: nothing migrates any more.
#        Treat them as history, not as behaviour.
# 0.4.26 finished the sweep 0.4.25 started, taking the legacy paths out of the
#        templates themselves: the Incidence `estimand` wrapper and its
#        snake_case renames, the Prevalence snake_case reads and the
#        interval_length_days guess, the `stratifications` fallback, the
#        top-level-parameters fallback in analysis_to_prefill(), the "unbounded"
#        washout sentinel, the `role` cohort vocabulary, and the "Incidence rate"
#        type alias. What is left in flatten() is its real job: the inverse of
#        collect(), reconciling the few input ids that differ from their JSON
#        keys, and defaulting the keys a type switch leaves absent.
# 0.4.27 gave an analysis `role` (primary / sensitivity): not an estimator
#        argument -- 0.3.1 and 0.4.0 were right to drop `sensitivity_analyses`,
#        since re-running with another washout is a second CALL -- but the
#        distinction is the first thing a reviewer looks for, and without a field
#        an author could only spell it into the analysis name, where nothing
#        could group or check it. analysis_role_problems() reports a plan that
#        states roles but marks none of them primary.
#
#        No key changed for the other fix in this version, because the key was
#        already there and simply ignored. `data_sources` -- WHICH databases a
#        cohort is built in and an estimate runs against -- reached the document
#        and stopped there, so a plan restricting an analysis to two of five
#        databases generated a script that ran it at all five. That is the
#        dangerous direction: the plan a reviewer signed said one thing and the
#        code exported another. Restricted cohorts and estimates are now guarded
#        with omopgenerics::cdmName(cdm), and cohort_source_coverage_problems()
#        reports an estimate that runs where its cohort is never built -- the
#        inconsistency that guard makes possible.
#
#        The other three non-parameter fields -- name, role, objectives -- are
#        now carried INTO the result, not just into a comment above the call.
#        None of them is an estimator argument, because none is a computational
#        choice: the primary analysis and its sensitivity analyses call the same
#        function with the same arguments, and what separates them is which one
#        the study may conclude from. omopgenerics settings are the documented
#        home for that, so the generated script defines sapTag() and labels each
#        estimate with sap_analysis / sap_role / sap_objectives. Verified against
#        omopgenerics 1.4.1 and IncidencePrevalence 1.2.1: the columns survive
#        bind(), suppress() and the export/import round trip, so a study report
#        can tell the primary estimate from the other eight without going back
#        to the plan.
# 0.4.28 added the `bind_cohorts` operation: several cohort definitions gathered
#        into ONE table, so one estimator call covers all of them.
#
#        estimatePrevalence()'s outcomeTable takes one table -- a vector fails
#        with "You can only read one table of a cdm_reference" -- but a table may
#        hold many cohorts and the estimator reports each separately. That is
#        already why C1-001's six cancers are one analysis; what it could not do
#        was combine definitions built by DIFFERENT pipelines.
#
#        Only cohorts with disjoint NAMES can merge: bind() aborts on a
#        duplicated cohort_name, and the estimator labels results by cohort_name,
#        so two definitions under one name could never be told apart anyway.
#        conceptCohort() names cohorts after their codelists, so definitions that
#        differ only in exit (C1-001's three prevalence definitions) collide and
#        stay separate analyses.
#
#        The op is the first whose call returns a CDM REFERENCE rather than a
#        cohort table, so register_cohort_op() gained `assigns`: "cdm" emits
#        `cdm <- bind(...)`, "table" (the default, every CohortConstructor verb)
#        emits `cdm$x <- ...`. Getting that wrong does not fail loudly -- it
#        leaves the table unattached and every estimate on it dies at the data
#        partner -- which is why it is declared per op rather than inferred.
#
#        bound_cohort_problems() reports the four things the op's own validate()
#        cannot see: binding a cohort nothing builds, one defined AFTER the bind,
#        a denominator, or constituents whose tables share a cohort name.
# 0.4.29 took the codes back out of the SAP, reversing 0.4.16/0.4.20: a codelist
#        is now {name, category, description} -- no upload, no
#        concept_set_expression, no resolved `codes` snapshot, no source_file or
#        vocabulary_version. The codes live with the study (the CSVs a codelist
#        tool produces, placed under studyCode/codelist/), not in the plan, so
#        the document's codelist-contents appendix is gone and the generated
#        code reads or TODO-marks each cited codelist instead of inlining it.
# 0.4.30 cut the codelist categories to the three roles an incidence-prevalence
#        study actually assigns -- Index event, Outcome, Covariate -- dropping
#        Medicine / Procedure / Condition / Comorbidity (OMOP-domain-flavoured
#        labels that said what the concepts WERE, not what the study uses them
#        for) and replacing the explicit "Other" option with Covariate. Still
#        free text for anything else; unset still renders under "Other".
SAP_SCHEMA_VERSION <- "0.4.30"

# Overridable so tests or a deployment can write somewhere else.
OUTPUT_DIR <- getOption("shinySAP.output_dir", "output")

ui <- page_navbar(
  title = "shinySAP",
  id = "nav",
  theme = bs_theme(version = 5, preset = "shiny"),
  window_title = "shinySAP",
  header = tags$head(tags$style(HTML("
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

    /* Load SAP in the navbar: strip fileInput() down to its button -- no
       filename box, no progress bar -- and dress it as a nav link in the
       theme's primary blue. It shares one right-pinned nav_item with the
       autosave status (nav_spacer() does the pinning; a second auto margin
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
  ")), tags$script(src = "codelist_refs.js")),
  nav_panel("Study", div(class = "container-fluid py-3", study_ui("study"))),
  nav_panel("CDM Sources", div(class = "container-fluid py-3", cdm_sources_ui("sources"))),
  nav_panel("CDM Changes", div(class = "container-fluid py-3", cdm_changes_ui("cdm"))),
  nav_panel("Codelists", div(class = "container-fluid py-3", codelists_ui("codelists"))),
  nav_panel("Cohorts", div(class = "container-fluid py-3", cohorts_ui("cohorts"))),
  nav_panel("Analyses", div(class = "container-fluid py-3", analyses_ui("analyses"))),
  nav_panel("Review", div(class = "container-fluid py-3", review_ui("review"))),
  nav_spacer(),
  # One group at the right edge: Save beside Load, both acting on the whole
  # SAP rather than one section. Save is a LINK, not just a status: it reads
  # "Save" until the working file exists, then "Saved HH:MM", and clicking it
  # either way writes THE working file -- one file per SAP, rewritten in
  # place, never a new copy.
  nav_item(
    div(
      class = "d-flex align-items-center gap-3",
      div(class = "navbar-text py-0",
          uiOutput("save_status", inline = TRUE)),
      # A quiet vertical rule: Save and Load are two whole-SAP actions side by
      # side, not one control -- the divider says where one ends.
      div(class = "vr my-2"),
      tagAppendAttributes(
        fileInput("load", NULL, accept = ".json",
                  buttonLabel = tagList(icon("upload"), "Load SAP"),
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
  objective_choices <- reactive({
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
  sap <- reactive(list(
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
    nav_select("nav", selected = "Study", session = session)
  }

  observeEvent(input$load, {
    loaded <- tryCatch(read_sap(input$load$datapath), error = function(e) e)
    if (inherits(loaded, "error")) {
      showNotification(paste("Could not read that file:", conditionMessage(loaded)), type = "error")
      return()
    }
    failed <- tryCatch({
      load_sap(loaded)
      NULL
    }, error = function(e) e)
    if (!is.null(failed)) {
      showNotification(paste("Could not load that SAP:", conditionMessage(failed)), type = "error")
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
    showNotification("SAP loaded.", type = "message")
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
  working_file <- reactiveVal(NULL)
  save_dir     <- reactiveVal(OUTPUT_DIR)
  saved_at     <- reactiveVal(NULL)
  # TRUE only for a LOADED file: that name the user chose, so it stays. An
  # app-derived name follows the study identity instead (see next_path).
  sticky_name  <- reactiveVal(FALSE)

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

  sap_settled <- debounce(sap, 2000)
  observeEvent(sap_settled(), {
    s <- sap_settled()
    if (sap_is_empty(s)) return()   # the app as it starts: nothing to keep yet
    persist(s)
  })

  # The navbar Save link doubles as the indicator: "Save" until the working
  # file exists, then "Saved HH:MM". Re-rendering keeps the same input id, so
  # the click observer below survives every relabel. The full path rides in
  # the tooltip; the navbar only has room for the when. .navbar-save (see the
  # header CSS) dresses it exactly like Load, in green.
  output$save_status <- renderUI({
    at <- saved_at()
    actionLink(
      "save_now",
      class = "navbar-save",
      title = if (is.null(at)) "Save the SAP to a file; every later save rewrites it"
              else sprintf("Every save and autosave rewrites %s. Click to save now.",
                           working_file()),
      if (is.null(at)) tagList(icon("floppy-disk"), "Save")
      else tagList(icon("check"), sprintf("Saved %s", format(at, "%H:%M")))
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
  problems <- reactive(c(study_problems(study$data()),
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
  observeEvent(input$save_now, {
    s <- sap()
    if (is.na(s$study$title %||% NA)) {
      showNotification("Give the study a title before saving.", type = "warning")
      return()
    }
    if (!is.null(working_file())) {
      path <- save_working(s, next_path(s), length(problems()))
      if (!is.null(path)) adopt(path)
      return()
    }
    showModal(modalDialog(
      title = "Save SAP",
      textInput("save_dir", "Folder to save into", value = save_dir(), width = "100%"),
      div(class = "form-text",
          sprintf("Creates %s in this folder (made if missing); every later save and
                   autosave rewrites that same file. A relative path is inside the app
                   folder; ~ is your home folder.",
                  basename(working_sap_path(s$study, ".")))),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_confirm", "Save", class = "btn btn-success",
                     icon = icon("floppy-disk"))
      ),
      easyClose = TRUE
    ))
  })

  # An unwritable folder keeps the dialog open with an error, so the typed
  # path is not lost.
  observeEvent(input$save_confirm, {
    dir <- path.expand(trimws(input$save_dir %||% ""))
    if (!nzchar(dir)) {
      showNotification("Name a folder to save into.", type = "warning")
      return()
    }
    s <- sap()
    path <- tryCatch(save_working(s, working_sap_path(s$study, dir), length(problems())),
                     error = function(e) {
                       showNotification(paste("Could not save there:", conditionMessage(e)),
                                        type = "error")
                       NULL
                     })
    if (is.null(path)) return()
    adopt(path)
    removeModal()
  })

  review_server("review", sap = sap, problems = problems)
}

shinyApp(ui, server)
