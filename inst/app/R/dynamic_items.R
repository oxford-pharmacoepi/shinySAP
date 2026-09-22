# Cards for a repeating section, kept in step with the SAP -----------------------
#
# Pure UI. The section's SAP ids arrive as a change-only view (sap_collection()$ids)
# and this reconciles the cards on the page with them: a card is inserted for an id
# that appears and removed for an id that goes. Each card is a real Shiny module
# inserted with insertUI, not a re-rendered block, so adding or removing one never
# resets its siblings. No SAP data lives here -- Remove, Duplicate and Undo call
# back into the section, which does the CRUD.
#
#   container    id of the <div> the cards are appended to (unnamespaced)
#   item_ui      function(id, prefill) -> UI whose root div has id paste0(id, "-box")
#   item_server  function(iid, sap_id, prefill, on_remove) -> starts the card's module
#   ids          reactive of the SAP ids to show, in order
#   prefill_of   function(sap_id) -> the prefill a card is built from
#   noun         what one item is called in notifications ("Cohort", "Data source")
#   on_remove    function(sap_id)   the section removes the component
#   on_duplicate function(sap_id)   the section adds a copy
#   on_undo      function()         the section re-adds the last removed; returns its id
dynamic_items <- function(prefix, container, item_ui, item_server, ids, prefill_of,
                          noun = "Item",
                          on_remove = function(id) {},
                          on_duplicate = function(id) {},
                          on_undo = function() NULL,
                          session = shiny::getDefaultReactiveDomain()) {
  # Pin the owning module's session: add/remove are also driven from outside
  # (a load), and insertUI/moduleServer would otherwise namespace against the
  # caller's domain.
  parent <- session
  ns <- parent$ns
  state <- new.env(parent = emptyenv())
  state$counter <- 0          # Shiny module ids are prefix_<n>, distinct from SAP ids
  state$by_id   <- list()     # SAP id -> Shiny module id
  state$owner   <- list()     # Shiny module id -> SAP id
  state$pending_reveal <- character(0)
  generation <- shiny::reactiveVal(0)   # bumped by reset(), so a load rebuilds every card
  observers <- new.env(parent = emptyenv())
  undo_input <- paste0(prefix, "_undo")
  undo_note  <- ns(paste0(prefix, "_undo_note"))

  insert_card <- function(id) {
    # An id the SAP no longer holds (a view one flush behind) gets no card; the
    # next reconciliation settles it.
    prefill <- tryCatch(prefill_of(id), error = function(e) NULL)
    if (is.null(prefill)) return(invisible(NULL))
    state$counter <- state$counter + 1
    iid <- sprintf("%s_%d", prefix, state$counter)
    state$by_id[[id]] <- iid
    state$owner[[iid]] <- id
    shiny::withReactiveDomain(parent, {
      shiny::insertUI(
        selector = paste0("#", ns(container)), where = "beforeEnd",
        ui = item_ui(ns(iid), prefill), immediate = TRUE, session = parent
      )
      item_server(iid, id, prefill, function() request_remove(id))
      # Duplicate lives here, not in the card: the card knows nothing about the
      # list it sits in, and the observer must die with the card.
      observers[[iid]] <- shiny::observeEvent(parent$input[[paste0(iid, "-duplicate")]],
                                              on_duplicate(id), ignoreInit = TRUE)
      # `reveal` scrolls a card the user caused (Add, Duplicate, Undo) into view;
      # cards a load rebuilds must not fight over the viewport.
      if (id %in% state$pending_reveal) {
        state$pending_reveal <- setdiff(state$pending_reveal, id)
        shiny::insertUI(selector = paste0("#", ns(container)), where = "beforeEnd",
                        ui = reveal_item_script(paste0(ns(iid), "-box")),
                        immediate = TRUE, session = parent)
      }
    })
  }

  remove_card <- function(id) {
    iid <- state$by_id[[id]]
    if (is.null(iid)) return(invisible(NULL))
    shiny::withReactiveDomain(parent, {
      shiny::removeUI(selector = paste0("#", ns(iid), "-box"), immediate = TRUE, session = parent)
    })
    if (!is.null(observers[[iid]])) {
      observers[[iid]]$destroy()
      rm(list = iid, envir = observers)
    }
    state$by_id[[id]] <- NULL
    state$owner[[iid]] <- NULL
  }

  # The card's own Remove button: offer an undo, then let the section remove it.
  request_remove <- function(id) {
    shiny::showNotification(
      sprintf("%s removed.", noun),
      action = shiny::actionLink(ns(undo_input), "Undo"),
      duration = 10, id = undo_note, session = parent
    )
    on_remove(id)
  }

  shiny::withReactiveDomain(parent, {
    shiny::observeEvent(parent$input[[undo_input]], {
      id <- on_undo()
      if (!is.null(id)) state$pending_reveal <- c(state$pending_reveal, id)
      shiny::removeNotification(undo_note, session = parent)
    }, ignoreInit = TRUE)

    # The reconciler: the page shows exactly the ids the SAP holds.
    shiny::observe({
      generation()
      wanted <- as.character(ids() %||% character(0))
      shown  <- names(state$by_id) %||% character(0)
      for (id in setdiff(shown, wanted)) remove_card(id)
      for (id in setdiff(wanted, shown)) insert_card(id)
    })
  })

  list(
    # Mark an id whose card, when it appears, is scrolled into view.
    reveal = function(id) {
      if (!is.null(id)) state$pending_reveal <- c(state$pending_reveal, id)
      invisible(id)
    },
    # Drop every card without touching the SAP; the next flush rebuilds them
    # from the ids -- even ids a loaded plan reuses.
    reset = function() {
      for (id in names(state$by_id)) remove_card(id)
      generation(shiny::isolate(generation()) + 1)
    },
    count = shiny::reactive(length(ids() %||% character(0))),
    # Whether a card still owns its id (a removed card's inputs can still emit).
    is_live = function(iid, id) identical(state$owner[[iid]], id)
  )
}

# A one-shot script inserted right after a new card: expand it, scroll it into
# view and put the cursor in its first field. Without this, "Add" appends below
# the fold of a long section and visibly does nothing -- and since cards start
# collapsed, without the expand step it would appear as a bare header with
# nothing to type into. shiny::insertUI() executes scripts on insertion; the timeout
# lets the card finish rendering first.
#
# Only PLAIN text fields (.form-control) are focused. Selectize renders its own
# bare <input> inside every select, and focusing that one auto-opens the
# dropdown -- on a card whose first field is a select (a CDM change's type),
# adding a card would pop a menu nobody asked for.
reveal_item_script <- function(box_id) {
  shiny::tags$script(shiny::HTML(sprintf(
    paste0("setTimeout(function(){",
           "var b=document.getElementById('%s');if(!b)return;",
           "var c=b.querySelector(':scope > .collapse');",
           "if(c)bootstrap.Collapse.getOrCreateInstance(c,{toggle:false}).show();",
           "b.scrollIntoView({behavior:'smooth',block:'center'});",
           "var f=b.querySelector('.card-body input.form-control[type=text],",
           ".card-body textarea.form-control');",
           "if(f)f.focus({preventScroll:true});",
           "},50);"),
    box_id
  )))
}

# Shared chrome for one item card: every repeating section (CDM sources, CDM
# changes, cohorts, analyses) is built from this, so they all collapse the same way.
#
# The body collapses because a SAP with a dozen cohorts is otherwise a very long
# scroll. A collapsed card still has to say WHICH cohort it is, or collapsing makes
# navigation worse rather than better -- hence `card_label`, which each item server
# fills with the item's own name (item_card_label()).
#
# Cards start COLLAPSED: a section is first read as a list of names, and opening
# a loaded SAP as a wall of expanded forms buried that list. The one exception
# is a card the user just created -- Add, Duplicate and Undo pass reveal = TRUE
# to dynamic_items(), and reveal_item_script() expands the new card so there is
# something to type into.
#
# Collapsing hides the body with display: none, and a hidden output does not
# render. Every uiOutput inside a card body that *contains inputs* already sets
# suspendWhenHidden = FALSE -- it had to, because the tab pane itself is hidden
# until selected -- so a collapsed card keeps building and reporting its inputs and
# still saves. Adding a new one without that flag would silently save it empty.
item_card <- function(id, title, ...) {
  ns   <- shiny::NS(id)
  body <- ns("body")
  shiny::div(
    id = ns("box"), class = "card mb-3",
    shiny::div(
      class = "card-header d-flex justify-content-between align-items-center py-2",
      shiny::tags$button(
        class = paste("btn btn-sm btn-link text-body text-decoration-none p-0",
                      "d-flex align-items-center gap-2 item-card-toggle collapsed"),
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", body),
        `aria-expanded` = "false",
        `aria-controls` = body,
        shiny::icon("chevron-down", class = "item-card-chevron small"),
        shiny::tags$strong(title),
        # The item's own name, so a collapsed card is still identifiable.
        shiny::tags$span(class = "text-muted fw-normal",
                  shiny::textOutput(ns("card_label"), inline = TRUE))
      ),
      shiny::div(
        class = "d-flex gap-2 flex-shrink-0",
        # Wired in dynamic_items(): re-adds this card's data as a new card.
        shiny::actionButton(ns("duplicate"), "Duplicate", class = "btn btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("remove"), "Remove", class = "btn btn-sm btn-outline-danger")
      )
    ),
    shiny::div(id = body, class = "collapse", shiny::div(class = "card-body", ...))
  )
}

# Fills the header label of an item_card(). `label` is a reactive giving the text.
#
# suspendWhenHidden = FALSE for the same reason as everywhere else here: the tab
# pane is hidden until it is first selected, and a suspended output would leave
# every card in a freshly loaded SAP labelled blank until you clicked into the tab.
item_card_label <- function(output, label) {
  output$card_label <- shiny::renderText(label())
  shiny::outputOptions(output, "card_label", suspendWhenHidden = FALSE)
}

# Collapse / expand every card in a section at once. The real navigation win when a
# SAP has a dozen cohorts: `selector` is the container the cards were inserted into.
collapse_all_button <- function(container_selector) {
  shiny::tags$button(
    class = "btn btn-sm btn-outline-secondary", type = "button",
    onclick = sprintf(
      paste0("var b=document.querySelectorAll('%s > .card > .collapse');",
             "var anyOpen=Array.prototype.some.call(b,function(e){",
             "return e.classList.contains('show')});",
             "Array.prototype.forEach.call(b,function(e){",
             "bootstrap.Collapse.getOrCreateInstance(e,{toggle:false})[",
             "anyOpen?'hide':'show']()});"),
      container_selector),
    "Collapse / expand all"
  )
}

empty_state <- function(text) {
  shiny::div(class = "text-muted fst-italic border rounded p-4 text-center", text)
}

# A picker over entities defined elsewhere in the SAP (a cohort, a data source,
# a codelist). It STORES ids and SHOWS names: `choices` is a named vector
# (label -> id) or a named list of them (selectize optgroups). No free text --
# every reference must resolve to an item that exists.
entity_picker <- function(inputId, label, selected = character(0), choices = character(0), # nolint: object_name_linter.
                          multiple = FALSE, placeholder = "Select") {
  selected <- chr_vec(selected)
  shiny::selectizeInput(
    inputId, label,
    choices = picker_choices(choices, selected),
    selected = selected, multiple = multiple, width = "100%",
    options = list(placeholder = placeholder)
  )
}

# The choices one picker gets: everything on offer, plus whatever it currently
# holds. A selectize silently drops a selection it cannot find among its
# options, so an id whose item has been removed stays in the list, labelled as
# removed, until the author changes it -- shinySAP::checkSap() reports the
# dangling reference meanwhile.
#
# `available` is a named vector (label -> id) or a named list of them (optgroups).
picker_choices <- function(available, current) {
  current <- chr_vec(current)
  known <- if (is.list(available)) unlist(available, use.names = FALSE) else available
  extra <- setdiff(current, as.character(known %||% character(0)))
  removed <- stats::setNames(extra, sprintf("%s (removed)", extra))
  if (!is.list(available)) return(c("", available, removed))
  out <- c(list(""), available)
  if (length(removed)) out[["Removed"]] <- removed
  out
}

# Keep entity_picker()s in step with the section that defines those entities.
#
# The first time this runs the inserted inputs have not reported back from the
# client, so input$field is NULL and we must fall back to the prefilled value
# or a freshly loaded SAP would be blanked. But a multi-select the user has
# emptied also reads NULL -- so once a field has reported any value, take the
# input at its word and never reinstate the prefill.
#
# `fields` may be a function, for a caller whose set of pickers depends on
# another input: reading that input inside it makes the observer re-run when the
# set changes, and pick up the pickers that have just been rendered.
sync_pickers <- function(session, fields, choices, pf) {
  reported <- new.env(parent = emptyenv())
  shiny::observe({
    available <- choices()
    for (field in if (is.function(fields)) fields() else fields) {
      current <- shiny::isolate(session$input[[field]])
      if (!is.null(current)) {
        assign(field, TRUE, envir = reported)
      } else if (!isTRUE(mget(field, envir = reported, ifnotfound = FALSE)[[1]])) {
        current <- as.character(unlist(pf(field)))
      }
      if (is.null(current)) current <- character(0)
      shiny::updateSelectizeInput(
        session, field,
        choices = picker_choices(available, current),
        selected = current
      )
    }
  })
}
