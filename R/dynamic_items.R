# Manages a growing, removable list of item modules inside a parent module.
#
# Each item is a real Shiny module rather than a re-rendered block of UI, so
# adding or removing one never resets the inputs of its siblings.
#
#   container   id of the <div> the items are appended to (unnamespaced)
#   item_ui     function(id, prefill) -> UI whose root div has id paste0(id, "-box")
#   item_server function(id, prefill, on_remove) -> reactive() giving the item's data
dynamic_items <- function(prefix, container, item_ui, item_server,
                          session = shiny::getDefaultReactiveDomain()) {
  # Pin the owning module's session. add()/remove() are also called from other
  # modules (loading a saved SAP is driven from the Review tab), and both
  # moduleServer() and insertUI() would otherwise namespace against the
  # *caller's* domain -- inserting UI under this module's id while binding the
  # item's inputs under the caller's, so every input$* would read NULL.
  parent <- session
  ns <- parent$ns
  ids <- reactiveVal(character(0))
  handlers <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  state$counter <- 0

  remove_item <- function(iid) {
    withReactiveDomain(parent, {
      removeUI(selector = paste0("#", ns(iid), "-box"), immediate = TRUE, session = parent)
    })
    if (!is.null(handlers[[iid]])) rm(list = iid, envir = handlers)
    ids(setdiff(isolate(ids()), iid))
  }

  add_item <- function(prefill = NULL) {
    state$counter <- state$counter + 1
    iid <- sprintf("%s_%d", prefix, state$counter)
    withReactiveDomain(parent, {
      insertUI(
        selector = paste0("#", ns(container)),
        where = "beforeEnd",
        ui = item_ui(ns(iid), prefill),
        immediate = TRUE,
        session = parent
      )
      handlers[[iid]] <- item_server(iid, prefill, function() remove_item(iid))
    })
    ids(c(isolate(ids()), iid))
  }

  clear <- function() for (iid in isolate(ids())) remove_item(iid)

  list(
    add = add_item,
    clear = clear,
    count = reactive(length(ids())),
    data = reactive(lapply(ids(), function(iid) handlers[[iid]]()))
  )
}

# Shared chrome for one item card: every repeating section (CDM sources, CDM
# changes, cohorts, analyses) is built from this, so they all collapse the same way.
#
# The body collapses because a SAP with a dozen cohorts is otherwise a very long
# scroll. A collapsed card still has to say WHICH cohort it is, or collapsing makes
# navigation worse rather than better -- hence `card_label`, which each item server
# fills with the item's own name (item_card_label()).
#
# Collapsing hides the body with display: none, and a hidden output does not
# render. Every uiOutput inside a card body that *contains inputs* already sets
# suspendWhenHidden = FALSE -- it had to, because the tab pane itself is hidden
# until selected -- so a collapsed card keeps building and reporting its inputs and
# still saves. Adding a new one without that flag would silently save it empty.
item_card <- function(id, title, ...) {
  ns   <- NS(id)
  body <- ns("body")
  div(
    id = ns("box"), class = "card mb-3",
    div(
      class = "card-header d-flex justify-content-between align-items-center py-2",
      tags$button(
        class = paste("btn btn-sm btn-link text-body text-decoration-none p-0",
                      "d-flex align-items-center gap-2 item-card-toggle"),
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", body),
        `aria-expanded` = "true",
        `aria-controls` = body,
        icon("chevron-down", class = "item-card-chevron small"),
        tags$strong(title),
        # The item's own name, so a collapsed card is still identifiable.
        tags$span(class = "text-muted fw-normal",
                  textOutput(ns("card_label"), inline = TRUE))
      ),
      actionButton(ns("remove"), "Remove", class = "btn btn-sm btn-outline-danger")
    ),
    div(id = body, class = "collapse show", div(class = "card-body", ...))
  )
}

# Fills the header label of an item_card(). `label` is a reactive giving the text.
#
# suspendWhenHidden = FALSE for the same reason as everywhere else here: the tab
# pane is hidden until it is first selected, and a suspended output would leave
# every card in a freshly loaded SAP labelled blank until you clicked into the tab.
item_card_label <- function(output, label) {
  output$card_label <- renderText(label())
  outputOptions(output, "card_label", suspendWhenHidden = FALSE)
}

# Collapse / expand every card in a section at once. The real navigation win when a
# SAP has a dozen cohorts: `selector` is the container the cards were inserted into.
collapse_all_button <- function(container_selector) {
  tags$button(
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
  div(class = "text-muted fst-italic border rounded p-4 text-center", text)
}

# A picker over entities defined elsewhere in the SAP (a cohort, a CDM source).
# create = TRUE so an analysis can name something not yet written down.
entity_picker <- function(inputId, label, selected = "", choices = character(0), # nolint: object_name_linter.
                          multiple = FALSE, placeholder = "Select or type") {
  selected <- as.character(unlist(selected))
  selectizeInput(
    inputId, label,
    choices = unique(c("", choices, selected)),
    selected = selected, multiple = multiple, width = "100%",
    options = list(create = TRUE, placeholder = placeholder)
  )
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
  observe({
    available <- choices()
    for (field in if (is.function(fields)) fields() else fields) {
      current <- isolate(session$input[[field]])
      if (!is.null(current)) {
        assign(field, TRUE, envir = reported)
      } else if (!isTRUE(mget(field, envir = reported, ifnotfound = FALSE)[[1]])) {
        current <- as.character(unlist(pf(field)))
      }
      if (is.null(current)) current <- character(0)
      updateSelectizeInput(
        session, field,
        choices = unique(c("", available, current)),
        selected = current
      )
    }
  })
}
