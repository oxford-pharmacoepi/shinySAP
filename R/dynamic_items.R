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
  counter <- 0

  remove_item <- function(iid) {
    withReactiveDomain(parent, {
      removeUI(selector = paste0("#", ns(iid), "-box"), immediate = TRUE, session = parent)
    })
    if (!is.null(handlers[[iid]])) rm(list = iid, envir = handlers)
    ids(setdiff(isolate(ids()), iid))
  }

  add_item <- function(prefill = NULL) {
    counter <<- counter + 1
    iid <- sprintf("%s_%d", prefix, counter)
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

# Shared chrome for one item card.
item_card <- function(id, title, ...) {
  ns <- NS(id)
  div(
    id = ns("box"), class = "card mb-3",
    div(
      class = "card-header d-flex justify-content-between align-items-center py-2",
      tags$strong(title),
      actionButton(ns("remove"), "Remove", class = "btn btn-sm btn-outline-danger")
    ),
    div(class = "card-body", ...)
  )
}

empty_state <- function(text) {
  div(class = "text-muted fst-italic border rounded p-4 text-center", text)
}
