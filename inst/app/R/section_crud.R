# The CRUD half of a repeating section -------------------------------------------
#
# The SAP is ONE object -- a shinySAP `sap` held in a reactiveVal in app.R -- and
# the only way anything in the app changes it is through the package's own
# functions: shinySAP::addSapComponent(), updateSapComponent(),
# removeSapComponent(), updateStudy(), with validate = FALSE because a plan is
# written incrementally and shinySAP::checkSap() reports what is still missing.
# Every component is built by its package constructor, also with validate = FALSE.
#
# dynamic_items() is the UI half (cards on the page); sap_collection() is the data
# half every repeating section shares. Neither keeps SAP data of its own.
#
# Sourced after problems.R and before utils.R (alphabetical); nothing here runs
# at the top level.

# A change-only view of the SAP. A reactiveVal ignores an identical value, so a
# view of "the cohort ids" wakes its dependents only when an id appears or goes,
# not on every keystroke anywhere in the plan.
sap_view <- function(sap, f) {
  view <- shiny::reactiveVal(f(shiny::isolate(sap())))
  # High priority: a view must be current before anything that depends on it
  # runs in the same flush -- the card reconciler in particular, which would
  # otherwise see the previous plan's ids for one flush after a load.
  shiny::observe(view(f(sap())), priority = 100)
  view
}

# `sap`           the reactiveVal holding the sap object
# `collection`    one of the SAP collection names
# `new_component` function(id) -> an empty draft component from the constructor
# `copy`          function(component, id) -> a copy under a new id, via the constructor
# `to_prefill`    function(component) -> the prefill a card is rebuilt from
sap_collection <- function(sap, collection, new_component, copy, to_prefill = identity) {
  state <- new.env(parent = emptyenv())
  state$issued <- character(0)   # ids handed out or removed this session, never reissued
  state$undo   <- NULL
  current <- function() shiny::isolate(sap())
  ids_now <- function() shinySAP::sapComponentIds(current(), collection)
  mint <- function() {
    id <- shinySAP::newSapId(current(), collection, taken = state$issued)
    state$issued <- unique(c(state$issued, id))
    id
  }

  list(
    ids = sap_view(sap, function(s) shinySAP::sapComponentIds(s, collection)),

    get = function(id) shinySAP::getSapComponent(current(), collection, id),

    # The prefill a card is built from, plus the component itself so the card
    # can keep its parameters until its own inputs have reported.
    prefill_of = function(id) {
      component <- shinySAP::getSapComponent(current(), collection, id)
      c(to_prefill(component), list(.component = component))
    },

    add = function() {
      id <- mint()
      sap(shinySAP::addSapComponent(current(), new_component(id), validate = FALSE))
      id
    },

    # A card writes itself back. A card whose id is gone (removed, or replaced
    # by a load) writes nothing: updateSapComponent() would abort on it.
    write = function(component) {
      if (!component$id %in% ids_now()) return(invisible(FALSE))
      sap(shinySAP::updateSapComponent(current(), component, validate = FALSE))
      invisible(TRUE)
    },

    remove = function(id) {
      if (!id %in% ids_now()) return(invisible(NULL))
      state$undo <- shinySAP::getSapComponent(current(), collection, id)
      state$issued <- unique(c(state$issued, id))
      sap(shinySAP::removeSapComponent(current(), collection, id, validate = FALSE))
      invisible(id)
    },

    # Single-slot undo: the last removed component goes back in, id included.
    undo = function() {
      component <- state$undo
      state$undo <- NULL
      if (is.null(component) || component$id %in% ids_now()) return(NULL)
      sap(shinySAP::addSapComponent(current(), component, validate = FALSE))
      component$id
    },

    # A removed component belongs to the plan it was removed from: once another
    # plan is loaded, undo must not carry it across.
    forget = function() {
      state$undo <- NULL
      invisible(NULL)
    },

    duplicate = function(id) {
      source <- shinySAP::getSapComponent(current(), collection, id)
      new_id <- mint()
      sap(shinySAP::addSapComponent(current(), copy(source, new_id), validate = FALSE))
      new_id
    }
  )
}

# A copy is a NEW item, so its name says so; an unnamed source stays unnamed.
copy_name <- function(name) if (is.null(name)) NULL else paste(name, "(copy)")

# Read a card's inputs with the prefill as the fallback until the client has
# reported each one: Shiny inputs are NULL before their first report, and a card
# built from a saved SAP must not write blanks over the plan in that window. Once
# an input has reported, NULL (a cleared multi-select) is taken at its word.
card_reader <- function(input, prefill) {
  reported <- new.env(parent = emptyenv())
  function(key) {
    value <- input[[key]]
    if (!is.null(value)) {
      assign(key, TRUE, envir = reported)
      return(value)
    }
    if (isTRUE(mget(key, envir = reported, ifnotfound = FALSE)[[1]])) return(NULL)
    prefill[[key]]
  }
}

# The type-specific parameters a card writes: what its template collects once
# the block's inputs have reported; until then, for the type the card was built
# with, the parameters it was built from -- so a loaded plan is never blanked in
# the flush before its type block exists. A type just switched to has no
# parameters yet.
card_parameters <- function(input, type, template, prefill) {
  if (!nzchar(type)) return(list())
  ids <- template_input_ids(template)
  reported <- !length(ids) || any(vapply(ids, function(id) !is.null(input[[id]]), logical(1)))
  if (reported) return(template$collect(input))
  component <- prefill$.component
  if (!is.null(component) && identical(as.character(component$type %||% ""), type)) {
    return(component$parameters %||% list())
  }
  list()
}

# template_field_ids() renders the template's UI to discover its ids, so the
# answer is remembered per template. Matched on the ui FUNCTION, not its source
# text: templates built by one factory (point and period prevalence, the two
# survival types) share their text and differ only in what the closure captured.
template_ids_cache <- new.env(parent = emptyenv())
template_ids_cache$entries <- list()
template_input_ids <- function(template) {
  for (entry in template_ids_cache$entries) {
    if (identical(entry$ui, template$ui)) return(entry$ids)
  }
  ids <- setdiff(template_field_ids(template), c(DISPLAY_ONLY_IDS, COHORT_DISPLAY_ONLY_IDS))
  template_ids_cache$entries <- c(template_ids_cache$entries, list(list(ui = template$ui, ids = ids)))
  ids
}
