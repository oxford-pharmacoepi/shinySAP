
prepareCodelists <- function(codelist, call = parent.frame()) {
  # input check
  msg <- paste(
    "`codelist` has to be a {.cls codelist}, {.cls codelist_with_details},",
    "or {.cls concept_set_expression} object, or a list containing any",
    "combination of these object types."
  )
  if (inherits(codelist, "codelist")) {
    return(prepareCodelist(codelist))
  }
  if (inherits(codelist, "codelist_with_details")) {
    return(prepareCodelistWithDetails(codelist))
  }
  if (inherits(codelist, "concept_set_expression")) {
    return(prepareConceptSetExpression(codelist))
  }
  if (inherits(codelist, "list")) {
    codelist <- codelist |>
      purrr::map(\(x) prepareCodelists(x, call = call)) |>
      purrr::list_flatten(name_repair = "minimal")
    # check names
    dup <- sort(unique(names(x)[duplicated(names(x))]))
    if (length(dup) > 0) {
      cli::cli_abort(c(x = "Duplicated names are not allowed for codelists: {dup}."), call = call)
    }
    return(codelist)
  }
  cli::cli_abort(c(x = msg))
}
prepareCodelist <- function(x) {
  names(x) |>
    rlang::set_names() |>
    purrr::map(\(nm) dplyr::tibble(concept_id = x[[nm]]))
}
prepareCodelistWithDetails <- function(x) {
  unclass(x)
}
prepareConceptSetExpression <- function(x) {
  unclass(x)
}
