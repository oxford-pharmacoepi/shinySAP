
# TODO
# updateCodelist
# removeCodelist
# appendCodelist

prepareCodelists <- function(codelist, call = parent.frame()) {
  if (inherits(codelist, "omop_sap_codelist")) {
    return(codelist)
  }
  # input check
  msg <- paste(
    "`codelist` has to be a {.cls codelist}, {.cls codelist_with_details},",
    "or {.cls concept_set_expression} object, or a list containing any",
    "combination of these object types."
  )
  if (inherits(codelist, "codelist")) {
    codelist <- prepareCodelist(codelist)
  } else if (inherits(codelist, "codelist_with_details")) {
    codelist <- prepareCodelistWithDetails(codelist)
  } else if (inherits(codelist, "concept_set_expression")) {
    codelist <- prepareConceptSetExpression(codelist)
  } else if (inherits(codelist, "list")) {
    codelist <- codelist |>
      purrr::map(\(x) prepareCodelists(x, call = call)) |>
      purrr::list_flatten(name_repair = "minimal")
    # check names
    dup <- sort(unique(names(codelist)[duplicated(names(codelist))]))
    if (length(dup) > 0) {
      cli::cli_abort(c(x = "Duplicated names are not allowed for codelists: {dup}."), call = call)
    }
  } else {
    cli::cli_abort(c(x = msg), call = call)
  }
  codelist <- codelist[sort(names(codelist))]
  class(codelist) <- c("omop_sap_codelist", "list")
  return(codelist)
}
prepareCodelist <- function(x) {
  names(x) |>
    rlang::set_names() |>
    purrr::map(\(nm) {
      list(
        type = "codelist",
        content = dplyr::tibble(concept_id = x[[nm]])
      )
    })
}
prepareCodelistWithDetails <- function(x) {
  unclass(x) |>
    purrr::map(\(x) {
      list(
        type = "codelist_with_details",
        content = x
      )
    })
}
prepareConceptSetExpression <- function(x) {
  unclass(x) |>
    purrr::map(\(x) {
      list(
        type = "concept_set_expression",
        content = x
      )
    })
}

#' @export
print.omop_sap_codelist <- function(x, ...) {
  nms <- names(x)
  types <- unname(purrr::map_chr(x, "type"))
  typs <- sort(unique(types))
  titles <- list(
    codelist = "Codelists",
    codelist_with_details = "Codelists with details",
    concept_set_expression = "Concept set expressions"
  )
  # shorten names
  cli_title("OMOP Codelists")
  for (typ in typs) {
    cli::cli_inform(c("*" = "{sum(types == typ)} {.emph {titles[[typ]]}}:"))
    printNames(nms[types == typ])
  }
  invisible(x)
}
printNames <- function(x) {
  width <- cli::console_width()
  widthPerName <- 31
  widthColumn <- 35
  long <- nchar(x) > widthPerName
  x[long] <- paste0(substr(x[long], 1, widthPerName - 3), "...")
  columns <- min(length(x), floor(width / widthColumn))
  if (columns < 2) {
    cat(paste0(" - ", x), sep = "\n")
  } else {
    # exact width of column
    x <- paste0(" - ", x, strrep(" ", pmax(0, widthColumn - 3 - nchar(x))))
    rows <- ceiling(length(x) / columns)
    id <- rep(seq_len(rows), length.out = length(x))
    x <- seq_len(rows) |>
      purrr::map_chr(\(i) trimws(paste0(x[id == i], collapse = ""), which = "right"))
    cat(x, sep = "\n")
  }
  invisible()
}

#' @export
`c.omop_sap_codelist` <- function(...) {
  prepareCodelists(unname(list(...)), call = parent.frame())
}

#' @export
`[[<-.omop_sap_codelist` <- function(x, i, value) {
  omopgenerics::assertCharacter(i, length = 1)
  if (!inherits(value, "codelist") &
      !inherits(value, "codelist_with_details") &
      !inherits(value, "concept_set_expression") &
      !is.null(value)) {
    cli::cli_abort(c(
      x = "`value` must be a {.cls codelist}, {.cls codelist_with_details} or {.cls concept_set_expression} object.",
      i = "You can use `omopgenerics::newCodelist()`, `omopgenerics::newCodelistWithDetails()` or `omopgenerics::newConceptSetExpression()`."
    ))
  }
  if (!is.null(value)) {
    if (length(value) != 1L) {
      cli::cli_abort("`value` must contain exactly one codelist.")
    }
    value <- prepareCodelists(value)[[1]]
  }
  NextMethod()
}

#' @export
`$<-.omop_sap_codelist` <- function(x, i, value) {
  x[[i]] <- value
  x
}

#' @export
`[<-.omop_sap_codelist` <- function(x, i, value) {
  cli::cli_abort(c(x = "Use `$` or `[[` to add or edit codelists."))
}
