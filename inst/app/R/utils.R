# Small helpers shared across the app --------------------------------------
#
# Sourced LAST by loadSupport() (alphabetical), so nothing in another R/ file may
# call these at the top level -- inside a function body is fine.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

# A textarea holds one item per line; the SAP holds a character vector.
split_lines <- function(x) {
  if (is.null(x)) return(character(0))
  v <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  v[nzchar(v)]
}

join_lines <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = "\n")
}

# The SAP records what was authored and nothing else: a blank field is ABSENT,
# not "" and not NA. shinySAP::checkSap() then reports a missing required
# field, and an optional one is simply not written.
chr_or_null <- function(x) {
  x <- trimws(as.character(x %||% "")[1])
  if (is.na(x) || !nzchar(x)) NULL else x
}

num_or_null <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x %||% NA)[1]))
  if (is.na(x)) NULL else x
}

# A picker's value as a clean character vector: no NA, no blanks.
chr_vec <- function(x) {
  v <- trimws(as.character(unlist(x %||% character(0))))
  v[!is.na(v) & nzchar(v)]
}

# Drop the NULL entries of a list, so an unauthored field never reaches the SAP.
compact <- function(x) x[!vapply(x, is.null, logical(1))]

# A dateInput's `value`: a Date, or NULL to start the field genuinely blank.
as_date_value <- function(x) {
  if (inherits(x, "Date")) x <- format(x[1], "%Y-%m-%d")
  x <- trimws(as.character(x %||% "")[1])
  if (is.na(x) || !nzchar(x)) return(NULL)
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (is.na(d)) NULL else d
}

# A dateInput that genuinely starts blank -- which shiny::dateInput() will not
# do: its JS binding substitutes TODAY whenever the field has no
# `data-initial-date`. Blank is a meaningful value here (an unset date range
# bound means "the whole observation period"), so an empty string is set
# explicitly and the picker starts empty and stays that way.
date_input <- function(inputId, label, value = NULL, # nolint: object_name_linter.
                       placeholder = "YYYY-MM-DD", ...) {
  value <- as_date_value(value)
  di <- shiny::dateInput(inputId, label, value = value, width = "100%", ...)
  tq <- htmltools::tagQuery(di)$find("input")$addAttrs(placeholder = placeholder)
  if (is.null(value)) tq <- tq$addAttrs("data-initial-date" = "")
  tq$allTags()
}

# Two date inputs -> one date_range (a two-element Date, NA for an open bound),
# or NULL when neither is set: the schema's date_range is one field.
date_range_value <- function(start, end) {
  s <- as_date_value(start)
  e <- as_date_value(end)
  if (is.null(s) && is.null(e)) return(NULL)
  as.Date(c(if (is.null(s)) NA else format(s), if (is.null(e)) NA else format(e)))
}

# One bound of a date_range, for pf() on the way back in. NULL when unset.
date_bound <- function(range, i) {
  if (is.null(range) || length(range) < i) return(NULL)
  v <- range[[i]]
  if (is.null(v) || is.na(v)) return(NULL)
  format(as.Date(v), "%Y-%m-%d")
}

slugify <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  substr(x, 1, 60)
}

# The base name every saved or downloaded SAP file shares:
# sap-<study_id>[-<version>]. The study id is the schema's identifier; the
# title only stands in while there is none. A leading "sap" is dropped -- the
# name already starts sap-.
sap_file_base <- function(study) {
  stem <- slugify(study$study_id %||% study$title)
  stem <- sub("^sap(-|$)", "", stem)
  if (!nzchar(stem)) stem <- "untitled"
  ver <- gsub("[^A-Za-z0-9._-]+", "", as.character(study$version %||% ""))
  if (nzchar(ver)) sprintf("sap-%s-%s", stem, ver) else sprintf("sap-%s", stem)
}

# Builds the value-lookup the *_item_ui() functions use when rebuilding a card:
# pf("name") returns the stored value or a default.
prefiller <- function(prefill) {
  function(key, default = "") {
    v <- prefill[[key]]
    if (is.null(v) || (length(v) == 1 && is.atomic(v) && is.na(v))) default else v
  }
}

# Files -------------------------------------------------------------------------
#
# A SAP lives in ONE file, written by shinySAP::writeSap(): no timestamped
# copies and no separate autosave file. The working file is created the first
# time there is anything to keep, and every write after that rewrites it.
working_sap_path <- function(study, dir) {
  file.path(dir, sprintf("%s.json", sap_file_base(study)))
}

# One deliberate save, with its guards and its notifications. Returns the path,
# or NULL when nothing was saved. Must run inside a Shiny session.
save_working <- function(sap, path, n_problems = 0) {
  missing <- Filter(function(key) is.null(chr_or_null(sap$study[[key]])), c("study_id", "title"))
  if (length(missing)) {
    shiny::showNotification(
      sprintf("Give the study a %s before saving.", paste(gsub("_", " ", missing), collapse = " and ")),
      type = "warning")
    return(invisible(NULL))
  }
  path <- shinySAP::writeSap(sap, path)
  if (n_problems > 0) {
    shiny::showNotification(
      sprintf("Saved %s, but %d item(s) still need attention.", basename(path), n_problems),
      type = "warning", duration = 8
    )
  } else {
    shiny::showNotification(paste("Saved", basename(path)), type = "message")
  }
  invisible(path)
}

# TRUE for the app exactly as it starts, which is not worth autosaving. The
# version carries a default the author never typed, so it does not count.
# Components are classed lists whose unset fields are NULL, so c() drops them.
sap_is_empty <- function(sap) {
  s <- sap$study %||% list()
  authored <- c(s$study_id, s$title, unlist(s$authors), s$description)
  !any(nzchar(as.character(authored[!is.na(authored)]))) &&
    !length(sap$data_sources) && !length(sap$data_source_modifications) &&
    !length(sap$codelists) && !length(sap$cohorts) && !length(sap$analyses)
}

# A vector, or NULL when it is empty -- so an empty picker is omitted from the SAP.
vec_or_null <- function(x) if (length(x)) x else NULL
