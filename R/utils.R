# Small helpers shared across the app --------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

# A textarea holds one item per line; JSON holds an array.
split_lines <- function(x) {
  if (is.null(x)) return(character(0))
  v <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  v[nzchar(v)]
}

join_lines <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = "\n")
}

# I() keeps jsonlite from unboxing a length-1 vector, so these fields are
# always arrays in the JSON regardless of how many entries the user typed.
as_array <- function(x) I(as.character(x))

# Blank text becomes null rather than "" so consumers can test for absence.
blank_to_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) NA_character_ else x
}

slugify <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) "sap" else substr(x, 1, 60)
}

# Builds the value-lookup used by the *_item_ui() functions when restoring a
# previously saved SAP: pf("cdm_table") returns the stored value or a default.
prefiller <- function(prefill) {
  function(key, default = "") {
    v <- prefill[[key]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) default else v
  }
}

sap_json <- function(sap) {
  jsonlite::toJSON(sap, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
}

save_sap <- function(sap, dir = "output") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, sprintf(
    "sap-%s-%s.json",
    slugify(sap$study$title),
    format(Sys.time(), "%Y%m%d-%H%M%S")
  ))
  writeLines(sap_json(sap), path)
  path
}

read_sap <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}
