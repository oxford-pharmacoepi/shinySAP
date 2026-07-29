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

# Same, for a field that must stay numeric -- an age group or a time-at-risk
# bound. Under na = "null" jsonlite writes Inf as null, so c(0, Inf) becomes
# [0, null]: an unbounded upper bound. Reading it back gives list(0, NULL), so
# index such a pair with [[1]]/[[2]] and never unlist() it, or the null collapses
# and a two-element pair silently becomes one.
as_num_array <- function(x) I(as.numeric(x))

# Blank text becomes null rather than "" so consumers can test for absence. A
# cleared dateInput sends NA rather than "", and lands here too -- same answer.
blank_to_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) NA_character_ else x
}

# A dateInput's `value`: a Date, or NULL to start the field genuinely blank.
#
# A hand-edited file may hold something a calendar cannot show ("Q1 2024",
# "unknown"). dateInput() would warn and render blank anyway; coercing here makes
# that explicit and keeps the console quiet. A date the picker cannot represent
# is not one this app can capture.
as_date_value <- function(x) {
  x <- trimws(as.character(x %||% "")[1])
  if (is.na(x) || !nzchar(x)) return(NULL)
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (is.na(d)) NULL else d
}

# A dateInput that genuinely starts blank -- which shiny::dateInput() will not do.
#
# Its JS binding substitutes TODAY whenever the field has no `data-initial-date`:
#
#   let date = $input.data("initial-date");
#   if (date === void 0 || date === null) date = ...new Date();   -- shiny.js
#
# Blank is a *meaningful* value here: an unset cohort date range means "the whole
# observation period", and an unset data lock point means the author has not said.
# A silently pre-filled today would decide both on their behalf -- the same trap
# the outcome washout's empty first choice exists to avoid. An empty string is
# neither undefined nor null, so the picker starts empty and stays that way.
#
# A blank field then needs to say what typing into it looks like, and
# dateInput() takes no placeholder either -- so that attribute goes on the same
# way. It shows the FORMAT, not what blank means: blank is meaningful per field,
# so each call site says so in its own help text.
date_input <- function(inputId, label, value = NULL, # nolint: object_name_linter.
                       placeholder = "YYYY-MM-DD", ...) {
  value <- as_date_value(value)
  di <- dateInput(inputId, label, value = value, width = "100%", ...)
  tq <- htmltools::tagQuery(di)$find("input")$addAttrs(placeholder = placeholder)
  if (is.null(value)) tq <- tq$addAttrs("data-initial-date" = "")
  tq$allTags()
}

slugify <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  substr(x, 1, 60)
}

# The base name every saved or downloaded SAP file shares: sap-<study>[-v<version>].
# The study is the study code when there is one (short and already unique), else
# the title. A leading "sap" in either is dropped -- the name already starts sap-,
# and a title of "SAP" alone must not become "sap-sap". No title at all is "untitled".
sap_file_base <- function(study) {
  stem <- slugify(study$study_code %||% study$title)
  stem <- sub("^sap(-|$)", "", stem)
  if (!nzchar(stem)) stem <- "untitled"
  ver <- gsub("[^A-Za-z0-9._-]+", "", as.character(study$version %||% ""))
  if (nzchar(ver)) sprintf("sap-%s-v%s", stem, ver) else sprintf("sap-%s", stem)
}

# The version a new amendment proposes: the current SAP version with the major
# bumped (1 -> 2, 1.0 -> 2.0, 1.2 -> 2.0). A version that does not start with a
# number gives no prefill -- better empty than a guess the author must delete.
next_sap_version <- function(v) {
  v <- trimws(as.character(v %||% ""))
  m <- regmatches(v, regexpr("^\\d+", v))
  if (!length(m)) return("")
  major <- as.integer(m) + 1
  if (grepl("^\\d+\\.", v)) sprintf("%d.0", major) else as.character(major)
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

# A SAP lives in ONE file. There are no timestamped copies and no separate
# autosave file: the working file is created the first time there is anything
# to keep, and every write after that -- a clicked Save or an autosave --
# rewrites it in place. Loading a SAP adopts the loaded file as the working
# file (see app.R). Versioning is git's job, or the author's, not the app's.
working_sap_path <- function(study, dir = "output") {
  file.path(dir, sprintf("%s.json", sap_file_base(study)))
}

write_sap <- function(sap, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(sap_json(sap), path)
  path
}

# One deliberate save, with its guard and its notifications. Returns the path,
# or NULL when nothing was saved. Must run inside a Shiny session
# (showNotification).
save_working <- function(sap, path, n_problems = 0) {
  if (is.na(sap$study$title %||% NA)) {
    showNotification("Give the study a title before saving.", type = "warning")
    return(invisible(NULL))
  }
  path <- write_sap(sap, path)
  if (n_problems > 0) {
    showNotification(
      sprintf("Saved %s, but %d item(s) still need attention.", basename(path), n_problems),
      type = "warning", duration = 8
    )
  } else {
    showNotification(paste("Saved", basename(path)), type = "message")
  }
  invisible(path)
}

# TRUE for the app exactly as it starts, which is not worth autosaving -- an
# empty file would overwrite nothing useful, but it would appear in output/
# every time someone opens the app and closes it again. version and date carry
# defaults the author never typed, so they do not count as content.
sap_is_empty <- function(sap) {
  s <- sap$study %||% list()
  # The protocol block is always written (four keys, null until filled), so it is
  # its VALUES that count as content -- naming the protocol and nothing else is a
  # real start on a plan, and one worth keeping.
  authored <- c(s$title, s$study_code, unlist(s$authors), s$background, s$aim,
                unlist(s$objectives), unlist(s$protocol))
  authored <- authored[!is.na(authored)]
  !any(nzchar(as.character(authored))) &&
    !length(s$amendments) &&
    !length(sap$cdm_sources) && !length(sap$cdm_changes) &&
    !length(sap$codelists) &&
    !length(sap$cohorts) && !length(sap$proposed_analyses)
}

read_sap <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

# Codelist uploads --------------------------------------------------------------
#
# A concept set EXPRESSION and a codelist are different objects, and the SAP
# carries both because they answer different questions.
#
# The expression is the SPECIFICATION: per concept, whether it is excluded,
# whether its descendants come with it, whether mapped concepts do. It is small,
# stable, and vocabulary-independent. The codelist is what that expression
# BECOMES when applied to one vocabulary version -- so it is an output, and a
# different data partner on a different vocabulary resolves the same expression
# to a different list. A study running in five countries that shipped resolved
# codes would be imposing the authoring machine's vocabulary on all five.
#
# So the expression is canonical -- it is what the study resolves at run time --
# and `codes` is a resolved SNAPSHOT, kept because a reviewer signing a plan has
# to see concepts, not a rule. The snapshot is never an input to anything.
#
# The field names are omopgenerics' own (concept_id / excluded / descendants /
# mapped), so the SAP speaks the standard rather than a private dialect and the
# generated study code can hand the expression straight to
# omopgenerics::newConceptSetExpression() without a translation step.
CONCEPT_SET_DEFAULTS <- list(excluded = FALSE, descendants = FALSE, mapped = FALSE)

# The shapes codelist tools actually produce: a CSV with a concept_id column
# (CodelistGenerator, Atlas exports), a text file with one code per line, or
# JSON -- a plain array of codes, an array of {concept_id/code, concept_name/
# name} objects, or an Atlas concept-set expression ({items: [{concept: ...}]}).
#
# Returns list(expression =, codes =). Codes are CHARACTER throughout -- concept
# ids can exceed integer range, and a code is an identifier, not a quantity.
# Stops with a plain message on anything unreadable; the caller shows it as a
# notification.
read_concept_set <- function(path, filename = path) {
  ext   <- tolower(tools::file_ext(filename))
  nms   <- NULL
  flags <- NULL
  if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(df)) stop("the file has no rows")
    cols   <- tolower(names(df))
    id_col <- which(cols %in% c("concept_id", "conceptid", "code", "codes", "id"))[1]
    if (is.na(id_col)) id_col <- 1
    nm_col <- which(cols %in% c("concept_name", "conceptname", "name", "description"))[1]
    codes  <- as.character(df[[id_col]])
    if (!is.na(nm_col)) nms <- as.character(df[[nm_col]])
  } else if (ext == "json") {
    x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    one <- function(el, key) {
      v <- el[[key[1]]]
      for (k in key[-1]) v <- v %||% el[[k]]
      v
    }
    # An Atlas export nests its items under `items`, or one level further down
    # under `expression`. THE FLAGS ARE THE POINT: the previous reader kept only
    # `concept` and dropped isExcluded / includeDescendants / includeMapped, so
    # an expression covering a whole SNOMED subtree arrived as the single seed
    # concept and the SAP silently understated its own codelist.
    items <- x$items %||% x$expression$items
    if (!is.null(items)) {
      flags <- lapply(items, function(it) list(
        excluded    = isTRUE(one(it, c("isExcluded", "IS_EXCLUDED"))),
        descendants = isTRUE(one(it, c("includeDescendants", "INCLUDE_DESCENDANTS"))),
        mapped      = isTRUE(one(it, c("includeMapped", "INCLUDE_MAPPED")))))
      x <- lapply(items, function(it) it$concept %||% it)
    }
    if (!length(x)) stop("the file holds no codes")
    codes <- vapply(x, function(el) {
      if (!is.list(el)) return(as.character(el))
      as.character(one(el, c("concept_id", "conceptId", "code", "CONCEPT_ID")) %||% NA)
    }, character(1))
    nms <- vapply(x, function(el) {
      if (!is.list(el)) return(NA_character_)
      as.character(one(el, c("concept_name", "conceptName", "name", "CONCEPT_NAME")) %||% NA)
    }, character(1))
  } else {   # txt, or anything line-based
    codes <- trimws(readLines(path, warn = FALSE))
  }
  if (is.null(nms))   nms   <- rep(NA_character_, length(codes))
  # A format carrying no flags is not silent about them: a bare list of concept
  # ids means exactly these concepts, nothing excluded and no descendants.
  if (is.null(flags)) flags <- rep(list(CONCEPT_SET_DEFAULTS), length(codes))
  keep  <- !is.na(codes) & nzchar(codes)
  codes <- codes[keep]
  nms   <- nms[keep]
  flags <- flags[keep]
  if (!length(codes)) stop("no codes found in the file")

  expression <- unname(Map(function(cd, fl) c(list(concept_id = cd), fl), codes, flags))
  # An EXCLUDED concept belongs in the expression and not in the snapshot: the
  # expression says "not this one", so a resolved list containing it would state
  # the opposite of what the author uploaded.
  keep_resolved <- !vapply(flags, function(fl) isTRUE(fl$excluded), logical(1))
  codes_out <- unname(Map(function(cd, nm) {
    out <- list(code = cd)
    if (!is.na(nm) && nzchar(nm)) out$name <- nm
    out
  }, codes[keep_resolved], nms[keep_resolved]))

  list(expression = expression, codes = codes_out)
}

# The resolved snapshot alone, for callers that only want the codes.
read_codelist <- function(path, filename = path) {
  read_concept_set(path, filename)$codes
}

# TRUE when the expression reaches beyond the concepts it names, so the snapshot
# is a SEED rather than the resolved list and the document has to say so.
concept_set_expands <- function(expression) {
  any(vapply(expression %||% list(),
             function(e) isTRUE(e$descendants) || isTRUE(e$mapped), logical(1)))
}

# Objectives --------------------------------------------------------------------
#
# An objective is what an analysis EXISTS FOR, so an analysis has to be able to
# point at one. That needs an identity, and until 0.4.22 the only identity an
# objective had was its position in the list -- which is exactly what the
# document prints as "1., 2., 3.". Position is the one identity that cannot be
# referenced safely: insert an objective at the top and every reference below it
# silently means something else.
#
# So an objective is {id, text}. The id is opaque and never shown; the document
# still numbers by position, because that is what a reader wants.
#
#   "To estimate the prevalence of X"  ->  {"id": "obj_1", "text": "To estimate ..."}
#
# The link is MANY-TO-MANY, not one-per-objective: one objective ("estimate the
# prevalence of follicular lymphoma") is typically served by several analyses
# (complete, 5-year and 2-year prevalence), and one analysis can serve several
# objectives. So an analysis carries a LIST of objective ids.

objective_text <- function(o) {
  v <- if (is.list(o)) o$text else o
  v <- as.character(v %||% "")[1]
  if (is.na(v)) "" else v
}

objective_id <- function(o) {
  v <- if (is.list(o)) as.character(o$id %||% "")[1] else NA_character_
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

# One past the highest obj_<n> in use -- NOT the lowest free one.
#
# Reuse is the hazard here. If a deleted objective's id were handed to the next
# new objective, an analysis still referencing the deleted one would silently
# start pointing at a different objective, which is precisely the silent repoint
# ids exist to prevent. Counting up leaves gaps, and gaps are harmless: the id is
# opaque and the document numbers by position anyway.
#
# The residual case this does not cover: delete the HIGHEST objective, save,
# reload, add a new one -- the high-water mark is gone with it, so the id can be
# reissued. Closing that needs a counter persisted in the SAP, which is not worth
# a field; in the meantime objective_coverage_problems() reports a reference to a
# missing objective, so the break is visible in the window where it matters.
next_objective_id <- function(taken) {
  taken <- as.character(taken %||% character(0))
  used  <- suppressWarnings(as.integer(sub("^obj_", "", grep("^obj_\\d+$", taken, value = TRUE))))
  used  <- used[!is.na(used)]
  sprintf("obj_%d", if (length(used)) max(used) + 1L else 1L)
}

# Objective texts (from the textarea) reconciled against the objectives already
# held, so an id survives everything except a rewording.
#
# Matched BY TEXT, and by position among equal texts, which is what makes
# reordering safe: the same sentence keeps its id wherever it moves to. Rewriting
# an objective's wording does mint a new id and orphan any analysis pointing at
# the old one -- deliberately, because a reworded objective may be a different
# objective, and objective_coverage_problems() surfaces the break rather than
# letting a stale link quietly persist.
reconcile_objectives <- function(texts, existing) {
  texts    <- trimws(as.character(texts %||% character(0)))
  texts    <- texts[nzchar(texts)]
  existing <- existing %||% list()
  taken    <- vapply(existing, function(o) objective_id(o) %||% "", character(1))
  taken    <- taken[!is.na(taken) & nzchar(taken)]
  unused   <- existing
  out <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    hit <- which(vapply(unused, function(o) identical(objective_text(o), texts[[i]]),
                        logical(1)))
    if (length(hit)) {
      out[[i]] <- list(id = objective_id(unused[[hit[[1]]]]), text = texts[[i]])
      unused[[hit[[1]]]] <- list(id = NA_character_, text = NA_character_)
    } else {
      id    <- next_objective_id(taken)
      taken <- c(taken, id)
      out[[i]] <- list(id = id, text = texts[[i]])
    }
  }
  out
}
