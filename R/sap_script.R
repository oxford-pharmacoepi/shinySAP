# SAP -> runnable R -------------------------------------------------------
#
# A SAP describes analyses argument-by-argument: which codelists to load,
# which cohorts to build, which estimators to run, and with what parameters.
# This file turns that description into R code -- either a single standalone
# script or the three files an OmopStudyBuilder study directory expects.
#
# The mapping from SAP parameter names to estimator argument names, and the
# Darwin EU package each type belongs to, now live in the schema CSVs:
#   data-raw/v0.1.0/types.csv   -- object/type_id → package, fn
#   data-raw/v0.1.0/schema.csv  -- parameters.* path → arg (estimator arg name)
# Adding a new analysis type now only requires new rows in those files.
#
# Rules:
#   * Arguments the author never decided are OMITTED, not defaulted. The
#     documented package default applies; the plan does not claim a choice
#     nobody made.
#   * References are by id in the SAP and by TABLE NAME in the generated
#     code; the slug is deterministic so the name a generator call creates
#     is the same string the estimator call points at.
#   * Two cohort names slugging to one table name is reported as a problem
#     (sapTableNameCollisions()) rather than silently mangled.
#   * The library() calls at the top of a standalone script come from the
#     `package` column; a Study Builder script omits them (codeToRun.R
#     loads everything).

# R literals ----------------------------------------------------------------

r_string <- function(x) sprintf('"%s"', gsub('"', '\\\\"', as.character(x)))

r_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || is.na(x)) return("NA")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

r_logical <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"

# A character vector: one value bare, several wrapped in c(). NULL for empty,
# so r_call() drops the argument entirely.
r_chr_vec <- function(x) {
  v <- as.character(unlist(x %||% character(0)))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NULL)
  if (length(v) == 1) return(r_string(v))
  sprintf("c(%s)", paste(vapply(v, r_string, character(1)), collapse = ", "))
}

r_num_vec <- function(x) {
  v <- suppressWarnings(as.numeric(unlist(x %||% numeric(0))))
  v <- v[!is.na(v)]
  if (!length(v)) return(NULL)
  if (length(v) == 1) return(r_number(v))
  sprintf("c(%s)", paste(vapply(v, r_number, character(1)), collapse = ", "))
}

r_date <- function(x) {
  if (is.null(x) || is.na(x)) return(NULL)
  sprintf('as.Date("%s")', format(as.Date(x), "%Y-%m-%d"))
}

# One [lower, upper] pair as c(lo, hi).
r_bounds <- function(pair) {
  pair <- as.numeric(unlist(pair))
  if (length(pair) < 2) return(NULL)
  sprintf("c(%s, %s)", r_number(pair[[1]]), r_number(pair[[2]]))
}

# age_group and time_at_risk are lists of pairs. One pair renders bare -- which
# is how both signatures write their own defaults -- and several as a list().
r_bound_list <- function(pairs, always_list = FALSE) {
  rendered <- Filter(Negate(is.null), lapply(pairs %||% list(), r_bounds))
  if (!length(rendered)) return(NULL)
  if (length(rendered) == 1 && !always_list) return(rendered[[1]])
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

# A date_range: two Dates, NA for an open bound. NULL when nothing was decided.
r_date_range <- function(dr) {
  if (is.null(dr) || length(dr) < 2) return(NULL)
  dr <- as.Date(dr)
  if (all(is.na(dr))) return(NULL)
  bound <- function(i) if (is.na(dr[[i]])) "NA" else r_string(format(dr[[i]], "%Y-%m-%d"))
  sprintf("as.Date(c(%s, %s))", bound(1), bound(2))
}

# strata is a list of variable GROUPS.
r_strata <- function(groups) {
  rendered <- Filter(Negate(is.null), lapply(groups %||% list(), r_chr_vec))
  if (!length(rendered)) return(NULL)
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

# Split "a, c(0, 9), c(10, 19)" on its TOP-LEVEL commas only.
split_top_level <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0L
  out   <- character(0)
  cur   <- character(0)
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") depth <- depth - 1L
    if (ch == "," && depth == 0L) {
      out <- c(out, paste(cur, collapse = ""))
      cur <- character(0)
    } else {
      cur <- c(cur, ch)
    }
  }
  trimws(c(out, paste(cur, collapse = "")))
}

# A long list() or c() broken across lines and aligned under its opening paren.
r_wrap_value <- function(value, prefix, width = 92) {
  head <- if (grepl("^list\\(", value)) "list(" else if (grepl("^c\\(", value)) "c(" else NULL
  if (is.null(head) || nchar(prefix) + nchar(value) <= width) return(value)
  inner <- substr(value, nchar(head) + 1L, nchar(value) - 1L)
  parts <- split_top_level(inner)
  if (length(parts) < 2) return(value)
  pad   <- strrep(" ", nchar(prefix) + nchar(head))
  wrapped <- character(0)
  line    <- head
  first   <- TRUE
  for (i in seq_along(parts)) {
    piece <- paste0(parts[[i]], if (i < length(parts)) "," else "")
    open  <- grepl("\\($", line)
    used  <- nchar(line) + if (first) nchar(prefix) else 0L
    sep   <- if (open) "" else " "
    if (!open && used + nchar(sep) + nchar(piece) > width) {
      wrapped <- c(wrapped, line)
      line    <- paste0(pad, piece)
      first   <- FALSE
    } else {
      line <- paste0(line, sep, piece)
    }
  }
  paste0(paste(c(wrapped, line), collapse = "\n"), ")")
}

# fn(\n  arg = value,\n  ...\n). Arguments that rendered NULL drop out.
r_call <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  pad  <- max(nchar(names(args)))
  lines <- vapply(seq_along(args), function(i) {
    prefix <- sprintf("  %-*s = ", pad, names(args)[[i]])
    paste0(prefix, r_wrap_value(args[[i]], prefix))
  }, character(1))
  paste0(fn, "(\n", paste(lines, collapse = ",\n"), "\n)")
}

indent_block <- function(code) paste0("  ", gsub("\n", "\n  ", code))

# Names -----------------------------------------------------------------------

#' Convert a cohort display name to its generated table identifier
#'
#' @param x A character string; a display name.
#' @return A valid, lowercase identifier at most 63 characters long, or
#'   `NA_character_` when `x` cannot be made into one.
#' @export
cohortTableName <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")[1]))
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) return(NA_character_)
  if (grepl("^[0-9]", s)) s <- paste0("c_", s)
  # 63 is Postgres's identifier limit, the tightest of the databases the CDM
  # runs on. Two long names can still truncate onto each other -- see
  # sapTableNameCollisions().
  substr(s, 1, 63)
}

# The table a cohort item becomes; its id when it has no usable name.
item_table_name <- function(item) {
  tbl <- cohortTableName(item$name)
  if (is.na(tbl)) as.character(item$id %||% "unnamed") else tbl
}

codelist_var <- function(cl) item_table_name(cl)

#' Detect cohorts whose names collide on the same table identifier
#'
#' @param cohorts A list of cohort objects (from a SAP's `$cohorts`).
#' @return A character vector of problem messages; empty when no collision exists.
#' @export
sapTableNameCollisions <- function(cohorts) {
  cohorts <- Filter(function(co) !is.null(co$name), cohorts %||% list())
  if (!length(cohorts)) return(character(0))
  tbl <- vapply(cohorts, item_table_name, character(1))
  nms <- vapply(cohorts, function(co) as.character(co$name), character(1))
  vapply(unique(tbl[duplicated(tbl)]), function(t) sprintf(
    "'%s' all map onto the table name '%s' in the generated code; rename one so each cohort gets its own table.",
    paste(nms[tbl == t], collapse = "', '"), t), character(1))
}

# SAP item lookups -----------------------------------------------------------

#' Find an item in a SAP collection by its id
#'
#' @param items A SAP collection list (e.g. `sap$cohorts`).
#' @param id The id to look up.
#' @return The matching item, or `NULL` when not found.
#' @export
sapItemById <- function(items, id) {
  id <- as.character(id %||% "")[1]
  if (is.na(id) || !nzchar(id)) return(NULL)
  for (item in items %||% list()) {
    if (identical(as.character(item$id %||% ""), id)) return(item)
  }
  NULL
}

#' Display label for a SAP item
#'
#' @param item A SAP item (cohort, analysis, etc.).
#' @param noun Fallback noun, e.g. `"cohort"`.
#' @return The item's `$name`, or `"Untitled <noun>"` when unnamed.
#' @export
sapItemLabel <- function(item, noun) {
  nm <- as.character(item$name %||% "")
  if (nzchar(nm)) nm else sprintf("Untitled %s", noun)
}

#' Test whether a cohort type is denominator-like
#'
#' @param type A cohort type id string.
#' @return `TRUE` for `"denominator"` and `"target_denominator"`, `FALSE` otherwise.
#' @export
sapIsDenominatorType <- function(type) {
  as.character(type %||% "") %in% c("denominator", "target_denominator")
}

# Data source guards ---------------------------------------------------------
#
# `data_source_id` is the SAP-level counterpart of `cdm`: WHICH databases a
# cohort is built in and an estimate runs against. A study package ships ONE
# script to every partner and each runs it against their own CDM, so the
# restriction is a run-time test on omopgenerics::cdmName(cdm). The guard
# fires only when an item names a PROPER SUBSET of the study's sources.

# Data source ids -> names.
data_source_names <- function(sap, ids) {
  vapply(chr_vec(ids), function(id) {
    ds <- sapItemById(sap$data_sources, id)
    nm <- as.character(ds$name %||% "")
    if (nzchar(nm)) nm else id
  }, character(1), USE.NAMES = FALSE)
}

sap_source_names <- function(sap) {
  data_source_names(sap, vapply(
    sap$data_sources %||% list(), function(ds) as.character(ds$id), character(1)))
}

is_source_restricted <- function(sap, item) {
  own <- data_source_names(sap, item$data_source_id)
  all <- sap_source_names(sap)
  length(own) > 0 && length(all) > 0 && !setequal(own, all)
}

r_chr_c <- function(x) sprintf("c(%s)", paste(vapply(
  as.character(unlist(x)), r_string, character(1)), collapse = ", "))

source_guard <- function(sap, code, item) {
  if (is.null(code) || !is_source_restricted(sap, item)) return(code)
  sprintf("if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n}",
          r_chr_c(data_source_names(sap, item$data_source_id)), indent_block(code))
}

source_guard_estimate <- function(sap, var, code, item) {
  if (!is_source_restricted(sap, item)) return(sprintf("%s <- %s", var, code))
  sprintf(paste0("%s <- if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n} else {\n",
                 "  # Not run here: this SAP restricts the analysis to the databases above.\n",
                 "  omopgenerics::emptySummarisedResult()\n}"),
          var, r_chr_c(data_source_names(sap, item$data_source_id)), indent_block(code))
}

# chr_vec from utils.R may not be loaded when R/ functions run (package context);
# define a local version for package-internal use.
chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  v <- as.character(unlist(x))
  v[!is.na(v) & nzchar(v)]
}

# Type specification lookup --------------------------------------------------

# fn/package for an object+type_id from the baked sapTypes data.
script_type_spec <- function(object, type_id) {
  version <- currentSapSchemaVersion()
  types <- sapTypes[[version]]
  if (is.null(types)) return(NULL)
  row <- types[types$object == object & types$type_id == type_id, , drop = FALSE]
  if (!nrow(row)) return(NULL)
  fn  <- as.character(row$fn[[1]])
  pkg <- as.character(row$package[[1]])
  if (is.na(fn) || is.na(pkg)) return(NULL)
  list(fn = fn, package = pkg)
}

# Generic parameter renderer -------------------------------------------------

# Render one SAP parameter as R source text, dispatching on value_type and ref.
# `ref` tells us what collection an id field points to:
#   "cohorts"   → look up table name (the generated identifier)
#   "codelists" → look up codelist variable name
#
# _ids_of_interest fields (naming convention) are numeric column indices, not
# SAP id references; they stay as r_num_vec() regardless of value_type.
render_parameter <- function(sap, key, value, value_type, ref) {
  if (is.null(value)) return(NULL)
  vt <- as.character(value_type %||% "character")
  rf <- as.character(ref %||% "")

  if (identical(vt, "id")) {
    if (identical(rf, "cohorts")) {
      co <- sapItemById(sap$cohorts, value)
      return(if (is.null(co)) NULL else r_string(item_table_name(co)))
    }
    if (identical(rf, "codelists")) {
      cl <- sapItemById(sap$codelists, value)
      return(if (is.null(cl)) NULL else codelist_var(cl))
    }
  }
  # id_vector + ref="cohorts": take the first id as a table name (e.g.
  # target_denominator.target_cohort_id). A multi-id SAP field maps to a
  # single-table function argument; the caller emits a TODO when length > 1.
  if (identical(vt, "id_vector") && identical(rf, "cohorts")) {
    ids <- chr_vec(value)
    if (!length(ids)) return(NULL)
    co <- sapItemById(sap$cohorts, ids[[1]])
    return(if (is.null(co)) NULL else r_string(item_table_name(co)))
  }
  # Numeric indices into the denominator table (not SAP ids).
  if (grepl("_ids_of_interest$", key)) return(r_num_vec(value))
  switch(vt,
    logic            = r_logical(value),
    integer          = r_number(value),
    date             = r_date(value),
    date_range       = r_date_range(value),
    strata           = r_strata(value),
    age_group =, # nolint: infix_spaces_linter.
    time_at_risk = r_bound_list(value),
    character_vector =, # nolint: infix_spaces_linter.
    id_vector = r_chr_vec(value),
    r_chr_vec(value)
  )
}

# Codelists ------------------------------------------------------------------

# The omopgenerics reader/writer for a codelist type.
codelist_io <- function(type) {
  switch(as.character(type %||% ""),
    codelist              = c(import = "importCodelist",               export = "exportCodelist"),
    codelist_with_details = c(import = "importCodelistWithDetails",    export = "exportCodelistWithDetails"),
    concept_set_expression = c(import = "importConceptSetExpression",  export = "exportConceptSetExpression"),
    NULL
  )
}

codelist_dir <- function(cl) file.path("codelist", codelist_var(cl))

codelist_r_code <- function(cl) {
  io <- codelist_io(cl$type)
  if (is.null(io)) return(NULL)
  sprintf('%s <- omopgenerics::%s(path = here::here("%s"), type = "json")',
          codelist_var(cl), io[["import"]], codelist_dir(cl))
}

used_codelists <- function(sap) {
  used <- unlist(lapply(sap$cohorts %||% list(), function(co) {
    if (identical(co$type, "concept_cohort")) chr_vec(co$parameters$codelist_id) else character(0)
  }))
  Filter(function(cl) as.character(cl$id %||% "") %in% used, sap$codelists %||% list())
}

# Cohorts --------------------------------------------------------------------

# One cohort as list(code, pkgs), or NULL for types with no estimator call.
cohort_r_code <- function(sap, co) {
  type <- as.character(co$type %||% "")
  spec <- script_type_spec("cohort", type)
  if (is.null(spec)) return(NULL)

  p   <- co$parameters %||% list()
  tbl <- item_table_name(co)

  # Generic arg rendering from the schema's arg column.
  fields      <- sapSchemaFields("cohort", type)
  param_fields <- fields[grepl("^parameters\\.", fields$path) & !is.na(fields$arg), , drop = FALSE]
  param_keys  <- sub("^parameters\\.", "", param_fields$path)

  # concept_cohort uses CohortConstructor::conceptCohort() with cdm$<tbl> <- .
  if (identical(type, "concept_cohort")) {
    args <- list(cdm = "cdm", conceptSet = NULL, name = r_string(tbl))
    todo <- ""
    for (i in seq_len(nrow(param_fields))) {
      key <- param_keys[[i]]
      arg <- param_fields$arg[[i]]
      if (identical(arg, "conceptSet") && is.null(p[[key]])) {
        todo <- "# TODO: this cohort names no codelist that exists in the SAP.\n"
      }
      rendered <- render_parameter(sap, key, p[[key]], param_fields$value_type[[i]], param_fields$ref[[i]])
      if (!is.null(rendered)) args[[arg]] <- rendered
    }
    fn   <- paste0(spec$package, "::", spec$fn)
    call <- r_call(fn, args)
    return(list(code = sprintf("%scdm$%s <- %s", todo, tbl, call), pkgs = spec$package))
  }

  # Denominator types: cdm <- generateDenominatorCohortSet(cdm, name = ..., ...).
  # ageGroup is always a list, even at length one: its default is list(c(0, 150)).
  args <- list(cdm = "cdm", name = r_string(tbl))
  todo <- ""
  for (i in seq_len(nrow(param_fields))) {
    key <- param_keys[[i]]
    arg <- param_fields$arg[[i]]
    vt  <- param_fields$value_type[[i]]
    val <- p[[key]]
    # target_cohort_id is stored as id_vector to support multi-select in the UI,
    # but generateTargetDenominatorCohortSet() takes a single table; the first
    # entry wins and we warn when more were named.
    if (identical(arg, "targetCohortTable") && length(chr_vec(val)) > 1) {
      todo <- "# TODO: several target cohorts are named; the generator takes one table.\n"
    }
    rendered <- if (identical(vt, "age_group")) {
      r_bound_list(val, always_list = TRUE)
    } else {
      render_parameter(sap, key, val, vt, param_fields$ref[[i]])
    }
    if (!is.null(rendered)) args[[arg]] <- rendered
  }
  list(code = sprintf("%scdm <- %s", todo, r_call(spec$fn, args)), pkgs = spec$package)
}

# Analyses -------------------------------------------------------------------

# The estimator call an analysis describes: list(code, pkgs), or NULL when
# the type has no package/fn mapping in types.csv.
analysis_r_code <- function(sap, a) {
  type <- as.character(a$type %||% "")
  spec <- script_type_spec("analysis", type)
  if (is.null(spec)) return(NULL)

  p           <- a$parameters %||% list()
  fields      <- sapSchemaFields("analysis", type)
  param_fields <- fields[grepl("^parameters\\.", fields$path) & !is.na(fields$arg), , drop = FALSE]
  param_keys  <- sub("^parameters\\.", "", param_fields$path)

  args <- list(cdm = "cdm")
  for (i in seq_len(nrow(param_fields))) {
    key <- param_keys[[i]]
    arg <- param_fields$arg[[i]]
    if (!is.null(args[[arg]])) next   # censor_on_date / censor_on_date_variable alias
    rendered <- render_parameter(sap, key, p[[key]], param_fields$value_type[[i]], param_fields$ref[[i]])
    if (!is.null(rendered)) args[[arg]] <- rendered
  }
  # includeOverallStrata is inert without strata; omit it when strata is not set.
  if (is.null(args$strata)) args$includeOverallStrata <- NULL

  list(code = r_call(spec$fn, args), pkgs = spec$package)
}

# A unique R variable name per analysis.
estimate_var_names <- function(analyses) {
  vapply(seq_along(analyses), function(i) {
    slug <- cohortTableName(analyses[[i]]$name)
    if (is.na(slug)) return(sprintf("estimate_%d", i))
    slug <- sub("_+$", "", substr(slug, 1, 32))
    if (!nzchar(slug)) sprintf("estimate_%d", i) else sprintf("%s_%d", slug, i)
  }, character(1))
}

# A one-line description of a codelist's content.
codelist_content_summary <- function(content) {
  if (is.null(content)) return("No codes uploaded yet.")
  sets <- names(content)
  n <- vapply(unclass(content), function(x) if (is.data.frame(x)) nrow(x) else length(x), integer(1))
  sprintf("%d concept set%s, %d concept%s: %s",
          length(sets), if (length(sets) == 1) "" else "s",
          sum(n), if (sum(n) == 1) "" else "s",
          paste(sprintf("%s (%d)", sets, n), collapse = ", "))
}

# Script assembly ------------------------------------------------------------

script_packages <- function(secs) {
  unique(unlist(lapply(secs, function(s) as.character(s$pkgs %||% character(0)))))
}

#' Generate the labelled code blocks for a SAP
#'
#' The blocks are ordered: Libraries, Codelists, Cohorts, Denominator cohort
#' sets, Estimates, Result suppression. Each entry is a list with fields
#' `group`, `title`, `code`, `note`, and `pkgs`.
#'
#' @param sap A SAP object from [readSap()] or the CRUD constructors.
#' @return A list of block entries.
#' @export
sapScriptSections <- function(sap) {
  cohorts  <- sap$cohorts %||% list()
  analyses <- sap$analyses %||% list()
  acc <- new.env(parent = emptyenv())
  acc$out <- list()
  add <- function(group, title, code = NULL, note = NULL, pkgs = character(0)) {
    acc$out <- c(acc$out, list(list(group = group, title = title,
                                   code = code, note = note, pkgs = pkgs)))
  }
  lbl <- function(item, noun) sapItemLabel(item, noun)

  for (cl in used_codelists(sap)) {
    add("Codelists", lbl(cl, "codelist"), code = codelist_r_code(cl))
  }

  for (pass in c("concept", "denominator")) {
    for (co in cohorts) {
      is_den <- sapIsDenominatorType(co$type)
      if ((pass == "concept") == is_den) next
      block <- cohort_r_code(sap, co)
      if (is.null(block)) next
      add(if (is_den) "Denominator cohort sets" else "Cohorts", lbl(co, "cohort"),
          code = source_guard(sap, block$code, co), pkgs = block$pkgs)
    }
  }

  vars  <- estimate_var_names(analyses)
  bound <- character(0)
  for (i in seq_along(analyses)) {
    a     <- analyses[[i]]
    block <- analysis_r_code(sap, a)
    if (is.null(block)) {
      add("Estimates", lbl(a, "analysis"), note = sprintf(
        "No estimator maps onto analysis type '%s'.", as.character(a$type %||% "none")))
    } else {
      bound <- c(bound, vars[[i]])
      add("Estimates", lbl(a, "analysis"),
          code = source_guard_estimate(sap, vars[[i]], block$code, a), pkgs = block$pkgs)
    }
  }

  supp <- suppression_r_code(bound)
  if (!is.null(supp)) add("Result suppression", NA_character_, code = supp)

  pkgs <- script_packages(acc$out)
  if (length(pkgs)) {
    acc$out <- c(list(list(group = "Libraries", title = NA_character_,
                           code  = paste(sprintf("library(%s)", pkgs), collapse = "\n"),
                           note  = NULL, pkgs = character(0))),
                 acc$out)
  }
  acc$out
}

#' Render a SAP as a single runnable R script
#'
#' @param sap A SAP object from [readSap()] or the CRUD constructors.
#' @return A single character string containing a runnable R script, or `""`
#'   when the SAP has no codable content.
#' @export
sapToScript <- function(sap) {
  secs <- sapScriptSections(sap)
  if (!length(secs)) return("")
  out   <- character(0)
  group <- NULL
  for (s in secs) {
    if (!identical(s$group, group)) {
      group <- s$group
      out   <- c(out, sprintf("# --- %s ---", group))
    }
    lab  <- if (is.na(s$title)) character(0) else sprintf("# %s", s$title)
    body <- if (is.null(s$code)) sprintf("#   %s", s$note) else s$code
    out  <- c(out, paste(c(lab, body), collapse = "\n"))
  }
  paste(out, collapse = "\n\n")
}

suppression_r_code <- function(vars) {
  if (!length(vars)) return(NULL)
  combine <- if (length(vars) == 1) vars[[1]] else
    sprintf("omopgenerics::bind(\n%s\n)", paste(sprintf("  %s", vars), collapse = ",\n"))
  paste0(
    "# Applied once, to every result above, before anything leaves the data partner.\n",
    "# 5 is omopgenerics' default minimum cell count; confirm the threshold the data\n",
    "# partners require.\n",
    sprintf("results <- %s\n", combine),
    "results <- omopgenerics::suppress(results, minCellCount = 5)"
  )
}

# OmopStudyBuilder study directory -------------------------------------------
#
# OmopStudyBuilder::initStudy() lays out a study and leaves the analysis files
# empty: codelist/codelistCreation.R, cohorts/instantiateCohorts.R, and
# analyses/estimates.R. Those files are exactly what a SAP decides.
#
#   The harness owns: codeToRun.R and runStudy.R. Both carry site-specific
#     credentials; this never writes them.
#   The SAP owns: codelists, cohorts, estimates.
#
# Differences from the standalone script:
#   - No library() header (codeToRun.R loads everything).
#   - No suppression block (runStudy.R applies the threshold once on export).
#   - Estimates accumulate in `results[[...]]` for runStudy.R to bind.
#   - Every concept cohort gets CohortConstructor::addCohortTableIndex().

STUDY_PATHS <- list(
  codelists = "codelist/codelistCreation.R",
  cohorts   = "cohorts/instantiateCohorts.R",
  analyses  = "analyses/estimates.R"
)

study_file_header <- function(sap, what) {
  study <- sap$study %||% list()
  sprintf(paste0(
    "# %s\n",
    "# Generated by shinySAP from the statistical analysis plan.\n",
    "#   Study:   %s\n",
    "#   Version: %s (SAP schema %s)\n",
    "# Edit the SAP and regenerate rather than editing this file: the plan a\n",
    "# reviewer signed and the code that runs are meant to be the same object.\n"),
    what,
    as.character(study$study_id %||% study$title %||% "unnamed"),
    as.character(study$version %||% "unversioned"),
    as.character(sap$sap_schema_version %||% "unknown"))
}

study_codelists_r <- function(sap) {
  cls <- used_codelists(sap)
  if (!length(cls)) return("# No codelists: no concept cohort in this SAP uses one yet.\n")
  blocks <- vapply(cls, function(cl) {
    sprintf("# %s\n%s", sapItemLabel(cl, "codelist"),
            codelist_r_code(cl) %||% "# (unknown codelist type)")
  }, character(1))
  paste0(paste(blocks, collapse = "\n\n"), "\n")
}

study_cohorts_r <- function(sap) {
  blocks <- Filter(Negate(is.null), lapply(sap$cohorts %||% list(), function(co) {
    if (!identical(co$type, "concept_cohort")) return(NULL)
    block <- cohort_r_code(sap, co)
    tbl <- item_table_name(co)
    # Index every concept cohort table on (subject_id, cohort_start_date).
    idx <- sprintf("\ncdm$%s <- cdm$%s |> CohortConstructor::addCohortTableIndex()", tbl, tbl)
    sprintf("# %s\n%s", sapItemLabel(co, "cohort"),
            source_guard(sap, paste0(block$code, idx), co))
  }))
  if (!length(blocks)) {
    return("# No concept cohorts: every cohort in this SAP is instantiated elsewhere.\n")
  }
  paste0(paste(unlist(blocks), collapse = "\n\n"), "\n")
}

study_analyses_r <- function(sap) {
  cohorts  <- sap$cohorts %||% list()
  analyses <- sap$analyses %||% list()
  out <- character(0)

  dens <- Filter(Negate(is.null), lapply(cohorts, function(co) {
    if (!sapIsDenominatorType(co$type)) return(NULL)
    block <- cohort_r_code(sap, co)
    sprintf("# %s\n%s", sapItemLabel(co, "cohort"),
            source_guard(sap, block$code, co))
  }))
  if (length(dens)) {
    out <- c(out, paste0("# Denominator cohort sets ----\n\n",
                         paste(unlist(dens), collapse = "\n\n")))
  }

  vars <- estimate_var_names(analyses)
  est <- lapply(seq_along(analyses), function(i) {
    a     <- analyses[[i]]
    block <- analysis_r_code(sap, a)
    if (is.null(block)) {
      return(sprintf("# %s\n#   No estimator maps onto analysis type '%s'.",
                     sapItemLabel(a, "analysis"), as.character(a$type %||% "none")))
    }
    sprintf("# %s\n%s", sapItemLabel(a, "analysis"),
            source_guard_estimate(sap, sprintf('results[["%s"]]', vars[[i]]),
                                  block$code, a))
  })
  if (length(est)) {
    out <- c(out, paste0("# Estimates ----\n\n",
                         paste(unlist(est), collapse = "\n\n")))
  }

  if (!length(out)) return("# No denominator cohort sets or estimates yet.\n")
  paste0(paste(out, collapse = "\n\n"), "\n")
}

#' Notes about what the SAP decides that lives outside the generated files
#'
#' @param sap A SAP object.
#' @return A single character string.
#' @export
sapStudyExportNotes <- function(sap) {
  notes <- paste("Results are suppressed by runStudy.R on export; the SAP states no",
                 "threshold, so confirm the minimum cell count the data partners require.")
  restricted <- Filter(function(x) is_source_restricted(sap, x),
                       c(sap$cohorts %||% list(), sap$analyses %||% list()))
  if (length(restricted)) {
    notes <- c(notes, sprintf(paste(
      "%d cohorts or analyses are restricted to named databases, so the script",
      "guards them with omopgenerics::cdmName(cdm). Check that codeToRun.R",
      "creates the cdm reference with the same names this SAP uses (%s),",
      "or the guard will never be true and those steps will silently not run."),
      length(restricted), paste(sap_source_names(sap), collapse = ", ")))
  }
  paste(notes, collapse = " ")
}

# Internal: the three study scripts as a named list of path -> content.
study_files_list <- function(sap) {
  stats::setNames(
    list(
      paste0(study_file_header(sap, "Codelists"),    "\n", study_codelists_r(sap)),
      paste0(study_file_header(sap, "Study cohorts"), "\n", study_cohorts_r(sap)),
      paste0(study_file_header(sap, "Estimates"),    "\n", study_analyses_r(sap))
    ),
    c(STUDY_PATHS$codelists, STUDY_PATHS$cohorts, STUDY_PATHS$analyses)
  )
}

#' Write an OmopStudyBuilder study directory from a SAP
#'
#' Fills `codelist/codelistCreation.R`, `cohorts/instantiateCohorts.R`, and
#' `analyses/estimates.R` under `dir`, creating subdirectories as needed.
#' Also writes the codelist JSON files that the generated R code reads back.
#' Never removes or overwrites files the study harness owns (`codeToRun.R`,
#' `runStudy.R`).
#'
#' @param sap A SAP object from [readSap()] or the CRUD constructors.
#' @param dir The study root directory (must exist).
#' @return The paths written, invisibly.
#' @export
writeSapStudy <- function(sap, dir) {
  files <- study_files_list(sap)
  paths <- character(0)
  for (rel in names(files)) {
    path <- file.path(dir, rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(sub("\n+$", "", files[[rel]]), path)
    paths <- c(paths, path)
  }
  for (cl in used_codelists(sap)) {
    io <- codelist_io(cl$type)
    if (is.null(io) || is.null(cl$content)) next
    target <- file.path(dir, codelist_dir(cl))
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    getExportedValue("omopgenerics", io[["export"]])(cl$content, path = target, type = "json")
    paths <- c(paths, list.files(target, full.names = TRUE))
  }
  invisible(paths)
}
