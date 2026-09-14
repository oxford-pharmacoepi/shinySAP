# SAP -> runnable R ------------------------------------------------------------
#
# The code a SAP describes, argument for argument, from its own parameters:
#
#   codelist            omopgenerics::import*() from the files the study export
#                       writes beside the script (see sap_study_export.R)
#   concept_cohort      CohortConstructor::conceptCohort()
#   denominator         IncidencePrevalence::generateDenominatorCohortSet()
#   target_denominator  IncidencePrevalence::generateTargetDenominatorCohortSet()
#   target              nothing: instantiated outside this script
#   incidence / point_prevalence / period_prevalence
#                       IncidencePrevalence::estimate*()
#   single_event_survival / competing_risk_survival
#                       CohortSurvival::estimate*Survival()
#   other               nothing
#
# Rules: arguments in the estimator's own order; an argument the author never
# decided is OMITTED rather than defaulted, so the package's documented default
# applies and the plan does not claim a choice nobody made. References are by
# id in the SAP and by TABLE NAME in the code: the table name is a slug of the
# cohort's name (readable for the reviewer who signs the plan), and the id never
# appears. Two names slugging to one table is reported (table_name_collisions).
#
# No Shiny: the preview document sources this file directly. Depends on utils.R,
# cohort_kinds.R and problems.R, all called inside functions only.

# R literals -------------------------------------------------------------------

r_string <- function(x) sprintf('"%s"', gsub('"', '\\\\"', as.character(x)))

r_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || is.na(x)) return("NA")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

r_logical <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"

# A character vector: one value bare, several wrapped in c(). NULL for empty, so
# r_call() drops the argument entirely.
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

# age_group and time_at_risk are lists of pairs. One pair renders bare -- which is
# how both signatures write their own defaults, c(0, Inf) and list(c(0, 150)) --
# and several as a list().
r_bound_list <- function(pairs, always_list = FALSE) {
  rendered <- Filter(Negate(is.null), lapply(pairs %||% list(), r_bounds))
  if (!length(rendered)) return(NULL)
  if (length(rendered) == 1 && !always_list) return(rendered[[1]])
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

# A date_range: two Dates, NA for an open bound -- which is what the signature's
# own default, as.Date(c(NA, NA)), is made of. NULL when nothing was decided.
r_date_range <- function(dr) {
  if (is.null(dr) || length(dr) < 2) return(NULL)
  dr <- as.Date(dr)
  if (all(is.na(dr))) return(NULL)
  bound <- function(i) if (is.na(dr[[i]])) "NA" else r_string(format(dr[[i]], "%Y-%m-%d"))
  sprintf("as.Date(c(%s, %s))", bound(1), bound(2))
}

# strata is a list of variable GROUPS: list("sex", c("age_group", "sex")) means
# one stratification by sex and another by their cross.
r_strata <- function(groups) {
  rendered <- Filter(Negate(is.null), lapply(groups %||% list(), r_chr_vec))
  if (!length(rendered)) return(NULL)
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

# Split "a, c(0, 9), c(10, 19)" on its TOP-LEVEL commas only.
split_top_level <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0
  out   <- character(0)
  cur   <- character(0)
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- depth - 1
    if (ch == "," && depth == 0) {
      out <- c(out, paste(cur, collapse = ""))
      cur <- character(0)
    } else {
      cur <- c(cur, ch)
    }
  }
  trimws(c(out, paste(cur, collapse = "")))
}

# A long list() or c() argument broken across lines and aligned under its opening
# paren, so the emitted script stays readable at the width a page assumes.
r_wrap_value <- function(value, prefix, width = 92) {
  head <- if (grepl("^list\\(", value)) "list(" else if (grepl("^c\\(", value)) "c(" else NULL
  if (is.null(head) || nchar(prefix) + nchar(value) <= width) return(value)
  inner <- substr(value, nchar(head) + 1L, nchar(value) - 1L)
  parts <- split_top_level(inner)
  if (length(parts) < 2) return(value)
  pad <- strrep(" ", nchar(prefix) + nchar(head))
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

# fn(\n  arg = value,\n  ...\n). Arguments that rendered NULL drop out here.
r_call <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  pad <- max(nchar(names(args)))
  lines <- vapply(seq_along(args), function(i) {
    prefix <- sprintf("  %-*s = ", pad, names(args)[[i]])
    paste0(prefix, r_wrap_value(args[[i]], prefix))
  }, character(1))
  paste0(fn, "(\n", paste(lines, collapse = ",\n"), "\n)")
}

indent_block <- function(code) paste0("  ", gsub("\n", "\n  ", code))

# Names -------------------------------------------------------------------------

# A display name -> the identifier the calls use: a CDM table name for a cohort,
# a variable name for a codelist. Deterministic, so the name a generator call
# creates is the same string the estimator call points at.
cohort_table_name <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")[1]))
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) return(NA_character_)
  if (grepl("^[0-9]", s)) s <- paste0("c_", s)
  # 63 is Postgres's identifier limit, the tightest of the databases the CDM
  # runs on. Two long names can still truncate onto each other, which is why the
  # collision is reported rather than silently generated.
  substr(s, 1, 63)
}

# The table a cohort item becomes; its id when it has no usable name.
item_table_name <- function(item) {
  tbl <- cohort_table_name(item$name)
  if (is.na(tbl)) as.character(item$id %||% "unnamed") else tbl
}

codelist_var <- function(cl) item_table_name(cl)

# Two cohorts whose names collapse to ONE table name: the generated script would
# create the table twice and every estimate on the first would silently read the
# second. Returns problem messages (see semantic_problems()).
table_name_collisions <- function(cohorts) {
  cohorts <- Filter(function(co) !is.null(co$name), cohorts %||% list())
  if (!length(cohorts)) return(character(0))
  tbl <- vapply(cohorts, item_table_name, character(1))
  nms <- vapply(cohorts, function(co) as.character(co$name), character(1))
  vapply(unique(tbl[duplicated(tbl)]), function(t) sprintf(
    "'%s' all map onto the table name '%s' in the generated code; rename one so each cohort gets its own table.",
    paste(nms[tbl == t], collapse = "', '"), t), character(1))
}

# Lookups ------------------------------------------------------------------------

cohort_by_id <- function(sap, id) sap_item_by_id(sap$cohorts, id)

# The table a cohort id refers to, as an R string; NULL when the id resolves to
# nothing (checkSap() reports the dangling reference).
cohort_table <- function(sap, id) {
  co <- cohort_by_id(sap, id)
  if (is.null(co)) NULL else r_string(item_table_name(co))
}

# Data source ids -> names, the strings omopgenerics::cdmName(cdm) is compared to.
data_source_names <- function(sap, ids) {
  vapply(chr_vec(ids), function(id) {
    ds <- sap_item_by_id(sap$data_sources, id)
    nm <- as.character(ds$name %||% "")
    if (nzchar(nm)) nm else id
  }, character(1), USE.NAMES = FALSE)
}

sap_source_names <- function(sap) data_source_names(sap, vapply(
  sap$data_sources %||% list(), function(ds) as.character(ds$id), character(1)))

# Data sources as the generated script's own guard -------------------------------
#
# `data_source_id` is the SAP-level counterpart of `cdm`: WHICH databases a
# cohort is built in and an estimate runs against. A study package ships ONE
# script to every partner and each runs it against their own CDM, so the
# restriction is a run-time test on omopgenerics::cdmName(cdm). The guard fires
# only for an item naming a PROPER SUBSET of the study's sources.
is_source_restricted <- function(sap, item) {
  own <- data_source_names(sap, item$data_source_id)
  all <- sap_source_names(sap)
  length(own) > 0 && length(all) > 0 && !setequal(own, all)
}

# Always c(), even for one source: `%in% c("SIDIAP")` reads as the membership
# test it is.
r_chr_c <- function(x) sprintf("c(%s)", paste(vapply(
  as.character(unlist(x)), r_string, character(1)), collapse = ", "))

# A side-effecting block (a cohort, a denominator set) under its guard.
source_guard <- function(sap, code, item) {
  if (is.null(code) || !is_source_restricted(sap, item)) return(code)
  sprintf("if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n}",
          r_chr_c(data_source_names(sap, item$data_source_id)), indent_block(code))
}

# An ESTIMATE under its guard, which needs the other branch: every variable the
# script binds has to EXIST at every partner, so the excluded branch yields an
# empty summarised_result that binds with the rest and contributes no rows.
source_guard_estimate <- function(sap, var, code, item) {
  if (!is_source_restricted(sap, item)) return(sprintf("%s <- %s", var, code))
  sprintf(paste0("%s <- if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n} else {\n",
                 "  # Not run here: this SAP restricts the analysis to the databases above.\n",
                 "  omopgenerics::emptySummarisedResult()\n}"),
          var, r_chr_c(data_source_names(sap, item$data_source_id)), indent_block(code))
}

# Codelists ----------------------------------------------------------------------

# A one-line description of a content object, for the card and the preview.
codelist_content_summary <- function(content) {
  if (is.null(content)) return("No codes uploaded yet.")
  sets <- names(content)
  n <- vapply(unclass(content), function(x) if (is.data.frame(x)) nrow(x) else length(x), integer(1))
  sprintf("%d concept set%s, %d concept%s: %s",
          length(sets), if (length(sets) == 1) "" else "s",
          sum(n), if (sum(n) == 1) "" else "s",
          paste(sprintf("%s (%d)", sets, n), collapse = ", "))
}

# The omopgenerics reader and writer for a codelist type.
codelist_io <- function(type) {
  switch(as.character(type %||% ""),
         codelist = c(import = "importCodelist", export = "exportCodelist"),
         codelist_with_details = c(import = "importCodelistWithDetails", export = "exportCodelistWithDetails"),
         concept_set_expression = c(import = "importConceptSetExpression", export = "exportConceptSetExpression"),
         NULL)
}

# Where the study export writes a codelist's files, relative to the study root.
codelist_dir <- function(cl) file.path("codelist", codelist_var(cl))

# The content lives in the SAP; the study export writes it to files beside the
# script (sap_study_export.R) and the script reads them back, so the codes a
# reviewer signed are the codes that run.
codelist_r_code <- function(cl) {
  io <- codelist_io(cl$type)
  if (is.null(io)) return(NULL)
  sprintf('%s <- omopgenerics::%s(path = here::here("%s"), type = "json")',
          codelist_var(cl), io[["import"]], codelist_dir(cl))
}

# The codelists any concept cohort actually uses, in codelist order.
used_codelists <- function(sap) {
  used <- unlist(lapply(sap$cohorts %||% list(), function(co) {
    if (identical(co$type, "concept_cohort")) chr_vec(co$parameters$codelist_id) else character(0)
  }))
  Filter(function(cl) as.character(cl$id %||% "") %in% used, sap$codelists %||% list())
}

# Cohorts ------------------------------------------------------------------------

# The call a cohort describes: list(code, pkgs), or NULL for a type that is
# instantiated outside this script.
cohort_r_code <- function(sap, co) {
  type <- as.character(co$type %||% "")
  p <- co$parameters %||% list()
  tbl <- item_table_name(co)

  if (identical(type, "concept_cohort")) {
    cl <- sap_item_by_id(sap$codelists, p$codelist_id)
    call <- r_call("CohortConstructor::conceptCohort", list(
      cdm        = "cdm",
      conceptSet = if (is.null(cl)) NULL else codelist_var(cl),
      name       = r_string(tbl),
      exit       = r_chr_vec(p$exit),
      overlap    = r_chr_vec(p$overlap)
    ))
    todo <- if (is.null(cl)) "# TODO: this cohort names no codelist that exists in the SAP.\n" else ""
    return(list(code = sprintf("%scdm$%s <- %s", todo, tbl, call), pkgs = "CohortConstructor"))
  }

  if (!is_denominator_type(type)) return(NULL)
  target <- identical(type, "target_denominator")
  args <- list(cdm = "cdm", name = r_string(tbl))
  todo <- ""
  if (target) {
    ids <- chr_vec(p$target_cohort_id)
    args$targetCohortTable <- if (length(ids)) cohort_table(sap, ids[[1]]) else NULL
    if (length(ids) > 1) {
      todo <- "# TODO: several target cohorts are named; the generator takes one table.\n"
    }
    args$cohortDateRange <- r_date_range(p$target_cohort_date_range)
    args$timeAtRisk      <- r_bound_list(p$time_at_risk)
  }
  # ageGroup is always a list, even at length one: its default is list(c(0, 150)).
  args$ageGroup             <- r_bound_list(p$age_group, always_list = TRUE)
  args$sex                  <- r_chr_vec(p$sex)
  args$daysPriorObservation <- r_num_vec(p$days_prior_observation)
  if (target && !is.null(p$requirements_at_entry)) {
    args$requirementsAtEntry <- r_logical(p$requirements_at_entry)
  }
  if (!is.null(p$requirement_interactions)) {
    args$requirementInteractions <- r_logical(p$requirement_interactions)
  }
  call <- r_call(
    if (target) "generateTargetDenominatorCohortSet" else "generateDenominatorCohortSet", args)
  # `cdm <- `: the generators return a CDM REFERENCE with the new table attached.
  list(code = sprintf("%scdm <- %s", todo, call), pkgs = "IncidencePrevalence")
}

# Analyses ---------------------------------------------------------------------

# Per analysis type: the estimator, its package, and the schema parameter ->
# argument name map in the estimator's own order. Two schema keys map onto one
# CohortSurvival argument (censorOnDate takes a Date OR a column name); whichever
# is set is rendered.
ANALYSIS_ARGS <- list(
  incidence = list(
    fn = "estimateIncidence", package = "IncidencePrevalence",
    args = c(denominator_cohort_id = "denominatorTable", outcome_cohort_id = "outcomeTable",
             censor_cohort_id = "censorTable", denominator_cohort_ids_of_interest = "denominatorCohortId",
             outcome_cohort_ids_of_interest = "outcomeCohortId", interval = "interval",
             complete_database_intervals = "completeDatabaseIntervals", washout = "outcomeWashout",
             repeated_events = "repeatedEvents", strata = "strata",
             include_overall_strata = "includeOverallStrata")),
  point_prevalence = list(
    fn = "estimatePointPrevalence", package = "IncidencePrevalence",
    args = c(denominator_cohort_id = "denominatorTable", outcome_cohort_id = "outcomeTable",
             denominator_cohort_ids_of_interest = "denominatorCohortId",
             outcome_cohort_ids_of_interest = "outcomeCohortId", interval = "interval",
             time_point = "timePoint", strata = "strata", include_overall_strata = "includeOverallStrata")),
  period_prevalence = list(
    fn = "estimatePeriodPrevalence", package = "IncidencePrevalence",
    args = c(denominator_cohort_id = "denominatorTable", outcome_cohort_id = "outcomeTable",
             denominator_cohort_ids_of_interest = "denominatorCohortId",
             outcome_cohort_ids_of_interest = "outcomeCohortId", interval = "interval",
             complete_database_intervals = "completeDatabaseIntervals",
             full_contribution = "fullContribution", level = "level", strata = "strata",
             include_overall_strata = "includeOverallStrata")),
  single_event_survival = list(
    fn = "estimateSingleEventSurvival", package = "CohortSurvival",
    args = c(target_cohort_id = "targetCohortTable", outcome_cohort_id = "outcomeCohortTable",
             outcome_date_variable = "outcomeDateVariable", outcome_washout = "outcomeWashout",
             censor_on_cohort_exit = "censorOnCohortExit", censor_on_date = "censorOnDate",
             censor_on_date_variable = "censorOnDate", weight = "weight",
             follow_up_days = "followUpDays", strata = "strata", event_gap = "eventGap",
             estimate_gap = "estimateGap", restricted_mean_follow_up = "restrictedMeanFollowUp",
             minimum_survival_days = "minimumSurvivalDays")),
  competing_risk_survival = list(
    fn = "estimateCompetingRiskSurvival", package = "CohortSurvival",
    args = c(target_cohort_id = "targetCohortTable", outcome_cohort_id = "outcomeCohortTable",
             competing_outcome_cohort_id = "competingOutcomeCohortTable",
             outcome_date_variable = "outcomeDateVariable", outcome_washout = "outcomeWashout",
             competing_outcome_date_variable = "competingOutcomeDateVariable",
             competing_outcome_washout = "competingOutcomeWashout",
             censor_on_cohort_exit = "censorOnCohortExit", censor_on_date = "censorOnDate",
             censor_on_date_variable = "censorOnDate", weight = "weight",
             follow_up_days = "followUpDays", strata = "strata", event_gap = "eventGap",
             estimate_gap = "estimateGap", restricted_mean_follow_up = "restrictedMeanFollowUp",
             minimum_survival_days = "minimumSurvivalDays"))
)

# One parameter as R, by its schema value type. A cohort id becomes the table it
# refers to; ids of interest stay numbers; everything else is the literal.
render_parameter <- function(sap, key, value, value_type) {
  if (is.null(value)) return(NULL)
  if (grepl("_cohort_id$", key)) return(cohort_table(sap, value))
  if (grepl("_ids_of_interest$", key)) return(r_num_vec(value))
  switch(as.character(value_type),
         logic = r_logical(value),
         integer = r_number(value),
         date = r_date(value),
         date_range = r_date_range(value),
         strata = r_strata(value),
         age_group = , time_at_risk = r_bound_list(value),
         character_vector = , id_vector = r_chr_vec(value),
         r_chr_vec(value))
}

# The estimator call an analysis describes: list(code, pkgs), or NULL when its
# type maps onto none.
analysis_r_code <- function(sap, a) {
  spec <- ANALYSIS_ARGS[[as.character(a$type %||% "")]]
  if (is.null(spec)) return(NULL)
  p <- a$parameters %||% list()
  fields <- shinySAP::sapSchemaFields("analysis", a$type)
  value_type <- stats::setNames(fields$value_type, sub("^parameters\\.", "", fields$path))

  args <- list(cdm = "cdm")
  for (key in names(spec$args)) {
    arg <- spec$args[[key]]
    if (!is.null(args[[arg]])) next     # censor_on_date already rendered
    rendered <- render_parameter(sap, key, p[[key]], value_type[[key]] %||% "character")
    if (!is.null(rendered)) args[[arg]] <- rendered
  }
  # Inert without strata -- the estimators then return only the overall estimate
  # -- so it is stated only where it does something.
  if (is.null(args$strata)) args$includeOverallStrata <- NULL
  list(code = r_call(spec$fn, args), pkgs = spec$package)
}

# A unique R variable name per analysis, so the estimates can be bound together
# at the end. Derived from the analysis name for readability; the index keeps it
# unique when two analyses share a name.
estimate_var_names <- function(analyses) {
  vapply(seq_along(analyses), function(i) {
    slug <- cohort_table_name(analyses[[i]]$name)
    if (is.na(slug)) return(sprintf("estimate_%d", i))
    slug <- sub("_+$", "", substr(slug, 1, 32))
    if (!nzchar(slug)) sprintf("estimate_%d", i) else sprintf("%s_%d", slug, i)
  }, character(1))
}

# The script -------------------------------------------------------------------
#
# The whole SAP as ordered, labelled BLOCKS: codelists, then the cohorts built
# from them, then the denominator sets generated from those, then the estimates,
# then one suppression step. sap_r_script() joins them into one runnable
# listing; the document's appendix renders the same blocks under headings.
#
# One entry per block: group, title (NA for a whole-plan step), code (NULL when
# the SAP asks for something with no package call), note (why, when code is
# NULL), pkgs (the libraries the block's calls come from).
sap_script_packages <- function(secs) {
  unique(unlist(lapply(secs, function(s) as.character(s$pkgs %||% character(0)))))
}

sap_script_sections <- function(sap) {
  cohorts  <- sap$cohorts %||% list()
  analyses <- sap$analyses %||% list()
  acc <- new.env(parent = emptyenv())
  acc$out <- list()
  add <- function(group, title, code = NULL, note = NULL, pkgs = character(0)) {
    acc$out <- c(acc$out, list(list(group = group, title = title, code = code,
                                    note = note, pkgs = pkgs)))
  }
  label <- function(item, noun) item_label(item, noun)

  for (cl in used_codelists(sap)) {
    add("Codelists", label(cl, "codelist"), code = codelist_r_code(cl))
  }

  # Concept cohorts first, then the denominators generated from them.
  for (pass in c("concept", "denominator")) {
    for (co in cohorts) {
      is_den <- is_denominator_type(co$type)
      if ((pass == "concept") == is_den) next
      block <- cohort_r_code(sap, co)
      if (is.null(block)) next
      add(if (is_den) "Denominator cohort sets" else "Cohorts", label(co, "cohort"),
          code = source_guard(sap, block$code, co), pkgs = block$pkgs)
    }
  }

  vars  <- estimate_var_names(analyses)
  bound <- character(0)
  for (i in seq_along(analyses)) {
    a     <- analyses[[i]]
    block <- analysis_r_code(sap, a)
    if (is.null(block)) {
      add("Estimates", label(a, "analysis"), note = sprintf(
        "No estimator maps onto analysis type '%s'.", as.character(a$type %||% "none")))
    } else {
      bound <- c(bound, vars[[i]])
      add("Estimates", label(a, "analysis"),
          code = source_guard_estimate(sap, vars[[i]], block$code, a), pkgs = block$pkgs)
    }
  }

  supp <- suppression_r_code(bound)
  if (!is.null(supp)) add("Result suppression", NA_character_, code = supp)

  pkgs <- sap_script_packages(acc$out)
  if (length(pkgs)) {
    acc$out <- c(list(list(group = "Libraries", title = NA_character_,
                           code  = paste(sprintf("library(%s)", pkgs), collapse = "\n"),
                           note  = NULL, pkgs = character(0))),
                 acc$out)
  }
  acc$out
}

# The blocks above as one runnable script, each under its group banner.
sap_r_script <- function(sap) {
  secs <- sap_script_sections(sap)
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

# Every estimate bound and suppressed once before anything leaves the data
# partner. The schema carries no threshold, so the call states omopgenerics'
# own default and says so: the data partners' governance decides the number.
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
