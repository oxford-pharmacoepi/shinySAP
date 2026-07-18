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
# These fields used to be free-text, so an older file may hold something a
# calendar cannot show ("Q1 2024", "unknown"). dateInput() would warn and render
# blank anyway; doing the coercion here makes that explicit and keeps the console
# quiet. A date the picker cannot represent is not one this app can capture.
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

# First non-empty key, so a renamed section can still read older SAP files.
coalesce_key <- function(x, ...) {
  for (key in c(...)) {
    v <- x[[key]]
    if (!is.null(v) && length(v) > 0) return(v)
  }
  list()
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
  authored <- c(s$title, s$study_code, unlist(s$authors), s$background, s$aim,
                unlist(s$objectives))
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
# The shapes codelist tools actually produce: a CSV with a concept_id column
# (CodelistGenerator, Atlas exports), a text file with one code per line, or
# JSON -- a plain array of codes, an array of {concept_id/code, concept_name/
# name} objects, or an Atlas concept-set expression ({items: [{concept: ...}]}).
# Returns a list of list(code, name?) with codes as CHARACTER -- concept ids can
# exceed integer range, and a code is an identifier, not a quantity. Stops with
# a plain message on anything unreadable; the caller shows it as a notification.
read_codelist <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  nms <- NULL
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
    if (!is.null(x$items)) x <- lapply(x$items, function(it) it$concept %||% it)
    if (!length(x)) stop("the file holds no codes")
    one <- function(el, key) {
      v <- el[[key[1]]]
      for (k in key[-1]) v <- v %||% el[[k]]
      v
    }
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
  if (is.null(nms)) nms <- rep(NA_character_, length(codes))
  keep  <- !is.na(codes) & nzchar(codes)
  codes <- codes[keep]
  nms   <- nms[keep]
  if (!length(codes)) stop("no codes found in the file")
  unname(Map(function(cd, nm) {
    out <- list(code = cd)
    if (!is.na(nm) && nzchar(nm)) out$name <- nm
    out
  }, codes, nms))
}

# Cross-section migrations, run once on a loaded SAP before any section sees it.
#
# A template's flatten() can only rewrite its own analysis, so anything that has
# to move *between* sections has to happen here -- and before cohorts$load(), or
# the cohort cards are already built by the time the analysis is read.
#
# Before 0.3.2 an analysis named a plain target cohort as its denominator and
# carried its own time at risk. Neither is how IncidencePrevalence works: a
# denominator is a cohort *set* produced by generateTargetDenominatorCohortSet(),
# and timeAtRisk is one of that generator's arguments.
#
# So the missing denominator is synthesised here -- one per (target cohort, time
# at risk) pair, since that is exactly what one generator call produces -- and the
# analysis is repointed at it. Doing it in the template's flatten() is not
# possible: an analysis can only rewrite itself, not add a cohort.
#
# The old {start_offset_days, start_anchor, end_offset_days, end_anchor} becomes
# a single [[start, end]] interval. The anchors are dropped: the API has nowhere
# to put them, because both bounds are relative to target cohort entry.
migrate_sap <- function(sap) {
  # 0.4.10 renamed study$acronym: what authors put there is a study code.
  if (!is.null(sap$study)) {
    sap$study$study_code <- sap$study$study_code %||% sap$study$acronym
    sap$study$acronym    <- NULL
  }
  sap$cdm_changes <- lapply(sap$cdm_changes %||% list(), migrate_cdm_change)
  analyses <- coalesce_key(sap, "proposed_analyses", "analyses")
  cohorts  <- lapply(sap$cohorts %||% list(), migrate_cohort)
  if (!length(analyses)) {
    sap$cohorts <- cohorts
    return(sap)
  }

  index_of <- function(nm) match(as.character(nm), vapply(
    cohorts, function(x) as.character(x$name %||% ""), character(1)))

  for (k in seq_along(analyses)) {
    a     <- analyses[[k]]
    flat  <- is.null(a$parameters)
    p     <- if (flat) a else a$parameters
    # Pre-0.3.0 the generic form called the denominator `target_cohort`.
    nm    <- p$denominator_cohort %||% p$target_cohort
    if (is.null(nm) || !nzchar(as.character(nm))) next

    i <- index_of(nm)
    # Names a cohort nobody defined, or one that is already a denominator: leave it.
    if (is.na(i) || is_denominator_kind(cohorts[[i]]$kind)) next

    tar <- migrate_time_at_risk(p$time_at_risk)
    den <- synthetic_denominator_name(as.character(nm), tar)

    if (is.na(index_of(den))) {
      cohorts[[length(cohorts) + 1]] <- list(
        name                = den,
        kind                = "target_denominator",
        targetCohortTable   = as.character(nm),
        timeAtRisk          = tar,
        requirementsAtEntry = TRUE
      )
    }

    p$denominator_cohort <- den
    p$time_at_risk       <- NULL
    if (flat) analyses[[k]] <- p else analyses[[k]]$parameters <- p
  }

  sap$cohorts <- cohorts
  # coalesce_key() may have read these from the pre-0.2.0 `analyses` key.
  sap$proposed_analyses <- analyses
  sap$analyses <- NULL
  sap
}

# Before 0.4.4 a CDM change named one `data_source`; now it holds a
# `data_sources` array. The dropped `cdm_version` is discarded.
#
# 0.4.5 retyped the section around the questions a SAP asks of the CDM
# (validations / alterations / person cleaning). Every pre-0.4.5 type described
# an alteration -- so any type the current list no longer carries lands on the
# catch-all -- and the dropped cdm_table/cdm_field pair folds into the
# description so nothing the author wrote is lost.
migrate_cdm_change <- function(ch) {
  if (is.null(ch$data_sources)) {
    old <- as.character(ch$data_source %||% character(0))
    ch$data_sources <- as_array(old[!is.na(old) & nzchar(old)])
  }
  ch$data_source <- NULL
  ch$cdm_version <- NULL

  type <- as.character(ch$change_type %||% "")
  if (!is.na(type) && nzchar(type) && !(type %in% CDM_CHANGE_TYPES)) {
    ch$change_type <- "Other database-specific alteration"
  }

  tf <- as.character(c(ch$cdm_table, ch$cdm_field))
  tf <- paste(tf[!is.na(tf) & nzchar(tf)], collapse = ".")
  if (nzchar(tf)) {
    desc <- as.character(ch$description %||% "")
    if (is.na(desc)) desc <- ""
    ch$description <- if (nzchar(desc)) paste0(tf, ": ", desc) else tf
  }
  ch$cdm_table <- NULL
  ch$cdm_field <- NULL

  # 0.4.7 dropped the rationale; an old one folds into the description.
  rat <- as.character(ch$rationale %||% "")
  if (!is.na(rat) && nzchar(trimws(rat))) {
    desc <- as.character(ch$description %||% "")
    if (is.na(desc)) desc <- ""
    ch$description <- if (nzchar(desc)) {
      paste0(desc, " Rationale: ", rat)
    } else {
      paste0("Rationale: ", rat)
    }
  }
  ch$rationale <- NULL
  ch
}

# One generator call per (target, time at risk), so the name has to carry both.
synthetic_denominator_name <- function(target, tar) {
  win <- paste(format_bound_list(tar), collapse = "; ")
  sprintf("%s denominator (%s days)", target, win)
}

# The anchored shape -> a list holding one [start, end] interval. Already-migrated
# cohorts hold a list of pairs, which has no $start_offset_days and is left alone.
migrate_time_at_risk <- function(tar) {
  if (is.null(tar)) return(list(as_num_array(c(0, Inf))))   # the API's default
  if (is.list(tar) && !is.null(tar$start_offset_days)) {
    return(list(as_num_array(c(
      as.numeric(tar$start_offset_days %||% 0),
      as.numeric(tar$end_offset_days   %||% Inf)
    ))))
  }
  tar
}

# Every key a denominator kind carries is now the generator argument's own name, so
# a cohort read from an older file has to be renamed onto them here -- before
# cohorts$load(), which builds the cards from these keys.
#
# Old keys are read with [[, never $: on a file that lacks one, $ partial-matches a
# longer name (`cohort_date_range` would find `cohort_date_range_start`, and
# `requirement` is a prefix of two different arguments) and would migrate the wrong
# value.
migrate_cohort <- function(ch) {
  ch$kind <- canonical_cohort_kind(ch$kind %||% ch$role)
  ch$role <- NULL

  # 0.4.2 renamed these onto generate(Target)DenominatorCohortSet()'s argument names.
  renames <- c(
    target_cohort            = "targetCohortTable",
    time_at_risk             = "timeAtRisk",
    requirements_at_entry    = "requirementsAtEntry",
    age_groups               = "ageGroup",
    days_prior_observation   = "daysPriorObservation",
    requirement_interactions = "requirementInteractions"
  )
  for (old in names(renames)) {
    new <- renames[[old]]
    if (is.null(ch[[new]]) && !is.null(ch[[old]])) ch[[new]] <- ch[[old]]
    ch[[old]] <- NULL
  }

  if (!is.null(ch$timeAtRisk)) ch$timeAtRisk <- migrate_time_at_risk(ch$timeAtRisk)

  # Age groups were free text ("18-64"); the API wants numeric pairs.
  ages <- ch$ageGroup
  if (length(ages) && all(vapply(ages, function(a) length(a) == 1 && is.character(a[[1]]),
                                 logical(1)))) {
    ch$ageGroup <- parse_bound_list(join_lines(ages))
  }

  # Neither generator takes a washout: it is estimateIncidence(outcomeWashout =),
  # which the Incidence analysis now captures. There is nowhere to move a cohort
  # washout to, so it is dropped rather than quietly misapplied.
  ch$washout_days <- NULL

  # cohortDateRange is ONE argument taking two dates. 0.3.1 called it the "study
  # period" and 0.3.2 split it across two keys; both become the pair.
  if (is.null(ch[["cohortDateRange"]])) {
    start <- ch[["cohort_date_range_start"]] %||% ch[["study_period_start"]]
    end   <- ch[["cohort_date_range_end"]]   %||% ch[["study_period_end"]]
    if (!is.null(start) || !is.null(end)) ch$cohortDateRange <- cohort_date_range(start, end)
  }
  ch$cohort_date_range_start <- NULL
  ch$cohort_date_range_end   <- NULL
  ch$study_period_start      <- NULL
  ch$study_period_end        <- NULL

  # daysPriorObservation may be a vector; the old field was a single number.
  if (is.null(ch$daysPriorObservation) && !is.null(ch[["prior_observation_days"]])) {
    ch$daysPriorObservation <- as_num_array(ch[["prior_observation_days"]])
  }
  ch$prior_observation_days <- NULL

  # 0.4.1 dropped the declared strata columns. generateDenominatorCohortSet()
  # produces age_group and sex and nothing else, so the field could only ever agree
  # with STRATA_VARIABLES or be wrong. An older file that named an extra ETL column
  # loses it -- and any analysis stratified by that column now fails validation,
  # which is the honest outcome: the generator was never going to produce it.
  ch$strata_variables <- NULL

  # 0.4.12 dropped the cohort description: the kind block already says what a
  # cohort is in structured form. An older file's description does not survive.
  ch$description <- NULL

  # 0.4.15 folded concept_set and index_rule back into the text fields: the
  # codelist is cited inline in the entry event that uses it, and the index
  # rule is an inclusion criterion. Nothing the author wrote is lost.
  cs <- as.character(ch$concept_set %||% "")[1]
  if (!is.na(cs) && nzchar(cs)) {
    ch$entry_events <- as_array(c(unlist(ch$entry_events), paste("Codelist:", cs)))
  }
  ch$concept_set <- NULL
  ir <- as.character(ch$index_rule %||% "")[1]
  if (!is.na(ir) && nzchar(ir)) {
    ch$inclusion_criteria <- as_array(c(unlist(ch$inclusion_criteria),
                                        paste("Index date:", ir)))
  }
  ch$index_rule <- NULL

  ch
}
