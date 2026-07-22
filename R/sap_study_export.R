# The SAP as an OmopStudyBuilder study directory -------------------------------
#
# OmopStudyBuilder::initStudy() lays out a study and leaves the analysis files
# EMPTY -- codelist/codelistCreation.R, cohorts/instantiateCohorts.R and
# analyses/*.R all ship with no content. Those empty files are exactly what a SAP
# decides, so this renders them.
#
# The split is deliberate and it is the whole point of the file:
#
#   the harness owns   codeToRun.R  (connection, schemas, library() calls,
#                                    min_cell_count) and runStudy.R (ordering,
#                                    logging, export). Both carry site-specific
#                                    credentials, so this NEVER writes them.
#   the SAP owns       the concept sets, the cohorts, and the estimates.
#
# Three of the harness's conventions differ from the standalone script the
# document's appendix shows, and following them is not optional:
#
#   1. NO library() header. codeToRun.R already loads CDMConnector, omopgenerics,
#      CohortConstructor, IncidencePrevalence and the rest, so a second set of
#      calls here would be noise.
#   2. NO suppression block. runStudy.R finishes with
#      exportSummarisedResult(results, minCellCount = min_cell_count), so the
#      threshold is applied once by the harness -- and the SAP's own
#      min_cell_count belongs in codeToRun.R, which is reported rather than
#      written (see study_export_notes()).
#   3. Estimates accumulate in `results[[...]]`, not in free variables, because
#      that list is what runStudy.R binds and exports.
#   4. Codelists are read with omopgenerics::importCodelist() and every cohort
#      gets CohortConstructor::addCohortTableIndex(). Both are what a real DARWIN
#      study does -- neither is a decision the PLAN makes, which is why the
#      document's appendix shows neither: an index is a performance step a
#      reviewer does not need to sign, and the appendix inlines its concept ids
#      because it is read on a page rather than sitting beside a CSV.
#
# Only the ASSIGNMENT differs between the two hosts. Every call still comes from
# cohort_operations_code() / cohort_r_code() / analysis_r_code(), so the study
# directory and the document appendix cannot describe different analyses.

# Where each artefact lands, relative to studyCode/. Named so a caller reads the
# layout rather than reconstructing it from paths.
STUDY_PATHS <- list(
  codelists = "codelist/codelistCreation.R",
  cohorts   = "cohorts/instantiateCohorts.R",
  analyses  = "analyses/incidencePrevalence.R",
  codes_dir = "codelist"
)

# A codelist's CSV, under its own category folder -- the layout DARWIN studies
# already use (Comorbidities/, Outcomes/, Symptoms/ ...). An uncategorised
# codelist falls into Other, the same answer the document's appendix gives.
codelist_csv_path <- function(cl) {
  cat_dir <- as.character(cl$category %||% "")[1]
  if (is.na(cat_dir) || !nzchar(trimws(cat_dir))) cat_dir <- "Other"
  # A category is free text and becomes a directory name, so anything a file
  # system would object to is folded away rather than passed through.
  cat_dir <- gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", trimws(cat_dir)))
  cat_dir <- gsub("^_|_$", "", cat_dir)
  file.path(STUDY_PATHS$codes_dir, cat_dir,
            sprintf("%s.csv", cohort_table_name(cl$name) %||% "codelist"))
}

# One codelist as its CSV: the resolved snapshot, which is what a study reads.
# concept_id and concept_name are the column names CodelistGenerator and the
# Atlas exports use, so the file drops straight into an existing pipeline.
codelist_csv_content <- function(cl) {
  codes <- cl$codes %||% list()
  if (!length(codes)) return(NULL)
  rows <- vapply(codes, function(cd) {
    nm <- as.character(cd$name %||% "")[1]
    if (is.na(nm)) nm <- ""
    # Quoted because concept names contain commas ("Follicular lymphoma, grade 1").
    sprintf('%s,"%s"', as.character(cd$code %||% ""), gsub('"', '""', nm))
  }, character(1))
  paste(c("concept_id,concept_name", rows, ""), collapse = "\n")
}

# codelist/codelistCreation.R: every concept set the cohorts enter on, read back
# from the CSVs written beside it.
#
# Read from file rather than inlined as literals, unlike the document's appendix.
# The appendix is read on a page, so the ids have to be visible there; a study
# directory has the CSV sitting next to the script, and a reviewer diffing a
# codelist wants the CSV, not a 75-element c().
#
# importCodelist() rather than read.csv(): it is omopgenerics' own reader, it
# returns a codelist keyed by the file name -- which is the codelist's name, so
# conceptCohort(conceptSet =) takes it directly -- and it is what a DARWIN study
# already writes. A hand-rolled read.csv()$concept_id was ours to maintain for no
# benefit.
study_codelists_r <- function(sap) {
  cited <- cited_codelist_names(sap$cohorts)
  cls   <- Filter(function(cl) {
    nm <- as.character(cl$name %||% "")[1]
    nm %in% cited && length(cl$codes %||% list())
  }, sap$codelists %||% list())
  if (!length(cls)) {
    return(paste("# No concept sets: no cohort in this SAP enters on a codelist yet.\n"))
  }
  blocks <- vapply(cls, function(cl) {
    var <- concept_set_var(cl$name)
    warn <- if (concept_set_expands(cl$concept_set_expression %||% list())) {
      paste0("# TODO: ", cl$name, " is a concept set EXPRESSION -- the CSV holds the\n",
             "#   concepts it names, not the descendants it also covers. Resolve it\n",
             "#   with CodelistGenerator before running.\n")
    } else ""
    sprintf('%s# %s\n%s <- importCodelist(path = here("%s"), type = "csv")',
            warn, as.character(cl$name %||% ""), var, codelist_csv_path(cl))
  }, character(1))
  paste0(paste(blocks, collapse = "\n\n"), "\n")
}

# cohorts/instantiateCohorts.R: every cohort the SAP types, in order.
study_cohorts_r <- function(sap) {
  cls <- vapply(sap$codelists %||% list(),
                function(cl) as.character(cl$name %||% "")[1], character(1))
  cohort_names <- vapply(sap$cohorts %||% list(),
                         function(co) as.character(co$name %||% "")[1], character(1))
  blocks <- Filter(Negate(is.null), lapply(sap$cohorts %||% list(), function(co) {
    code <- cohort_operations_code(co, cls, cohort_names)
    if (is.null(code)) return(NULL)
    # An index on subject_id and cohort_start_date, added the way every DARWIN
    # study adds one. Purely a performance step -- it changes no result -- so it
    # belongs in the code that runs and not in the plan a reviewer signs.
    tbl <- cohort_table_name(co$name)
    idx <- if (is.na(tbl)) "" else sprintf(
      "\ncdm$%s <- cdm$%s |> addCohortTableIndex()", tbl, tbl)
    sprintf("# %s\n%s%s", as.character(co$name %||% ""), code, idx)
  }))
  if (!length(blocks)) {
    return(paste0(
      "# No cohorts are generated yet: none of this SAP's cohorts carry typed\n",
      "# operations, so their logic is still prose. See R/cohort_operations.R.\n"))
  }
  paste0(paste(unlist(blocks), collapse = "\n\n"), "\n")
}

# analyses/incidencePrevalence.R: the denominator sets, then the estimates,
# accumulating into `results` for runStudy.R to bind and export.
study_analyses_r <- function(sap) {
  cohorts  <- sap$cohorts %||% list()
  analyses <- sap$proposed_analyses %||% list()
  out <- character(0)

  dens <- Filter(Negate(is.null), lapply(cohorts, function(co) {
    code <- cohort_r_code(co)
    if (is.null(code)) NULL else sprintf("# %s\ncdm <- %s", as.character(co$name %||% ""), code)
  }))
  if (length(dens)) {
    out <- c(out, paste0("# Denominator cohort sets ----\n\n",
                         paste(unlist(dens), collapse = "\n\n")))
  }

  # The result NAMES are the estimate variable names the standalone script uses,
  # so a reader moving between the appendix and this file recognises them.
  vars <- estimate_var_names(analyses)
  est  <- Filter(Negate(is.null), lapply(seq_along(analyses), function(i) {
    a    <- analyses[[i]]
    nm   <- as.character(a$name %||% "")
    code <- analysis_r_code(a, cohorts)
    if (is.null(code)) {
      return(sprintf("# %s\n#   No IncidencePrevalence function maps onto analysis type '%s'.",
                     nm, as.character(a$analysis_type %||% "")))
    }
    # The objective is carried into the code, so someone reading the study
    # directory can see WHY an estimate is there without going back to the plan.
    obj <- objective_labels(sap, a)
    sprintf('# %s%s\nresults[["%s"]] <- %s', nm, obj, vars[[i]], code)
  }))
  if (length(est)) {
    out <- c(out, paste0("# Estimates ----\n\n", paste(unlist(est), collapse = "\n\n")))
  }

  if (!length(out)) {
    return("# No denominator cohort sets or incidence / prevalence estimates yet.\n")
  }
  paste0(paste(out, collapse = "\n\n"), "\n")
}

# "  [objective 1, 3]" for an analysis that names them, "" otherwise. Numbered
# as the document numbers them, so the two read the same way.
objective_labels <- function(sap, a) {
  objs <- sap$study$objectives %||% list()
  ids  <- vapply(objs, function(o) objective_id(o) %||% "", character(1))
  named <- as.character(unlist(a$objectives %||% list()))
  hits  <- which(ids %in% named)
  if (!length(hits)) return("")
  sprintf("  [objective%s %s]", if (length(hits) == 1) "" else "s",
          paste(hits, collapse = ", "))
}

# A header naming the SAP a file came from, so a study directory says where its
# code was decided and a regenerated file is recognisable as generated.
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
    as.character(study$study_code %||% study$title %||% "unnamed"),
    as.character(study$version %||% "unversioned"),
    as.character(sap$sap_schema_version %||% "unknown"))
}

# Every file the SAP owns: relative path -> content. The single place the layout
# is decided, so writing, testing and previewing all agree.
study_files <- function(sap) {
  files <- list()
  files[[STUDY_PATHS$codelists]] <- paste0(
    study_file_header(sap, "Concept sets"), "\n", study_codelists_r(sap))
  files[[STUDY_PATHS$cohorts]] <- paste0(
    study_file_header(sap, "Study cohorts"), "\n", study_cohorts_r(sap))
  files[[STUDY_PATHS$analyses]] <- paste0(
    study_file_header(sap, "Incidence and prevalence"), "\n", study_analyses_r(sap))

  cited <- cited_codelist_names(sap$cohorts)
  for (cl in sap$codelists %||% list()) {
    if (!as.character(cl$name %||% "")[1] %in% cited) next
    content <- codelist_csv_content(cl)
    if (!is.null(content)) files[[codelist_csv_path(cl)]] <- content
  }
  files
}

# What the SAP decides that lives in a file this must not write. codeToRun.R
# holds database credentials, so it is never overwritten -- but the SAP's export
# threshold belongs in it, and a silent mismatch between the plan's threshold and
# the study's would be exactly the kind of gap the plan exists to close.
study_export_notes <- function(sap) {
  n <- sap$study$min_cell_count %||% NULL
  if (is.null(n)) {
    return(paste("This SAP states no minimum cell count. codeToRun.R ships with",
                 "min_cell_count <- 5; confirm that is the threshold the data",
                 "partners require."))
  }
  sprintf(paste("Set min_cell_count <- %s in codeToRun.R to match this SAP.",
                "runStudy.R applies it once, in exportSummarisedResult(), so no",
                "suppression is generated here."), format(n))
}

# Write the files into an OmopStudyBuilder studyCode/ directory. Returns the
# paths written, invisibly.
#
# Creates missing directories but never removes anything: the target is a study
# skeleton with its own README files, .Rproj and results folders, and an export
# has no business deleting what it did not create.
write_study_files <- function(sap, dir) {
  files <- study_files(sap)
  paths <- character(0)
  for (rel in names(files)) {
    path <- file.path(dir, rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    # writeLines() adds the final newline, so the content must not carry one or
    # every generated file ends on a blank line.
    writeLines(sub("\n+$", "", files[[rel]]), path)
    paths <- c(paths, path)
  }
  invisible(paths)
}
