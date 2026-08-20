
schemaDirs <- list.dirs(
  path = here::here("data-raw"),
  full.names = TRUE,
  recursive = FALSE
) |>
  purrr::keep(\(x) stringr::str_starts(basename(x), "v"))

if (!length(schemaDirs)) {
  cli::cli_abort(c(x = "No versioned SAP schema directories were found."))
}

schemaVersions <- stringr::str_remove(basename(schemaDirs), "^v")
if (anyDuplicated(schemaVersions)) {
  cli::cli_abort(c(x = "SAP schema directories must have unique versions."))
}

sapTypes <- purrr::map(
  schemaDirs,
  \(schemaDir) {
    readr::read_csv(
      file = file.path(schemaDir, "types.csv"),
      col_types = c(object = "c", type_id = "c", label = "c")
    )
  }
)
names(sapTypes) <- schemaVersions

sapSchema <- purrr::map(
  schemaDirs,
  \(schemaDir) {
    readr::read_csv(
      file = file.path(schemaDir, "schema.csv"),
      col_types = c(
        object = "c", type_id = "c", path = "c", node_type = "c",
        value_type = "c", json_type = "c", required = "l",
        default_json = "c", order = "i"
      )
    )
  }
)
names(sapSchema) <- schemaVersions

currentVersion <- schemaVersions[[1]]
if (length(schemaVersions) > 1L) {
  for (candidateVersion in schemaVersions[-1]) {
    if (utils::compareVersion(candidateVersion, currentVersion) > 0) {
      currentVersion <- candidateVersion
    }
  }
}

usethis::use_data(sapSchema, sapTypes, currentVersion, internal = TRUE, overwrite = TRUE)
