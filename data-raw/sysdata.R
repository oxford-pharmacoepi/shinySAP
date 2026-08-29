
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
        default_json = "c", order = "i", ref = "c"
      )
    )
  }
)
names(sapSchema) <- schemaVersions

purrr::iwalk(sapSchema, function(fields, version) {
  references <- fields[!is.na(fields$ref), , drop = FALSE]
  collections <- fields$path[fields$object == "sap" & fields$node_type == "collection"]
  unknown <- setdiff(references$ref, collections)
  if (length(unknown)) {
    cli::cli_abort(c(
      x = "Schema {version}: 'ref' names unknown collections: {unknown}."
    ))
  }
  if (!all(references$value_type %in% c("id", "id_vector"))) {
    cli::cli_abort(c(
      x = "Schema {version}: only id fields may declare a 'ref'."
    ))
  }
  if (!all(references$node_type == "field")) {
    cli::cli_abort(c(x = "Schema {version}: only fields may declare a 'ref'."))
  }
})

currentVersion <- schemaVersions[[1]]
if (length(schemaVersions) > 1L) {
  for (candidateVersion in schemaVersions[-1]) {
    if (utils::compareVersion(candidateVersion, currentVersion) > 0) {
      currentVersion <- candidateVersion
    }
  }
}

usethis::use_data(sapSchema, sapTypes, currentVersion, internal = TRUE, overwrite = TRUE)
