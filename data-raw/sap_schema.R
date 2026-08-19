
schema_dirs <- list.dirs(
  path = here::here("data-raw"),
  full.names = TRUE,
  recursive = FALSE
) |>
  purrr::keep(\(x) stringr::str_starts(basename(x), "v"))

if (!length(schema_dirs)) {
  stop("No versioned schema directories found in data-raw.")
}

schema_versions <- schema_dirs |>
  basename() |>
  stringr::str_remove("^v")

if (anyDuplicated(schema_versions)) {
  stop("Schema version directories must be unique.")
}

read_schema_csv <- function(path, version) {
  out <- readr::read_csv(
    file = path,
    col_types = c(
      schema_version = "c",
      object = "c",
      type_id = "c",
      path = "c",
      node_type = "c",
      value_type = "c",
      json_type = "c",
      required = "l",
      default_json = "c",
      order = "i"
    ),
    show_col_types = FALSE
  )

  required_columns <- c(
    "schema_version", "object", "type_id", "path", "node_type",
    "value_type", "json_type", "required", "default_json", "order"
  )
  if (!identical(names(out), required_columns)) {
    stop("Unexpected columns in ", path, ".")
  }
  if (any(out$schema_version != version)) {
    stop("Schema version mismatch in ", path, ".")
  }
  field_key <- paste(
    out$object,
    ifelse(is.na(out$type_id), "", out$type_id),
    out$path,
    sep = "\u001f"
  )
  if (anyDuplicated(field_key)) {
    stop("Duplicate object/type/path definitions in ", path, ".")
  }
  out
}

read_types_csv <- function(path, version) {
  out <- readr::read_csv(
    file = path,
    col_types = c(
      schema_version = "c",
      object = "c",
      type_id = "c",
      label = "c"
    ),
    show_col_types = FALSE
  )

  required_columns <- c("schema_version", "object", "type_id", "label")
  if (!identical(names(out), required_columns)) {
    stop("Unexpected columns in ", path, ".")
  }
  if (any(out$schema_version != version)) {
    stop("Type version mismatch in ", path, ".")
  }
  if (anyDuplicated(paste(out$object, out$type_id))) {
    stop("Duplicate object/type definitions in ", path, ".")
  }
  out
}

sapSchema <- purrr::map2(
  schema_dirs,
  schema_versions,
  \(directory, version) read_schema_csv(
    file.path(directory, "schema.csv"), version
  )
)
sapTypes <- purrr::map2(
  schema_dirs,
  schema_versions,
  \(directory, version) read_types_csv(
    file.path(directory, "types.csv"), version
  )
)
names(sapSchema) <- schema_versions
names(sapTypes) <- schema_versions

# A type-specific field may only refer to a type declared in types.csv. Common
# fields have an empty type_id and are valid for every instance of the object.
purrr::walk2(sapSchema, sapTypes, \(fields, types) {
  typed_fields <- unique(fields[
    !is.na(fields$type_id) & nzchar(fields$type_id),
    c("object", "type_id")
  ])
  known_types <- unique(types[, c("object", "type_id")])
  known_key <- paste(known_types$object, known_types$type_id, sep = "\u001f")
  typed_key <- paste(typed_fields$object, typed_fields$type_id, sep = "\u001f")
  unknown <- typed_fields[!typed_key %in% known_key, , drop = FALSE]
  if (nrow(unknown) > 0) {
    stop("Schema fields refer to undeclared types: ",
         paste(paste(unknown$object, unknown$type_id, sep = "/"), collapse = ", "))
  }
})

usethis::use_data(sapSchema, sapTypes, internal = TRUE, overwrite = TRUE)
