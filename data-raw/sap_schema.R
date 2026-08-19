
sapSchema <- list.dirs(
  path = here::here("data-raw"),
  full.names = TRUE,
  recursive = FALSE
) |>
  purrr::keep(\(x) stringr::str_starts(basename(x), "v"))
schemaVersions <- stringr::str_remove(basename(schemaDirs), "^v")

names(sapSchema) <- schemaVersions

sapTypes <- sapSchema |>
  purrr::map(\(x) {
    cl <- c(object = "c", type_id = "c", label = "c")
    readr::read_csv(file = file.path(x, "types.csv"), col_types = cl)
  })

sapSchema <- sapSchema |>
  purrr::map(\(x) {
    cl <- c(object = "c", type_id = "c", path = "c", node_type = "c",
            value_type = "c", json_type = "c", required = "l",
            default_json = "c", order = "i")
    readr::read_csv(file = file.path(x, "schema.csv"), col_types = cl)
  })

currentVersion <- "0.1.0"

usethis::use_data(sapSchema, sapTypes, currentVersion, internal = TRUE, overwrite = TRUE)
