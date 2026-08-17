
sapSchema <- list.files(
  path = here::here("data-raw"),
  pattern = "^sap_schema_.*\\.csv$",
  full.names = TRUE
)
names(sapSchema) <- sapSchema |>
  basename() |>
  stringr::str_remove("^sap_schema_") |>
  stringr::str_remove("\\.csv$")
sapSchema <- sapSchema |>
  purrr::map(\(x) {
    ct <- c(sap_field = "c", sap_subfield = "c", default = "c")
    tbl <- readr::read_csv(file = x, col_types = ct)
    unique(tbl$sap_field) |>
      rlang::set_names() |>
      purrr::map(\(x) {
        tbli <- tbl |>
          dplyr::filter(.data$sap_field == .env$x)
        tbli$default |>
          as.list() |>
          rlang::set_names(tbli$sap_subfield)
      })
  })

usethis::use_data(sapSchema, internal = TRUE, overwrite = TRUE)
