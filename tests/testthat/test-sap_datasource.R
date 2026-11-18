sourceDescription <- function(acronym) {
  list(
    administrative_details = list(
      name_of_data_source = paste("Data source", acronym),
      data_source_acronym = acronym
    ),
    data_collection = list(geography = c("England", "Wales")),
    omop_standardisation = list()
  )
}

test_that("c combines data-source collections and raw descriptions", {
  prepared <- newSapDataSource(list(sourceDescription("D"), sourceDescription("B")))
  original <- prepared
  single <- sourceDescription("C")
  multiple <- list(sourceDescription("E"), sourceDescription("A"))

  combined <- c(prepared = prepared, single = single, multiple = multiple)
  expect_s3_class(combined, "omop_sap_data_source")
  expect_named(combined, LETTERS[1:5])
  expect_identical(combined$B, prepared$B)
  expect_identical(combined$C, newSapDataSource(single)$C)
  expect_identical(combined$A$data_collection$geography, "England\nWales")
  expect_identical(combined$A$administrative_details$data_source_website, NA_character_)
  expect_identical(prepared, original)
  expect_identical(c(prepared), prepared)
  expect_no_error(capture_messages(print(combined)))
})

test_that("c rejects duplicate data-source acronyms across inputs", {
  prepared <- newSapDataSource(sourceDescription("A"))
  duplicate <- sourceDescription("A")

  expect_error(c(prepared, duplicate), "acronyms must be unique")
  expect_error(c(prepared, prepared), "acronyms must be unique")
  expect_error(c(first = prepared, second = duplicate), "acronyms must be unique")
})

test_that("c preserves empty data-source collections and validates raw inputs", {
  empty <- newSapDataSource(list())
  source <- newSapDataSource(sourceDescription("A"))

  expect_identical(c(empty), empty)
  expect_identical(c(empty, source), source)
  expect_identical(c(source, empty), source)
  expect_error(c(source, 1L), "not a list")
  incomplete <- sourceDescription("B")
  incomplete$data_collection <- NULL
  expect_error(c(source, incomplete), "Missing section")
})

test_that("data-source validation rejects malformed sections and fields", {
  expect_error(newSapDataSource(list(invalid = 1L)), "not a list")

  description <- sourceDescription("A")
  description$unexpected <- list()
  expect_error(newSapDataSource(description), "Unexpected section.*unexpected")
  description$unexpected <- NULL

  description$data_collection <- "invalid"
  expect_error(newSapDataSource(description), "data_collection.*must be a list")
  description$data_collection <- list(unexpected = "value")
  expect_error(newSapDataSource(description), "Unexpected field.*unexpected")
  description$data_collection <- list()

  description$administrative_details$name_of_data_source <- NULL
  expect_error(newSapDataSource(description), "Missing field.*name_of_data_source")
  description$administrative_details["name_of_data_source"] <- list(NULL)
  expect_error(newSapDataSource(description), "name_of_data_source.*must not be NULL")
})

test_that("data-source printing describes both populated and empty collections", {
  withr::local_options(list(cli.num_colors = 1, cli.width = 80))
  empty <- newSapDataSource(list())
  output <- capture_messages(expect_invisible(print(empty)))
  expect_true(any(grepl("No Data Sources included in this study", output, fixed = TRUE)))

  source <- newSapDataSource(sourceDescription("A"))
  output <- capture_messages(expect_invisible(print(source)))
  expect_true(any(grepl("Data source A (A)", output, fixed = TRUE)))
})

test_that("data-source assignments store a single normalized description", {
  sources <- newSapDataSource(sourceDescription("A"))
  replacement <- sourceDescription("A")
  replacement$administrative_details$name_of_data_source <- "Updated source"
  sources$A <- replacement
  expect_identical(sources$A, newSapDataSource(replacement)$A)

  added <- newSapDataSource(sourceDescription("B"))
  sources[["B"]] <- added
  expect_identical(sources$B, added$B)
  expect_named(sources, c("A", "B"))
  expect_s3_class(sources, "omop_sap_data_source")
  expect_no_error(capture_messages(print(sources)))

  sources$A <- NULL
  expect_named(sources, "B")
  sources[["B"]] <- NULL
  expect_length(sources, 0)
  expect_s3_class(sources, "omop_sap_data_source")
})

test_that("invalid data-source assignments leave the original unchanged", {
  sources <- newSapDataSource(sourceDescription("A"))
  original <- sources
  expect_error(sources[[1]] <- sourceDescription("A"), "i")
  expect_error(sources$A <- list(), "Only one data source")
  expect_error(
    sources[["A"]] <- list(sourceDescription("A"), sourceDescription("B")),
    "Only one data source"
  )
  expect_error(sources$A <- sourceDescription("B"), "named B.*assigned to A")
  expect_error(sources["A"] <- list(sourceDescription("A")), "Use.*to edit individual data sources")
  expect_identical(sources, original)
})
