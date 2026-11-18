test_that("codelist assignments store normalized content for each supported type", {
  details <- dplyr::tibble(concept_id = 3L, concept_name = "Example")
  expression <- dplyr::tibble(
    concept_id = 4L, excluded = FALSE, descendants = TRUE, mapped = FALSE
  )
  values <- list(
    codelist = omopgenerics::newCodelist(list(source = 2L)),
    codelist_with_details = omopgenerics::newCodelistWithDetails(list(source = details)),
    concept_set_expression = omopgenerics::newConceptSetExpression(list(source = expression))
  )
  contents <- list(
    codelist = dplyr::tibble(concept_id = 2L),
    codelist_with_details = details,
    concept_set_expression = expression
  )

  for (type in names(values)) {
    codelists <- prepareCodelists(omopgenerics::newCodelist(list(existing = 1L)))
    original <- codelists$existing
    expected <- list(type = type, content = contents[[type]])

    codelists[["added"]] <- values[[type]]
    expect_identical(codelists$added, expected)
    expect_identical(codelists$existing, original)
    codelists$existing <- values[[type]]
    expect_identical(codelists$existing, expected)
    expect_named(codelists, c("existing", "added"))
    expect_s3_class(codelists, "omop_sap_codelist")
    output <- capture.output(messages <- capture_messages(expect_invisible(print(codelists))))
    title <- switch(type,
      codelist = "Codelists",
      codelist_with_details = "Codelists with details",
      concept_set_expression = "Concept set expressions"
    )
    expect_true(any(grepl(paste0("2 ", title, ":"), messages, fixed = TRUE)))
    expect_true(all(vapply(c("added", "existing"), function(name) {
      any(grepl(name, output, fixed = TRUE))
    }, logical(1))))
  }
})

test_that("single-field codelist assignments reject empty and multiple entries", {
  codelists <- prepareCodelists(omopgenerics::newCodelist(list(existing = 1L)))
  original <- codelists
  expect_error(
    codelists$existing <- omopgenerics::newCodelist(list()),
    "exactly one codelist"
  )
  expect_error(
    codelists[["added"]] <- omopgenerics::newCodelist(list(a = 2L, b = 3L)),
    "exactly one codelist"
  )
  expect_identical(codelists, original)
})

test_that("NULL still removes a codelist through either assignment method", {
  codelists <- prepareCodelists(omopgenerics::newCodelist(list(a = 1L, b = 2L)))
  codelists$a <- NULL
  expect_named(codelists, "b")
  codelists[["b"]] <- NULL
  expect_length(codelists, 0)
  expect_s3_class(codelists, "omop_sap_codelist")
})

test_that("c combines prepared codelists with all supported raw types", {
  prepared <- prepareCodelists(omopgenerics::newCodelist(list(z = 1L)))
  plain <- omopgenerics::newCodelist(list(a = 2L))
  details <- dplyr::tibble(concept_id = 3L, concept_name = "Example")
  detailed <- omopgenerics::newCodelistWithDetails(list(b = details))
  expression <- dplyr::tibble(
    concept_id = 4L, excluded = FALSE, descendants = TRUE, mapped = FALSE
  )
  concept_set <- omopgenerics::newConceptSetExpression(list(c = expression))

  combined <- c(prepared, plain, detailed, concept_set)
  expect_s3_class(combined, "omop_sap_codelist")
  expect_named(combined, c("a", "b", "c", "z"))
  expect_identical(combined$a, list(type = "codelist", content = dplyr::tibble(concept_id = 2L)))
  expect_identical(combined$b, list(type = "codelist_with_details", content = details))
  expect_identical(combined$c, list(type = "concept_set_expression", content = expression))
  expect_identical(combined$z, prepared$z)
  expect_identical(names(prepared), "z")
})

test_that("c combines prepared collections without adding argument names", {
  first <- prepareCodelists(omopgenerics::newCodelist(list(b = 1L)))
  second <- prepareCodelists(omopgenerics::newCodelist(list(a = 2L)))

  expect_identical(c(first = first, second = second), prepareCodelists(list(first, second)))
  expect_identical(c(first), first)
})

test_that("c rejects duplicate names across prepared and raw codelists", {
  prepared <- prepareCodelists(omopgenerics::newCodelist(list(a = 1L)))
  duplicate <- omopgenerics::newCodelistWithDetails(list(a = dplyr::tibble(concept_id = 2L)))

  expect_error(c(prepared, duplicate), "Duplicated names.*a")
  expect_error(c(prepared, prepared), "Duplicated names.*a")
  expect_error(c(first = prepared, second = duplicate), "Duplicated names.*a")
})

test_that("preparation handles empty and nested codelist collections", {
  empty <- prepareCodelists(list())
  expect_identical(empty, structure(list(), class = c("omop_sap_codelist", "list")))
  expect_identical(prepareCodelists(empty), empty)

  first <- omopgenerics::newCodelist(list(b = 1L))
  second <- omopgenerics::newCodelist(list(a = 2L))
  combined <- prepareCodelists(list(list(first), second))
  expect_named(combined, c("a", "b"))
  expect_identical(combined$a$content$concept_id, 2L)
  expect_identical(combined$b$content$concept_id, 1L)
})

test_that("unsupported inputs and assignment methods cannot corrupt codelists", {
  expect_error(prepareCodelists(1L), "has to be a")
  codelists <- prepareCodelists(omopgenerics::newCodelist(list(a = 1L)))
  original <- codelists

  expect_error(codelists$a <- 2L, "must be a")
  expect_error(codelists[["a"]] <- list(concept_id = 2L), "must be a")
  expect_error(codelists["a"] <- list(2L), "Use.*to add or edit codelists")
  expect_identical(codelists, original)
})

test_that("printed names truncate and fill columns from top to bottom", {
  withr::local_options(list(cli.width = 40))
  expect_identical(
    capture.output(printNames(c("short", strrep("a", 40)))),
    c(" - short", paste0(" - ", strrep("a", 28), "..."))
  )

  withr::local_options(list(cli.width = 70))
  expect_identical(
    capture.output(printNames(as.character(1:6))),
    sprintf(" - %-32s - %s", 1:3, 4:6)
  )
  expect_identical(
    capture.output(printNames(as.character(1:5))),
    c(sprintf(" - %-32s - %s", 1:2, 4:5), " - 3")
  )
})
