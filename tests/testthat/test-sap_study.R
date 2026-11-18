test_that("newSapStudy preserves supplied metadata and supplies defaults", {
  expect_identical(
    newSapStudy("S1"),
    structure(
      list(study_id = "S1", title = NULL, authors = character(),
           version = "1.0.0", description = NULL),
      class = c("omop_sap_study", "list")
    )
  )

  study <- newSapStudy(
    "S1", title = "A study", authors = c("Martí Català", "Another Author"),
    version = "1.2-3", description = "Study details"
  )
  expect_identical(
    unclass(study),
    list(study_id = "S1", title = "A study",
         authors = c("Martí Català", "Another Author"),
         version = "1.2-3", description = "Study details")
  )
})

test_that("newSapStudy validates its arguments", {
  expect_error(newSapStudy(), "studyId")
  invalid <- list(
    studyId = list(NULL, 1, c("S1", "S2"), NA_character_),
    title = list(1, c("A", "B")),
    authors = list(NULL, 1, NA_character_),
    version = list(NULL, 1, c("1", "2"), NA_character_),
    description = list(1, c("A", "B"))
  )
  for (argument in names(invalid)) {
    for (value in invalid[[argument]]) {
      args <- list(studyId = "S1")
      args[argument] <- list(value)
      expect_error(do.call(newSapStudy, args), argument, info = argument)
    }
  }
  for (version in c("", "v1.0", "1.0_rc1", "1 0")) {
    expect_error(
      newSapStudy("S1", version = version),
      "can only contain numbers", info = version
    )
  }
})

test_that("printing study metadata formats all fields and returns invisibly", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 80))
  study <- newSapStudy(
    "S1", "A study", c("Alice", "Bob"), "1.2-3", "Study details"
  )
  original <- study
  output <- capture_messages(expect_invisible(print(study)))

  expect_match(output[[1]], "^── OMOP Study metadata ─{3,}$")
  expect_lte(nchar(output[[1]], type = "width"), 80L)
  for (field in c("Study ID: S1", "Title: A study", "Authors: Alice & Bob",
                  "Version: ‘1.2-3’", "Description: Study details")) {
    expect_match(output[[2]], field, fixed = TRUE)
  }
  expect_identical(study, original)
})

test_that("printing missing metadata uses placeholders", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 80))
  output <- capture_messages(print(newSapStudy("S1")))
  for (field in c("Title: ─", "Authors: ─", "Description: ─")) {
    expect_match(output[[2]], field, fixed = TRUE)
  }
})

test_that("study descriptions can be hidden and headings fit narrow consoles", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 10))
  output <- capture_messages(print(
    newSapStudy("S1", description = "Study details"), description = FALSE
  ))

  expect_identical(output[[1]], "── OMOP Study metadata ──")
  expect_false(any(grepl("Description|Study details", output)))
})

test_that("study edits validate fields through both assignment methods", {
  study <- newSapStudy("S1")
  study$title <- "Updated title"
  study[["authors"]] <- c("Alice", "Bob")
  expect_identical(study$title, "Updated title")
  expect_identical(study$authors, c("Alice", "Bob"))
  expect_s3_class(study, "omop_sap_study")

  original <- study
  expect_error(study$title <- 123, "title")
  expect_error(study[["version"]] <- "invalid", "can only contain numbers")
  expect_error(study$studyId <- "S2", "Only fields")
  expect_error(study[["unknown"]] <- "value", "Only fields")
  expect_error(study[[1]] <- "S2", "i")
  expect_identical(study, original)
})

test_that("clearing optional metadata preserves its named fields", {
  study <- newSapStudy("S1", title = "A study", description = "Details")
  fields <- names(study)
  study$title <- NULL
  study[["description"]] <- NULL

  expect_null(study$title)
  expect_null(study$description)
  expect_named(study, fields)
  expect_s3_class(study, "omop_sap_study")
  expect_identical(study$study_id, "S1")
  expect_error(study$version <- NULL, "version")
})

test_that("single-bracket assignment cannot bypass study validation", {
  study <- newSapStudy("S1", title = "A study")
  original <- study

  expect_error(study["title"] <- list(123), "Use.*to edit individual study fields")
  expect_error(study["title"] <- NULL, "Use.*to edit individual study fields")
  expect_error(study[] <- list(NULL), "Use.*to edit individual study fields")
  expect_identical(study, original)
})
