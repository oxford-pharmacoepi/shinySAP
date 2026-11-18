test_that("both study constructors produce identical objects with default metadata", {
  expected <- structure(
    list(study_id = "S1", title = NULL, authors = character(),
         version = "1.0.0", description = NULL),
    class = c("omop_sap_study", "list")
  )
  expect_identical(createStudySap("S1"), expected)
  expect_identical(newStudySap(unclass(expected)), expected)

  study <- createStudySap(
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

test_that("newStudySap accepts named metadata lists and normalizes field order", {
  fields <- list(
    description = "Study details", version = "1.2-3", authors = c("Alice", "Bob"),
    title = "A study", study_id = "S1"
  )
  original <- fields
  study <- newStudySap(fields)
  expect_identical(study, createStudySap("S1", "A study", c("Alice", "Bob"), "1.2-3", "Study details"))
  expect_identical(newStudySap(study), study)
  expect_identical(fields, original)
})

test_that("newStudySap requires exactly the named study fields", {
  fields <- unclass(createStudySap("S1"))
  expect_error(newStudySap("S1"), "must be a list")
  expect_error(newStudySap(unname(fields)), "named")
  expect_error(newStudySap(list()))
  expect_error(newStudySap(fields[-1]), "names")
  expect_error(newStudySap(c(fields, list(extra = "value"))), "names")

  unknown <- fields
  names(unknown)[1] <- "studyId"
  expect_error(newStudySap(unknown), "names")
  duplicated <- fields
  names(duplicated)[2] <- "study_id"
  expect_error(newStudySap(duplicated), "names")
})

test_that("both study constructors apply the same field validations", {
  expect_error(createStudySap(), "studyId")
  invalid <- list(
    studyId = list(NULL, 1, c("S1", "S2"), NA_character_),
    title = list(1, c("A", "B"), NA_character_),
    authors = list(NULL, 1, NA_character_),
    version = list(NULL, 1, c("1", "2"), NA_character_),
    description = list(1, c("A", "B"), NA_character_)
  )
  for (argument in names(invalid)) {
    for (value in invalid[[argument]]) {
      args <- list(studyId = "S1")
      args[argument] <- list(value)
      expect_error(do.call(createStudySap, args), argument, info = argument)
      fields <- unclass(createStudySap("S1"))
      field <- if (argument == "studyId") "study_id" else argument
      fields[field] <- list(value)
      expect_error(newStudySap(fields), argument, info = argument)
    }
  }
  for (version in c("", "v1.0", "1.0_rc1", "1 0")) {
    expect_error(
      createStudySap("S1", version = version),
      "can only contain numbers", info = version
    )
  }
})

test_that("printing study metadata formats all fields and returns invisibly", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 80))
  study <- createStudySap(
    "S1", "A study", c("Alice", "Bob"), "1.2-3", "Study details"
  )
  original <- study
  output <- capture_messages(printed <- expect_invisible(print(study)))

  expect_match(output[[1]], "^── OMOP Study metadata ─{3,}$")
  expect_lte(nchar(output[[1]], type = "width"), 80L)
  for (field in c("Study ID: S1", "Title: A study", "Authors: Alice & Bob",
                  "Version: ‘1.2-3’", "Description: Study details")) {
    expect_match(output[[2]], field, fixed = TRUE)
  }
  expect_identical(printed, study)
  expect_identical(study, original)
})

test_that("printing missing metadata uses placeholders", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 80))
  output <- capture_messages(print(createStudySap("S1")))
  for (field in c("Title: ─", "Authors: ─", "Description: ─")) {
    expect_match(output[[2]], field, fixed = TRUE)
  }
})

test_that("study descriptions can be hidden and headings fit narrow consoles", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 10))
  output <- capture_messages(print(
    createStudySap("S1", description = "Study details"), description = FALSE
  ))

  expect_identical(output[[1]], "── OMOP Study metadata ──")
  expect_false(any(grepl("Description|Study details", output)))
})

test_that("study edits validate fields through both assignment methods", {
  study <- createStudySap("S1")
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
  study <- createStudySap("S1", title = "A study", description = "Details")
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
  study <- createStudySap("S1", title = "A study")
  original <- study

  expect_error(study["title"] <- list(123), "Use.*to edit individual study fields")
  expect_error(study["title"] <- NULL, "Use.*to edit individual study fields")
  expect_error(study[] <- list(NULL), "Use.*to edit individual study fields")
  expect_identical(study, original)
})

test_that("updateStudy returns an updated SAP without changing its input", {
  sap <- createSap(createStudySap("S1", title = "Original title"))
  original <- sap
  updated <- updateStudy(sap, "title", "Updated title")
  updated <- updateStudy(updated, "authors", c("Alice", "Bob"))

  expect_identical(updated$study, createStudySap("S1", "Updated title", c("Alice", "Bob")))
  expect_s3_class(updated, "omop_sap")
  other_fields <- setdiff(names(sap), "study")
  expect_identical(unclass(updated)[other_fields], unclass(sap)[other_fields])
  expect_identical(sap, original)
})

test_that("updateStudy clears optional values while preserving their fields", {
  sap <- createSap(createStudySap("S1", "A study", "Alice", description = "Details"))
  updated <- updateStudy(sap, "title", NULL)
  updated <- updateStudy(updated, "description", NULL)
  updated <- updateStudy(updated, "authors", character())

  expect_identical(updated$study, createStudySap("S1"))
  expect_named(updated$study, names(sap$study))
})

test_that("updateStudy rejects invalid containers, fields and values", {
  sap <- createSap(createStudySap("S1"))
  original <- sap
  expect_error(updateStudy(unclass(sap), "title", "A study"), "omop_sap")
  malformed <- structure(list(study = list()), class = c("omop_sap", "list"))
  expect_error(updateStudy(malformed, "title", "A study"), "omop_sap_study")
  expect_error(updateStudy(sap, "unknown", "value"), "Only fields")
  expect_error(updateStudy(sap, c("title", "description"), "value"), "length")
  expect_error(updateStudy(sap, "title", 123), "title")
  expect_error(updateStudy(sap, "version", "invalid"), "can only contain numbers")
  expect_error(updateStudy(sap, "study_id", NULL), "studyId")
  expect_identical(sap, original)
})
