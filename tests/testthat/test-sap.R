# Tests for the current SAP containers. The remaining component APIs are legacy
# and will be covered here as they are migrated.

test_that("createSap builds the current containers with empty collections", {
  study <- createStudySap("S1", title = "Example study")
  sap <- createSap(study)

  expect_identical(class(sap), c("omop_sap", "list"))
  expect_named(sap, c(
    "study", "data_sources", "data_source_modifications", "codelists",
    "cohorts", "analyses", "sap_schema_version", "generated_at"
  ))
  expect_identical(sap$study, study)
  expect_identical(sap$data_sources, newSapDataSource(list()))
  expect_identical(sap$codelists, prepareCodelists(list()))
  expect_identical(sap$data_source_modifications, list())
  expect_identical(sap$cohorts, list())
  expect_identical(sap$analyses, list())
  expect_identical(sap$sap_schema_version, currentSapSchemaVersion())
  expect_match(sap$generated_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[+-][0-9]{4}$")
})

test_that("SAP construction normalizes study lists, data descriptions and codelists", {
  description <- list(
    administrative_details = list(
      name_of_data_source = "Example source", data_source_acronym = "EX"
    ),
    data_collection = list(),
    omop_standardisation = list()
  )
  codelists <- omopgenerics::newCodelist(list(example = c(1L, 2L)))
  sap <- createSap(unclass(createStudySap("S1")), dataSources = description, codelists = codelists)

  expect_identical(sap$study, createStudySap("S1"))
  expect_identical(sap$data_sources, newSapDataSource(description))
  expect_identical(sap$codelists, prepareCodelists(codelists))
  expect_identical(sap$codelists$example$content$concept_id, c(1L, 2L))

  sap[["study"]] <- createStudySap("S2")
  sap[["data_sources"]] <- newSapDataSource(description)
  expect_identical(sap$study$study_id, "S2")
  expect_identical(sap$data_sources$EX, newSapDataSource(description)$EX)
})

test_that("newSap supplies missing metadata and preserves supplied metadata", {
  input <- unclass(createSap(createStudySap("S1")))
  input$sap_schema_version <- NULL
  input$generated_at <- NULL
  sap <- newSap(input)
  expect_identical(sap$sap_schema_version, currentSapSchemaVersion())
  expect_type(sap$generated_at, "character")
  expect_length(sap$generated_at, 1)

  input$generated_at <- "2026-01-01T12:00:00+0000"
  input$sap_schema_version <- currentSapSchemaVersion()
  sap <- newSap(input, sapSchemaVersion = NULL, validate = FALSE)
  expect_identical(unclass(sap), input)
  expect_identical(createSap(createStudySap("S1"), validate = FALSE)$study, createStudySap("S1"))

  input$sap_schema_version <- "unsupported"
  expect_error(newSap(input), "sap_schema_version must be")
})

test_that("newSap rejects invalid arguments and unexpected container fields", {
  input <- unclass(createSap(createStudySap("S1")))
  expect_error(newSap(1L), "must be a list")
  expect_error(newSap(list("S1")), "named")
  expect_error(newSap(input, sapSchemaVersion = "unsupported"), "sapSchemaVersion")
  expect_error(newSap(input, validate = NA), "validate")

  missing <- input
  missing$study <- NULL
  expect_error(newSap(missing), "Missing compulsory fields.*study")
  extra <- input
  extra$unexpected <- list()
  expect_error(newSap(extra), "Extra not allowed fields.*unexpected")
})

test_that("newSap rejects duplicate component names", {
  input <- unclass(createSap(createStudySap("S1")))
  duplicated <- c(input, list(study = 123))
  expect_error(newSap(duplicated), "[Dd]uplicat|unique")
})

test_that("single-field SAP replacement reuses component constructors", {
  sap <- createSap(createStudySap("S1"))
  sap[["study"]] <- unclass(createStudySap("S2"))
  expect_identical(sap$study, createStudySap("S2"))
  sap$study <- createStudySap("S3")
  expect_identical(sap$study, createStudySap("S3"))
  sap[["codelists"]] <- omopgenerics::newCodelist(list(example = 1L))
  expect_identical(sap$codelists$example$type, "codelist")
  sap[["data_sources"]] <- list()
  expect_identical(sap$data_sources, newSapDataSource(list()))
  expect_s3_class(sap, "omop_sap")
})

test_that("invalid SAP component replacements leave the original unchanged", {
  sap <- createSap(createStudySap("S1"))
  original <- sap

  expect_error(sap[["unexpected"]] <- list(), "i")
  expect_error(sap[["study"]] <- 123, "valid.*omop_sap_study")
  expect_error(sap[["data_sources"]] <- 123, "valid.*omop_sap_data_source")
  expect_error(sap[["codelists"]] <- 123, "has to be a")
  expect_identical(sap, original)
})

test_that("SAP dollar replacement rejects invalid component values", {
  sap <- createSap(createStudySap("S1"))
  expect_error(sap$study <- 123, "valid.*omop_sap_study")
})

test_that("single-bracket assignment cannot bypass SAP validation", {
  sap <- createSap(createStudySap("S1"))
  original <- sap
  expect_error(sap["study"] <- list(123), "Use.*to edit individual sap fields")
  expect_error(sap["study"] <- NULL, "Use.*to edit individual sap fields")
  expect_error(sap[] <- list(NULL), "Use.*to edit individual sap fields")
  expect_identical(sap, original)
})

test_that("printing a SAP shows its current components and hides the study description", {
  withr::local_options(list(cli.num_colors = 1, cli.width = 80))
  sap <- createSap(
    createStudySap("S1", description = "Study details"),
    codelists = omopgenerics::newCodelist(list(example = 1L))
  )
  original <- sap
  output <- capture.output(messages <- capture_messages(printed <- expect_invisible(print(sap))))

  for (heading in c("OMOP Study metadata", "Data Sources", "OMOP Codelists")) {
    expect_true(any(grepl(heading, messages, fixed = TRUE)), info = heading)
  }
  expect_true(any(grepl("No Data Sources included", messages, fixed = TRUE)))
  expect_false(any(grepl("Description:|Study details", messages)))
  expect_identical(output, " - example")
  expect_identical(printed, sap)
  expect_identical(sap, original)
})
