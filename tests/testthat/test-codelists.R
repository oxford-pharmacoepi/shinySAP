# Codelists: first-class entities whose CODES live in the SAP json, uploaded
# from the shapes codelist tools actually produce (read_codelist in utils.R).

write_tmp <- function(lines, ext) {
  path <- tempfile(fileext = ext)
  writeLines(lines, path)
  path
}

test_that("read_codelist: a csv with concept_id and concept_name columns", {
  p <- write_tmp(c("concept_id,concept_name", "1503297,metformin", "40164929,metformin 500mg"),
                 ".csv")
  codes <- read_codelist(p, "x.csv")
  expect_length(codes, 2)
  expect_identical(codes[[1]], list(code = "1503297", name = "metformin"))
})

test_that("read_codelist: a csv with no recognised id column uses the first", {
  p <- write_tmp(c("snomed,label", "12345,thing"), ".csv")
  expect_identical(read_codelist(p, "x.csv")[[1]]$code, "12345")
})

test_that("read_codelist: a txt file is one code per line, blanks dropped", {
  p <- write_tmp(c("111", "", "  222  "), ".txt")
  codes <- read_codelist(p, "x.txt")
  expect_identical(vapply(codes, `[[`, "", "code"), c("111", "222"))
})

test_that("read_codelist: a json array of plain codes", {
  p <- write_tmp("[111, 222]", ".json")
  expect_identical(vapply(read_codelist(p, "x.json"), `[[`, "", "code"), c("111", "222"))
})

test_that("read_codelist: a json array of objects", {
  p <- write_tmp("[{\"concept_id\": 111, \"concept_name\": \"a\"}]", ".json")
  expect_identical(read_codelist(p, "x.json")[[1]], list(code = "111", name = "a"))
})

test_that("read_codelist: an Atlas concept-set expression", {
  p <- write_tmp("{\"items\": [{\"concept\": {\"CONCEPT_ID\": 111, \"CONCEPT_NAME\": \"a\"}}]}",
                 ".json")
  expect_identical(read_codelist(p, "x.json")[[1]], list(code = "111", name = "a"))
})

test_that("read_codelist: an empty file is a plain error, not a crash", {
  p <- write_tmp(character(0), ".txt")
  expect_error(read_codelist(p, "x.txt"), "no codes found")
})

# The module: upload becomes card state, survives into the data, and rides the
# shared Duplicate machinery like every other card.
test_that("codelists: upload -> data -> duplicate keeps the codes", {
  csv <- write_tmp(c("concept_id,concept_name", "111,a", "222,b"), ".csv")
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_test",
                      "codelist_1-upload" = list(name = "codes.csv", datapath = csv))
    d <- isolate(items$data())[[1]]
    expect_identical(d$name, "cs_test")
    expect_identical(d$source_file, "codes.csv")
    expect_length(d$codes, 2)
    expect_identical(d$codes[[1]]$code, "111")
    # Duplicate: the copy carries the codes without re-uploading.
    session$setInputs("codelist_1-duplicate" = 1)
    d2 <- isolate(items$data())[[2]]
    expect_length(d2$codes, 2)
  })
})

# 0.4.17: an optional category the document groups by. Blank stays null.
test_that("codelists: the card collects an optional category", {
  csv <- write_tmp(c("concept_id", "111"), ".csv")
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_mi", "codelist_1-category" = "Index event")
    expect_identical(isolate(items$data())[[1]]$category, "Index event")
    session$setInputs(add = 2)
    session$setInputs("codelist_2-name" = "cs_x")
    expect_true(is.na(isolate(items$data())[[2]]$category))
  })
})

test_that("codelists: load seeds the codes back from the saved SAP", {
  testServer(codelists_server, {
    load(list(list(name = "cs_x", description = NA, source_file = "x.csv",
                   codes = list(list(code = "111", name = "a")))))
    d <- isolate(items$data())[[1]]
    expect_length(d$codes, 1)
    expect_identical(d$codes[[1]]$code, "111")
  })
})

# Concept set expressions -------------------------------------------------------
#
# The expression is the specification and the codes are a snapshot of what it
# resolved to; see the header on read_concept_set() for why the SAP carries both.

test_that("read_concept_set: a flat file yields an expression with no expansion", {
  p <- write_tmp(c("concept_id,concept_name", "111,a", "222,b"), ".csv")
  cs <- read_concept_set(p, "x.csv")
  expect_length(cs$expression, 2)
  expect_identical(cs$expression[[1]],
                   list(concept_id = "111", excluded = FALSE,
                        descendants = FALSE, mapped = FALSE))
  expect_false(concept_set_expands(cs$expression))
  # The snapshot is exactly the concepts named, so it is the resolved codelist.
  expect_identical(cs$codes[[1]], list(code = "111", name = "a"))
})

# The flags are the whole reason to keep an expression: without them an Atlas
# export covering a subtree arrived as its single seed concept, and the SAP
# understated its own codelist.
test_that("read_concept_set: an Atlas export keeps its descendant and mapped flags", {
  p <- write_tmp(paste0(
    '{"items": [',
    '{"concept": {"CONCEPT_ID": 111, "CONCEPT_NAME": "a"}, "includeDescendants": true},',
    '{"concept": {"CONCEPT_ID": 222, "CONCEPT_NAME": "b"}, "includeMapped": true}]}'), ".json")
  cs <- read_concept_set(p, "x.json")
  expect_true(cs$expression[[1]]$descendants)
  expect_true(cs$expression[[2]]$mapped)
  expect_true(concept_set_expands(cs$expression))
})

# An excluded concept says "not this one", so a resolved list containing it
# would state the opposite of what the author uploaded.
test_that("read_concept_set: an excluded concept stays in the expression, not the codes", {
  p <- write_tmp(paste0(
    '{"items": [',
    '{"concept": {"CONCEPT_ID": 111, "CONCEPT_NAME": "a"}},',
    '{"concept": {"CONCEPT_ID": 222, "CONCEPT_NAME": "b"}, "isExcluded": true}]}'), ".json")
  cs <- read_concept_set(p, "x.json")
  expect_length(cs$expression, 2)
  expect_true(cs$expression[[2]]$excluded)
  expect_length(cs$codes, 1)
  expect_identical(cs$codes[[1]]$code, "111")
})

test_that("read_concept_set: an Atlas export nested under `expression` is found", {
  p <- write_tmp(
    '{"expression": {"items": [{"concept": {"CONCEPT_ID": 111}, "includeDescendants": true}]}}',
    ".json")
  cs <- read_concept_set(p, "x.json")
  expect_length(cs$expression, 1)
  expect_true(cs$expression[[1]]$descendants)
})

# read_codelist() is the snapshot half of the same reader, and the shape it
# returns is what pre-0.4.20 callers and saved SAPs already expect.
test_that("read_codelist still returns just the resolved codes", {
  p <- write_tmp(c("concept_id,concept_name", "111,a"), ".csv")
  expect_identical(read_codelist(p, "x.csv"), list(list(code = "111", name = "a")))
})

test_that("codelists: an upload collects the expression alongside the codes", {
  json <- write_tmp(
    '{"items": [{"concept": {"CONCEPT_ID": 111, "CONCEPT_NAME": "a"}, "includeDescendants": true}]}',
    ".json")
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_test",
                      "codelist_1-upload" = list(name = "cs.json", datapath = json))
    d <- isolate(items$data())[[1]]
    expect_length(d$concept_set_expression, 1)
    expect_true(d$concept_set_expression[[1]]$descendants)
    expect_identical(d$codes[[1]]$code, "111")
    # Duplicate carries the expression too, not just the snapshot.
    session$setInputs("codelist_1-duplicate" = 1)
    expect_true(isolate(items$data())[[2]]$concept_set_expression[[1]]$descendants)
  })
})

test_that("codelists: the card collects an optional vocabulary version", {
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_x",
                      "codelist_1-vocabulary_version" = "v5.0 31-AUG-23")
    expect_identical(isolate(items$data())[[1]]$vocabulary_version, "v5.0 31-AUG-23")
    session$setInputs(add = 2)
    session$setInputs("codelist_2-name" = "cs_y")
    expect_true(is.na(isolate(items$data())[[2]]$vocabulary_version))
  })
})
