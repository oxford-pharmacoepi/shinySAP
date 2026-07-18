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
