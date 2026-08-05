# Codelists: named entities with a provenance line and an optional category.
# The SAP names a codelist; the codes themselves live with the study, not in
# the plan, so the card carries no upload and no code state.

test_that("codelists: the card collects name, category and description", {
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_mi",
                      "codelist_1-category" = "Index event",
                      "codelist_1-description" = "CodelistGenerator, ICD-10 I21")
    d <- isolate(items$data())[[1]]
    expect_identical(d$name, "cs_mi")
    expect_identical(d$category, "Index event")
    expect_identical(d$description, "CodelistGenerator, ICD-10 I21")
  })
})

# 0.4.17: an optional category the document groups by. Blank stays null.
test_that("codelists: an unset category stays null rather than defaulting", {
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_x")
    d <- isolate(items$data())[[1]]
    expect_true(is.na(d$category))
    expect_true(is.na(d$description))
  })
})

# The fields themselves are inputs, which testServer cannot see prefilled --
# they reach the card as UI values, not server state -- so what load() can be
# held to here is one card per saved codelist, replacing whatever was there.
test_that("codelists: load replaces the cards with one per saved codelist", {
  testServer(codelists_server, {
    session$setInputs(add = 1)
    load(list(list(name = "cs_x", category = "Covariate", description = "ATC J07BB"),
              list(name = "cs_y")))
    expect_identical(isolate(items$count()), 2L)
  })
})

test_that("codelists: names() reports the defined names for the reference check", {
  testServer(codelists_server, {
    session$setInputs(add = 1)
    session$setInputs("codelist_1-name" = "cs_b")
    session$setInputs(add = 2)
    session$setInputs("codelist_2-name" = "cs_a")
    expect_identical(isolate(names_r()), c("cs_a", "cs_b"))
  })
})
