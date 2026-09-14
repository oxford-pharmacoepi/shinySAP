skip_without_app()

test_that("blank inputs become absent values", {
  expect_null(app("chr_or_null")(""))
  expect_null(app("chr_or_null")("  "))
  expect_null(app("chr_or_null")(NULL))
  expect_identical(app("chr_or_null")(" x "), "x")
  expect_null(app("num_or_null")(NA))
  expect_null(app("num_or_null")("abc"))
  expect_identical(app("num_or_null")("365"), 365)
  expect_identical(app("chr_vec")(list("a", "", NA, " b ")), c("a", "b"))
  expect_identical(app("compact")(list(a = 1, b = NULL, c = "x")), list(a = 1, c = "x"))
})

test_that("ids are minted one past the highest in use", {
  next_id <- app("next_item_id")
  expect_identical(next_id("coh", character(0)), "coh_1")
  expect_identical(next_id("coh", c("coh_1", "coh_7", "cl_9", "coh_x")), "coh_8")
  expect_identical(next_id("ds", c("coh_3")), "ds_1")
})

test_that("date ranges pair two inputs and split back", {
  dr <- app("date_range_value")("2010-01-01", "")
  expect_s3_class(dr, "Date")
  expect_true(is.na(dr[[2]]))
  expect_null(app("date_range_value")("", NULL))
  expect_identical(app("date_bound")(dr, 1), "2010-01-01")
  expect_null(app("date_bound")(dr, 2))
  expect_null(app("as_date_value")("not a date"))
})

test_that("the file base name follows the study id and version", {
  base <- app("sap_file_base")
  expect_identical(base(list(study_id = "C1-001", version = "v1.0.0")), "sap-c1-001-v1.0.0")
  expect_identical(base(list(title = "SAP for X")), "sap-for-x")
  expect_identical(base(list()), "sap-untitled")
  expect_identical(app("working_sap_path")(list(study_id = "A", version = "v2"), "out"), "out/sap-a-v2.json")
})

test_that("an untouched SAP is empty, an authored one is not", {
  empty <- newSap(list(study = list(version = "v1.0.0")), validate = FALSE)
  expect_true(app("sap_is_empty")(empty))
  expect_false(app("sap_is_empty")(newSap(list(study = list(title = "x")), validate = FALSE)))
  expect_false(app("sap_is_empty")(fixture_sap()))
})

test_that("prefiller returns the stored value or the default", {
  pf <- app("prefiller")(list(a = "x", b = NA, c = list(1, 2)))
  expect_identical(pf("a"), "x")
  expect_identical(pf("b", "d"), "d")
  expect_identical(pf("missing", 3), 3)
  expect_identical(pf("c"), list(1, 2))
})
