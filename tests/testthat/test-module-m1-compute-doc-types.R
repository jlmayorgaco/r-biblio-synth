# ============================================================================
# test-module-m1-compute-doc-types.R
# ============================================================================



test_that("compute_m1_doc_types returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_doc_types(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_doc_types returns doc_type_table", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_doc_types(fixture)
  expect_true("doc_type_table" %in% names(result))
  expect_true(nrow(result$doc_type_table) > 0)
})
