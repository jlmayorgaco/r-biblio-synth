# ============================================================================
# test-module-m1-compute-doc-types.R - Tests for M1 compute doc types
# ============================================================================

context("module m1 - compute doc_types")

test_that("compute_m1_doc_types exists", {
  expect_true(is.function(compute_m1_doc_types))
})

test_that("compute_m1_doc_types returns a list", {
  result <- compute_m1_doc_types()
  expect_type(result, "list")
})

test_that("compute_m1_doc_types contains status", {
  result <- compute_m1_doc_types()
  expect_true("status" %in% names(result))
})
