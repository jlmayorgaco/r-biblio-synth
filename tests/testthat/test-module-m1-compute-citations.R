# ============================================================================
# test-module-m1-compute-citations.R - Tests for M1 compute citations
# ============================================================================

context("module m1 - compute citations")

test_that("compute_m1_citations exists", {
  expect_true(is.function(compute_m1_citations))
})

test_that("compute_m1_citations returns a list", {
  result <- compute_m1_citations()
  expect_type(result, "list")
})

test_that("compute_m1_citations contains status", {
  result <- compute_m1_citations()
  expect_true("status" %in% names(result))
})
