# ============================================================================
# test-module-m1-compute-keywords.R - Tests for M1 compute keywords
# ============================================================================

context("module m1 - compute keywords")

test_that("compute_m1_keywords exists", {
  expect_true(is.function(compute_m1_keywords))
})

test_that("compute_m1_keywords returns a list", {
  result <- compute_m1_keywords()
  expect_type(result, "list")
})

test_that("compute_m1_keywords contains status", {
  result <- compute_m1_keywords()
  expect_true("status" %in% names(result))
})
