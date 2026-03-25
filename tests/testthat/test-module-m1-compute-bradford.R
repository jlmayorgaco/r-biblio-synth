# ============================================================================
# test-module-m1-compute-bradford.R - Tests for M1 compute bradford
# ============================================================================

context("module m1 - compute bradford")

test_that("compute_m1_bradford exists", {
  expect_true(is.function(compute_m1_bradford))
})

test_that("compute_m1_bradford returns a list", {
  result <- compute_m1_bradford()
  expect_type(result, "list")
})

test_that("compute_m1_bradford contains status", {
  result <- compute_m1_bradford()
  expect_true("status" %in% names(result))
})
