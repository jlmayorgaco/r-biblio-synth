# ============================================================================
# test-module-m1-compute-sources.R - Tests for M1 compute sources
# ============================================================================

context("module m1 - compute sources")

test_that("compute_m1_sources exists", {
  expect_true(is.function(compute_m1_sources))
})

test_that("compute_m1_sources returns a list", {
  result <- compute_m1_sources()
  expect_type(result, "list")
})

test_that("compute_m1_sources contains status", {
  result <- compute_m1_sources()
  expect_true("status" %in% names(result))
})
