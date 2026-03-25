# ============================================================================
# test-module-m1-compute-overview.R - Tests for M1 compute overview
# ============================================================================

context("module m1 - compute overview")

test_that("compute_m1_overview exists", {
  expect_true(is.function(compute_m1_overview))
})

test_that("compute_m1_overview returns a list", {
  result <- compute_m1_overview()
  expect_type(result, "list")
})

test_that("compute_m1_overview contains status", {
  result <- compute_m1_overview()
  expect_true("status" %in% names(result))
})
