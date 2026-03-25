# ============================================================================
# test-module-m1-compute-countries.R - Tests for M1 compute countries
# ============================================================================

context("module m1 - compute countries")

test_that("compute_m1_countries exists", {
  expect_true(is.function(compute_m1_countries))
})

test_that("compute_m1_countries returns a list", {
  result <- compute_m1_countries()
  expect_type(result, "list")
})

test_that("compute_m1_countries contains status", {
  result <- compute_m1_countries()
  expect_true("status" %in% names(result))
})
