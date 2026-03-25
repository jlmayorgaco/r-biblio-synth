# ============================================================================
# test-module-m1-compute-authors.R - Tests for M1 compute authors
# ============================================================================

context("module m1 - compute authors")

test_that("compute_m1_authors exists", {
  expect_true(is.function(compute_m1_authors))
})

test_that("compute_m1_authors returns a list", {
  result <- compute_m1_authors()
  expect_type(result, "list")
})

test_that("compute_m1_authors contains status", {
  result <- compute_m1_authors()
  expect_true("status" %in% names(result))
})
