# ============================================================================
# test-module-m1-compute-overview.R - Tests for M1 compute overview
# ============================================================================

context("module m1 - compute overview")

test_that("compute_m1_overview exists", {
  expect_true(is.function(compute_m1_overview))
})

test_that("compute_m1_overview returns a list on extended fixture", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_overview(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_overview returns error on empty input", {
  result <- compute_m1_overview(data.frame())
  expect_equal(result$status, "error")
})
