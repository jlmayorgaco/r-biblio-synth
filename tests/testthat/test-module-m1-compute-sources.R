# ============================================================================
# test-module-m1-compute-sources.R
# ============================================================================

context("module m1 - compute sources")

test_that("compute_m1_sources returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_sources(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_sources returns top_sources", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_sources(fixture)
  expect_true("top_sources" %in% names(result))
})
