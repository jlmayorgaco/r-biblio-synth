# ============================================================================
# test-module-m1-compute-keywords.R
# ============================================================================

context("module m1 - compute keywords")

test_that("compute_m1_keywords returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_keywords(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_keywords returns top_keywords", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_keywords(fixture)
  expect_true("top_keywords" %in% names(result))
})
