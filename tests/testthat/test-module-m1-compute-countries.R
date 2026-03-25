# ============================================================================
# test-module-m1-compute-countries.R
# ============================================================================

context("module m1 - compute countries")

test_that("compute_m1_countries returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_countries(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_countries returns top_countries_by_articles", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_countries(fixture)
  expect_true("top_countries_by_articles" %in% names(result))
})
