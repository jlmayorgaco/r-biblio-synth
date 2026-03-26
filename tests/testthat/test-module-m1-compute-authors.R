# ============================================================================
# test-module-m1-compute-authors.R
# ============================================================================



test_that("compute_m1_authors returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_authors(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_authors returns top_authors table", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_authors(fixture)
  expect_true("top_authors" %in% names(result))
  expect_true(nrow(result$top_authors) > 0)
})
