# ============================================================================
# test-module-m1-compute-citations.R
# ============================================================================

context("module m1 - compute citations")

test_that("compute_m1_citations returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_citations(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_citations returns top_cited_documents", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_citations(fixture)
  expect_true("top_cited_documents" %in% names(result))
})
