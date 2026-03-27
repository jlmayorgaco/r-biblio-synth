# ============================================================================
# test-module-m3-compute-citations.R
# ============================================================================
test_that("m3_compute_citations returns list with expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_citations(pd)
  expect_type(result, "list")
  expect_true(all(c("country_citations", "top_countries_by_citations",
                    "top_countries_by_avg_citations", "citation_summary",
                    "status") %in% names(result)))
})

test_that("m3_compute_citations returns success for valid data", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_citations(pd)
  expect_equal(result$status, "success")
})

test_that("country_citations has average_citations column", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_citations(pd)
  expect_true("average_citations" %in% names(result$country_citations))
})

test_that("average_citations is non-negative", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_citations(pd)
  expect_true(all(result$country_citations$average_citations >= 0, na.rm = TRUE))
})

test_that("citation Gini is between 0 and 1", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_citations(pd)
  gini   <- result$citation_summary$gini_citations
  expect_true(is.na(gini) || (gini >= 0 && gini <= 1))
})

test_that("m3_compute_citations returns error stub for empty data", {
  empty_pd <- list(country_summary = tibble::tibble())
  result   <- m3_compute_citations(empty_pd)
  expect_match(result$status, "error")
})
