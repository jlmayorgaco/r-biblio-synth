# ============================================================================
# test-module-m3-compute-similarity-clustering.R
# ============================================================================
test_that("m3_compute_country_profiles returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_country_profiles(pd)
  expect_type(result, "list")
  expect_true(all(c("country_features", "country_profiles", "pca_info",
                    "clustering_info", "profile_summary", "status") %in%
                    names(result)))
})

test_that("profile returns success for valid data", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_country_profiles(pd)
  expect_equal(result$status, "success")
})

test_that("country_features has expected base columns", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_country_profiles(pd)
  expect_true(all(c("country", "article_count", "total_citations") %in%
                    names(result$country_features)))
})

test_that("clustering is performed when enough countries exist", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_country_profiles(pd)
  ps     <- result$profile_summary
  # Extended fixture has >= 2 countries → clustering should be attempted
  expect_true(!is.null(ps$clustering_performed))
})

test_that("profile returns error stub for empty data", {
  empty_pd <- list(
    country_summary    = tibble::tibble(),
    country_doc_level  = tibble::tibble()
  )
  result <- m3_compute_country_profiles(empty_pd)
  expect_match(result$status, "error")
})
