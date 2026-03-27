# ============================================================================
# test-module-m3-compute-production.R
# ============================================================================
test_that("m3_compute_production returns list with expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_production(pd)
  expect_type(result, "list")
  expect_true(all(c("country_production", "top_countries_by_production",
                    "production_summary", "status") %in% names(result)))
})

test_that("m3_compute_production returns success for valid data", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_production(pd)
  expect_equal(result$status, "success")
})

test_that("country_production is a tibble with expected columns", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_production(pd)
  expect_s3_class(result$country_production, "data.frame")
  expect_true(all(c("country", "article_count", "share") %in%
                    names(result$country_production)))
})

test_that("production_summary contains correct totals", {
  pd     <- make_m3_prepared_minimal()
  result <- m3_compute_production(pd)
  expect_gte(result$production_summary$total_articles, 1L)
  expect_gte(result$production_summary$total_countries, 1L)
})

test_that("Gini coefficient is between 0 and 1", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_production(pd)
  gini   <- result$production_summary$gini_articles
  expect_true(is.na(gini) || (gini >= 0 && gini <= 1))
})

test_that("m3_compute_production returns error stub for empty data", {
  empty_pd <- list(country_summary = tibble::tibble())
  result   <- m3_compute_production(empty_pd)
  expect_match(result$status, "error")
})
