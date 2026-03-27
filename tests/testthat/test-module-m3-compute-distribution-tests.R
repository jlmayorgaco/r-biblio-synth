# ============================================================================
# test-module-m3-compute-distribution-tests.R
# ============================================================================
test_that("m3_compute_distribution_tests returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_distribution_tests(pd)
  expect_type(result, "list")
  expect_true(all(c("production_distribution", "citations_distribution",
                    "distribution_summary", "status") %in% names(result)))
})

test_that("distribution summary has mean, sd, skewness", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_distribution_tests(pd)
  s <- result$distribution_summary$production
  expect_true(all(c("mean", "sd", "skewness") %in% names(s)))
})

test_that("coefficient of variation is non-negative when mean > 0", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_distribution_tests(pd)
  cv <- result$distribution_summary$production$cv
  expect_true(is.na(cv) || cv >= 0)
})

test_that("shapiro_p is either NA (too few obs) or a valid p-value in [0, 1]", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_distribution_tests(pd)
  p <- result$distribution_summary$production$shapiro_p
  expect_true(is.na(p) || (p >= 0 && p <= 1))
})

test_that("m3_compute_distribution_tests returns error for empty data", {
  empty_pd <- list(country_summary = tibble::tibble())
  result   <- m3_compute_distribution_tests(empty_pd)
  expect_match(result$status, "error")
})
