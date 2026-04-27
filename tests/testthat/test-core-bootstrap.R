# ============================================================================
# test-core-bootstrap.R - Tests for bootstrap CI functions
# ============================================================================

context("Bootstrap Confidence Intervals")

test_that("bootstrap_ci returns correct structure for percentile method", {
  set.seed(42)
  data <- rnorm(100, mean = 5, sd = 2)
  
  result <- bootstrap_ci(
    data = data,
    statistic = mean,
    R = 100,
    conf_level = 0.95,
    method = "percentile",
    seed = 42
  )
  
  expect_true(is.list(result))
  expect_true("estimate" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("status" %in% names(result))
  
  expect_true(result$status == "success")
  expect_true(result$estimate > result$lower)
  expect_true(result$estimate < result$upper)
  expect_true(result$n_boot == 100)
})

test_that("bootstrap_ci handles NA values gracefully", {
  set.seed(42)
  data <- c(rnorm(50), NA, NA, rnorm(50))
  
  result <- bootstrap_ci(
    data = data,
    statistic = mean,
    R = 100,
    conf_level = 0.95,
    method = "percentile",
    seed = 42
  )
  
  expect_true(result$status == "success")
  expect_false(is.na(result$estimate))
  expect_false(is.na(result$lower))
  expect_false(is.na(result$upper))
})

test_that("bootstrap_ci BCA method works for median", {
  set.seed(42)
  data <- rnorm(200, mean = 10, sd = 3)
  
  result <- bootstrap_ci(
    data = data,
    statistic = median,
    R = 500,
    conf_level = 0.95,
    method = "bca",
    seed = 42
  )
  
  expect_true(result$status == "success")
  expect_true(abs(result$estimate - median(data)) < 0.001)
})

test_that("bootstrap_ci fails gracefully with insufficient data", {
  data <- c(1, 2)  # Too few points
  
  result <- bootstrap_ci(
    data = data,
    statistic = mean,
    R = 100,
    conf_level = 0.95
  )
  
  # Should still work with minimal data
  expect_true(is.list(result))
})

test_that("bootstrap_ci works with data frames", {
  set.seed(42)
  df <- data.frame(
    value = rnorm(100, mean = 50, sd = 10),
    group = rep(1:2, each = 50)
  )
  
  # Bootstrap the mean of value column
  result <- bootstrap_ci(
    data = df,
    statistic = function(d) mean(d$value),
    R = 100,
    seed = 42
  )
  
  expect_true(result$status == "success")
  expect_true(result$estimate > 45)
  expect_true(result$estimate < 55)
})

test_that("bootstrap_ci handles statistic that returns NA", {
  data <- c(1, 2, 3)
  
  # Statistic that returns NA
  result <- bootstrap_ci(
    data = data,
    statistic = function(x) NA_real_,
    R = 10
  )
  
  expect_true(result$status == "error" || is.na(result$estimate))
})

test_that("bootstrap_mean returns correct structure", {
  set.seed(42)
  data <- rnorm(100, mean = 100, sd = 15)
  
  result <- bootstrap_mean(data, R = 500, conf_level = 0.95, seed = 42)
  
  expect_true(is.list(result))
  expect_true(result$status == "success")
  expect_true(result$estimate > 95)
  expect_true(result$estimate < 105)
  expect_true(result$lower < result$estimate)
  expect_true(result$upper > result$estimate)
})

test_that("bootstrap_median returns correct structure", {
  set.seed(42)
  data <- rlnorm(100, meanlog = 2, sdlog = 0.5)  # Skewed distribution
  
  result <- bootstrap_median(data, R = 500, seed = 42)
  
  expect_true(result$status == "success")
  expect_true(result$estimate > 0)
})

test_that("bootstrap_proportion works for binary data", {
  set.seed(42)
  successes <- 75
  trials <- 100
  
  result <- bootstrap_proportion(successes, trials, R = 1000, seed = 42)
  
  expect_true(result$status == "success")
  expect_true(result$estimate > 0.7)
  expect_true(result$estimate < 0.8)
  expect_true(result$lower > 0)
  expect_true(result$upper < 1)
})

test_that("bootstrap_gini returns valid Gini coefficient", {
  set.seed(42)
  # Create data with known Gini
  data <- c(1, 1, 1, 10, 10, 10)  # Some inequality
  
  result <- bootstrap_gini(data, R = 500, seed = 42)
  
  expect_true(result$status == "success")
  expect_true(result$estimate >= 0)
  expect_true(result$estimate <= 1)
  # Gini should be non-zero for unequal data
  expect_true(result$estimate > 0)
})

test_that("safe_divide prevents division by zero", {
  expect_equal(safe_divide(10, 0), 0)
  expect_equal(safe_divide(10, 0, default = NA), NA)
  expect_equal(safe_divide(10, 5), 2)
})

test_that("safe_proportion handles edge cases", {
  expect_equal(safe_proportion(5, 10), 0.5)
  expect_equal(safe_proportion(5, 0), 0)
  expect_equal(safe_proportion(10, 5), 1)  # Capped at 100%
})
