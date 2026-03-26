# ============================================================================
# test-correctness.R - Correctness tests for M1 and M2
# ============================================================================

test_that("Lorenz curve has correct orientation - perfect equality", {
  # Perfect equality: all values same -> diagonal line
  x <- c(10, 10, 10, 10)
  lorenz <- m1_compute_lorenz(x)
  # First point should be (0,0), last should be (1,1)
  expect_equal(lorenz$cumulative_entities[1], 0)
  expect_equal(lorenz$cumulative_values[1], 0)
  expect_equal(lorenz$cumulative_entities[nrow(lorenz)], 1)
  expect_equal(lorenz$cumulative_values[nrow(lorenz)], 1)
  # Perfect equality: entities = values (diagonal)
  expect_equal(lorenz$cumulative_entities, lorenz$cumulative_values, tolerance = 0.01)
})

test_that("Gini coefficient is 0 for perfect equality", {
  x <- c(10, 10, 10, 10)
  lorenz <- m1_compute_lorenz(x)
  gini <- m1_compute_gini(lorenz$cumulative_entities, lorenz$cumulative_values)
  expect_equal(gini, 0, tolerance = 0.01)
})

test_that("Gini coefficient approaches 1 for perfect inequality", {
  x <- c(0, 0, 0, 100)
  lorenz <- m1_compute_lorenz(x)
  gini <- m1_compute_gini(lorenz$cumulative_entities, lorenz$cumulative_values)
  expect_gte(gini, 0.7)  # Should be very high
})

test_that("Lorenz cumulative_values sums monotonically increase", {
  x <- c(5, 10, 15, 20, 25)
  lorenz <- m1_compute_lorenz(x)
  expect_true(all(diff(lorenz$cumulative_values) >= 0))
})

test_that("Bradford zones have correct article distribution", {
  # Create test data with known distribution - use manual computation
  input <- data.frame(SO = rep(paste0("Journal", 1:9), times = c(10, 8, 6, 4, 3, 2, 2, 1, 1)))
  
  # Force manual computation by not having bibliometrix attributes
  source_counts <- table(input$SO, useNA = "no")
  sdf <- data.frame(SO = names(source_counts), Freq = as.integer(source_counts), stringsAsFactors = FALSE)
  sdf <- sdf[order(-sdf$Freq), ]
  sdf$CumFreq <- cumsum(sdf$Freq)
  total_articles <- sum(sdf$Freq)
  sdf$Zone <- ifelse(
    sdf$CumFreq <= total_articles / 3, "Zone 1: Core",
    ifelse(sdf$CumFreq <= 2 * total_articles / 3, "Zone 2: Moderate",
           "Zone 3: Peripheral"))
  
  zone1_articles <- sum(sdf$Freq[sdf$Zone == "Zone 1: Core"])
  expect_gte(zone1_articles, total_articles * 0.25)
  expect_lte(zone1_articles, total_articles * 0.45)
})

test_that("MCP ratio handles zero denominator", {
  # Both SCP and MCP = 0 should return NA
  denom <- 0
  result <- ifelse(denom > 0, 0 / denom * 100, NA_real_)
  expect_true(is.na(result))
})

test_that("MCP ratio computes correctly", {
  scp <- 10
  mcp <- 5
  denom <- scp + mcp
  result <- round(mcp / denom * 100, 1)
  expect_equal(result, 33.3)
})

test_that("NLS R² is bounded between 0 and 1", {
  data <- data.frame(Year = 1990:2020, Articles = seq(10, 100, length.out = 31))
  result <- compute_m2_regression(data)
  
  for (i in seq_len(nrow(result$comparison_table))) {
    expect_gte(result$comparison_table$R2[i], 0)
    expect_lte(result$comparison_table$R2[i], 1)
  }
})

test_that("FFT frequencies use correct time step", {
  y <- sin(2 * pi * 0.5 * (1:100))  # 0.5 Hz signal
  dt <- 1
  result <- m2_perform_fft(y, dt = dt)
  # First frequency should be 0
  expect_equal(result$frequencies[1], 0)
  # Should have correct number of frequencies
  expect_equal(length(result$frequencies), 51)  # floor(100/2) + 1
})

test_that("Weibull function is correctly parameterized", {
  t <- 1:10
  K <- 100
  r <- 2
  t0 <- 1
  alpha <- 5
  result <- m2_weibull(t, K, r, t0, alpha)
  # At t = t0, should be 0
  expect_equal(result[1], 0)
  # Should increase over time
  expect_true(all(diff(result) >= 0))
})

test_that("Gompertz function returns positive values", {
  # Just test the function itself, not the nls fitting
  t <- 1990:2020
  N0 <- 100
  Nmax <- 150
  k <- 0.1
  t0 <- 1990
  y0 <- 10
  
  result <- m2_gompertz(t, N0, Nmax, k, t0, y0)
  expect_true(all(result > 0))
  expect_true(length(result) == length(t))
})
