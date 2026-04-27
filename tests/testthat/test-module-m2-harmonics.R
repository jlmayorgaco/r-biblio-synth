# ============================================================================
# test-module-m2-harmonics.R - Tests for M2 harmonics
# ============================================================================

test_that("compute_m2_harmonics returns list with status", {
  data <- data.frame(Year = 1990:2020, Articles = sin(seq(0, 4*pi, length.out = 31)) * 10 + 50)
  result <- compute_m2_harmonics(data)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("FFT returns correct structure", {
  y <- sin(2 * pi * 0.5 * 1:100)
  fft_result <- m2_perform_fft(y, dt = 1)
  expect_true("frequencies" %in% names(fft_result))
  expect_true("magnitude" %in% names(fft_result))
  expect_true("phase" %in% names(fft_result))
  expect_equal(length(fft_result$frequencies), length(fft_result$magnitude))
})

test_that("FFT frequencies are correct with time step", {
  y <- sin(2 * pi * 0.5 * 1:100)
  fft_result <- m2_perform_fft(y, dt = 1)
  expect_equal(fft_result$frequencies[1], 0)
  expect_true(all(diff(fft_result$frequencies) >= 0))
})

test_that("Harmonic regression returns R2 values", {
  data <- data.frame(Year = 1990:2020, Articles = sin(seq(0, 4*pi, length.out = 31)) * 10 + 50)
  result <- compute_m2_harmonics(data)
  expect_true("r_squared_table" %in% names(result))
  if (nrow(result$r_squared_table) > 0) {
    expect_true("Frequency" %in% names(result$r_squared_table))
    expect_true("R2" %in% names(result$r_squared_table))
    expect_true(all(result$r_squared_table$R2 >= 0))
    expect_true(all(result$r_squared_table$R2 <= 1))
  }
})

test_that("render_m2_harmonics exposes legacy-inspired fitness plots", {
  data <- data.frame(Year = 1990:2020, Articles = sin(seq(0, 4*pi, length.out = 31)) * 10 + 50)
  result <- compute_m2_harmonics(data)
  rendered <- render_m2_harmonics(result)

  expect_equal(rendered$status, "success")
  expect_true("r2_vs_frequency" %in% names(rendered$plots))
  expect_true("data_top_periods" %in% names(rendered$plots))
  expect_s3_class(rendered$plots$r2_vs_frequency, "ggplot")
})

test_that("Lomb-Scargle handles errors gracefully", {
  data <- data.frame(Year = 1990:2005, Articles = sin(seq(0, 4*pi, length.out = 16)) * 10 + 50)
  result <- compute_m2_harmonics(data)
  expect_equal(result$status, "success")
  expect_true("lomb" %in% names(result))
})

test_that("Harmonics rejects insufficient data", {
  data <- data.frame(Year = 1:5, Articles = 1:5)
  result <- compute_m2_harmonics(data)
  expect_equal(result$status, "error")
})
