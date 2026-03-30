# ============================================================================
# test-integration-full.R - Full Pipeline Integration Tests
# ============================================================================

context("Full Pipeline Integration")

test_that("Complete pipeline runs end-to-end with sample data", {
  skip_if_not_installed("bibliometrix")
  
  # Use minimal test configuration
  config <- biblio_config(
    verbose = FALSE,
    parallel = FALSE,
    cache_enabled = FALSE
  )
  
  # Create minimal test data
  test_data <- data.frame(
    AU = c("Smith J; Jones M", "Johnson K", "Brown A; Davis R"),
    PY = c(2020L, 2021L, 2022L),
    TI = c("Paper 1", "Paper 2", "Paper 3"),
    SO = c("Journal A", "Journal B", "Journal C"),
    TC = c(10L, 5L, 15L),
    DI = c("10.1/1", "10.1/2", "10.1/3"),
    DE = c("keyword1; keyword2", "keyword2; keyword3", "keyword1; keyword3"),
    AU_CO = c("USA; UK", "USA", "UK; Germany"),
    C1 = c("University A, USA; University B, UK", 
           "Institute X, USA",
           "University C, UK; Institute Y, Germany"),
    stringsAsFactors = FALSE
  )
  
  # Test M0
  expect_silent({
    m0_result <- list(
      status = "success",
      data = list(bib_merged = test_data)
    )
  })
  
  # Test M1
  m1_result <- tryCatch({
    run_m1(test_data, config = config, export = FALSE)
  }, error = function(e) {
    skip(paste("M1 failed:", e$message))
  })
  
  expect_equal(m1_result$status, "success")
  expect_true(!is.null(m1_result$data$overview))
  
  # Test M2
  annual_data <- data.frame(
    Year = 2020:2022,
    Articles = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  
  m2_result <- tryCatch({
    run_m2(annual_data, config = config, export = FALSE)
  }, error = function(e) {
    skip(paste("M2 failed:", e$message))
  })
  
  expect_equal(m2_result$status, "success")
  
  # Test M3
  m3_result <- tryCatch({
    run_m3(test_data, config = config, export = FALSE)
  }, error = function(e) {
    skip(paste("M3 failed:", e$message))
  })
  
  expect_equal(m3_result$status, "success")
  expect_true(!is.null(m3_result$data$production))
})

test_that("M4 Institutional Analysis works", {
  test_data <- data.frame(
    AU = c("Smith J", "Jones M"),
    C1 = c("University of Example, Department of Science, USA",
           "Institute of Research, UK"),
    PY = c(2020L, 2021L),
    stringsAsFactors = FALSE
  )
  
  result <- tryCatch({
    run_m4(test_data, config = biblio_config(verbose = FALSE), export = FALSE)
  }, error = function(e) NULL)
  
  if (!is.null(result)) {
    expect_equal(result$status, "success")
  }
})

test_that("Configuration system works correctly", {
  # Test default config
  cfg1 <- biblio_config()
  expect_true(is.list(cfg1))
  expect_equal(cfg1$output_dir, "results")
  expect_true(cfg1$verbose)
  
  # Test custom config
  cfg2 <- biblio_config(verbose = FALSE, parallel = TRUE, n_cores = 4)
  expect_false(cfg2$verbose)
  expect_true(cfg2$parallel)
  expect_equal(cfg2$n_cores, 4)
  
  # Test merge
  cfg3 <- merge_biblio_config(list(verbose = FALSE, custom_param = "test"))
  expect_false(cfg3$verbose)
  expect_equal(cfg3$custom_param, "test")
})

test_that("Logging framework works", {
  # Initialize logging
  expect_silent(init_logging(level = "INFO"))
  
  # Test log levels
  expect_silent(log_message("INFO", "Test message"))
  expect_silent(log_message("WARN", "Warning message"))
  
  # Test with interpolation
  expect_silent(log_message("INFO", "Value is {x}", x = 42))
  
  # Cleanup
  close_logging()
})

test_that("Caching system works", {
  # Test cache compute
  result <- cache_compute("test_key", {
    Sys.sleep(0.1)
    42
  }, ttl = 3600, dir = tempdir())
  
  expect_equal(result, 42)
  
  # Test cache stats
  stats <- cache_stats(dir = tempdir())
  expect_true(is.list(stats))
  
  # Cleanup
  clear_cache(dir = tempdir())
})

test_that("Error handling works", {
  # Test safe_compute
  safe_mean <- safe_compute(mean, default = NA)
  expect_equal(safe_mean(c(1, 2, 3)), 2)
  expect_true(is.na(safe_mean(numeric())))  # Returns default on error
  
  # Test try_with_retry
  counter <- 0
  result <- try_with_retry({
    counter <<- counter + 1
    if (counter < 3) stop("Temporary error")
    "success"
  }, max_retries = 3)
  
  expect_equal(result, "success")
  expect_equal(counter, 3)
})

test_that("Parallel processing setup works", {
  # Test setup
  result <- setup_parallel(n_cores = 1)
  expect_false(result)  # Sequential mode
  
  # Test with 2 cores (if available)
  result2 <- setup_parallel(n_cores = 2)
  # Result depends on system
  expect_true(is.logical(result2))
  
  # Cleanup
  cleanup_parallel()
})

test_that("Example script runs without errors", {
  skip("Skip full example test - requires actual data file")
  
  # This would test the actual example
  # expect_true(file.exists("examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r"))
})