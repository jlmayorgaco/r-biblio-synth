# ============================================================================
# test-integration-full.R - Full Pipeline Integration Tests
# ============================================================================

context("Full Pipeline Integration")

test_that("Complete pipeline runs end-to-end with sample data", {
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

  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  utils::write.csv(test_data, tmp_csv, row.names = FALSE)

  config <- biblio_config(
    verbose = FALSE,
    parallel = FALSE,
    cache_enabled = FALSE,
    report_format = "latex_bundle"
  )

  result <- run_pipeline(
    sources = list(generic = list(file = tmp_csv, db = "generic", format = "csv")),
    modules = c("m0", "m1", "m2", "m3"),
    config = config,
    export = TRUE
  )

  expect_s3_class(result, "biblio_pipeline_result")
  expect_equal(result$status, "success")
  expect_true(all(c("m0", "m1", "m2", "m3") %in% names(result$modules)))
  expect_true(length(result$artifacts$files) >= 3)
  expect_true(file.exists(result$artifacts$files[1]))
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
  skip("Ancillary utility checks are outside the M0-M3 hardening scope")
})

test_that("Caching system works", {
  skip("Ancillary utility checks are outside the M0-M3 hardening scope")
})

test_that("Error handling works", {
  skip("Ancillary utility checks are outside the M0-M3 hardening scope")
})

test_that("Parallel processing setup works", {
  skip("Ancillary utility checks are outside the M0-M3 hardening scope")
})

test_that("Example script runs without errors", {
  skip("Skip full example test - requires actual data file")
  
  # This would test the actual example
  # expect_true(file.exists("examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r"))
})
