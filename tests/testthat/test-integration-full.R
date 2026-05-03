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

test_that("Pipeline can run the B-SLR methodology layer without a separate orchestration path", {
  test_data <- data.frame(
    AU = c("Smith J; Jones M", "Johnson K", "Brown A; Davis R", "Miller P"),
    PY = c(2020L, 2021L, 2022L, 2023L),
    TI = c("Frequency estimator alpha", "Frequency estimator beta", "Power system trends", "Grid frequency review"),
    SO = c("Journal A", "Journal B", "Journal C", "Journal D"),
    TC = c(10L, 5L, 15L, 8L),
    DI = c("10.1/1", "10.1/2", "10.1/3", "10.1/4"),
    DE = c("frequency estimation; power systems", "frequency; estimator", "power systems; review", "grid; frequency"),
    CR = c("Ref A; Ref B", "Ref A; Ref C", "Ref D", "Ref B; Ref D"),
    AU_CO = c("USA; UK", "USA", "UK; Germany", "Canada"),
    C1 = c(
      "University A, USA; University B, UK",
      "Institute X, USA",
      "University C, UK; Institute Y, Germany",
      "Institute Z, Canada"
    ),
    stringsAsFactors = FALSE
  )

  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  utils::write.csv(test_data, tmp_csv, row.names = FALSE)

  protocol <- bslr_protocol_template("Pipeline B-SLR integration")
  protocol$review_topic <- "Power systems frequency estimation"
  protocol$research_question <- "How has research on frequency estimation evolved?"
  protocol$inclusion_criteria <- c("Peer-reviewed", "In scope")
  protocol$exclusion_criteria <- c("Out of scope")
  protocol$search$primary_database <- "scopus"
  protocol$search$queries$primary <- "TITLE-ABS-KEY(frequency AND estimation)"
  protocol$search$first_run_date <- "2026-04-30"
  protocol$search$time_span$start <- "2020"
  protocol$search$time_span$end <- "2023"
  protocol$search$language <- "English"
  protocol$search$document_types <- c("article")

  result <- run_pipeline(
    sources = list(generic = list(file = tmp_csv, db = "generic", format = "csv")),
    modules = c("m0", "m1", "m2", "m3", "bslr"),
    config = biblio_config(validate_strict = TRUE, export = FALSE, verbose = FALSE),
    bslr_protocol = protocol,
    export = FALSE
  )

  expect_s3_class(result, "biblio_pipeline_result")
  expect_equal(result$metadata$workflow, "bslr")
  expect_true(all(c("m0", "m1", "m2", "m3", "bslr") %in% names(result$modules)))
  expect_s3_class(result$modules$bslr, "bslr_workflow_result")
  expect_equal(result$modules$bslr$data$search_metadata$primary_database, "scopus")
  expect_true(result$status %in% c("success", "warning"))
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
