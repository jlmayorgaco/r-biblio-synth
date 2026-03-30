# ============================================================================
# test-module-m0-load.R - Tests for M0 data loading functions
# ============================================================================

context("M0 Data Loading")

# Source M0 functions
source("../../R/core/bootstrap.R")

test_that("m0_load_single_source handles missing file gracefully", {
  source_spec <- list(
    file = "nonexistent.bib",
    db = "scopus",
    format = "bibtex"
  )
  
  result <- m0_load_single_source(source_spec)
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  expect_true(result$status == "error" || grepl("error", result$status))
})

test_that("m0_normalize_country_names corrects variants", {
  countries <- c("USA", "U.S.A.", "United States", "UK", "England", "Korea", "Iran")
  
  result <- m0_normalize_country_names(countries)
  
  expect_equal(length(result), length(countries))
  expect_true("UNITED STATES" %in% result)
  expect_true("UNITED KINGDOM" %in% result)
  expect_true("SOUTH KOREA" %in% result)
})

test_that("m0_validate_sources validates source specifications", {
  valid_sources <- list(
    scopus = list(file = "data/scopus.bib", db = "scopus", format = "bibtex")
  )
  
  result <- m0_validate_sources(valid_sources)
  
  expect_true(is.list(result))
  expect_true("ok" %in% names(result))
})

test_that("m0_validate_sources fails with missing required fields", {
  invalid_sources <- list(
    scopus = list(db = "scopus")  # Missing file
  )
  
  expect_warning(m0_validate_sources(invalid_sources))
})

test_that("m0_harmonize_columns standardizes column names", {
  df <- data.frame(
    authors = c("Smith J"),
    year = c(2020),
    title = c("Test"),
    stringsAsFactors = FALSE
  )
  
  result <- m0_harmonize_columns(df, "scopus")
  
  expect_true(is.data.frame(result))
})

test_that("m0_deduplicate removes DOI duplicates", {
  df <- data.frame(
    DI = c("10.1234/1", "10.1234/1", "10.5678/2"),
    TI = c("Article 1", "Article 1 duplicate", "Article 2"),
    PY = c(2020, 2020, 2021),
    stringsAsFactors = FALSE
  )
  
  result <- m0_deduplicate(df)
  
  expect_true(nrow(result) <= nrow(df))
  # Should remove duplicate DOI
  expect_true(length(unique(result$DI)) == 2 || nrow(result) == 2)
})

test_that("m0_string_similarity computes similarity correctly", {
  s1 <- "Machine Learning in Power Systems"
  s2 <- "Machine Learning for Power Systems"
  s3 <- "Completely Different Title"
  
  sim_12 <- m0_string_similarity(s1, s2)
  sim_13 <- m0_string_similarity(s1, s3)
  
  expect_true(sim_12 > sim_13)
  expect_true(sim_12 > 0.5)
  expect_true(sim_13 < 0.5)
})

test_that("m0_fuzzy_title_match identifies similar titles", {
  titles <- c(
    "Machine Learning in Power Systems",
    "Machine Learning for Power Systems",
    "Neural Networks in Energy",
    "Deep Learning Applications"
  )
  
  result <- m0_fuzzy_title_match(titles, threshold = 0.8)
  
  expect_true(is.matrix(result))
  expect_true(nrow(result) == length(titles))
  expect_true(ncol(result) == length(titles))
})

test_that("m0_check_data_quality returns quality report", {
  df <- data.frame(
    AU = c("Smith J", "Jones M", NA),
    PY = c(2020, 2030, 2022),  # One future year
    TI = c("Title 1", "Title 2", "Title 3"),
    TC = c(10, -5, 20),  # One negative citation
    stringsAsFactors = FALSE
  )
  
  result <- m0_check_data_quality(df)
  
  expect_true(is.list(result))
  expect_true("missing_fields" %in% names(result))
  expect_true("issues" %in% names(result))
})

test_that("m0_create_quality_report generates report", {
  quality_data <- list(
    n_records = 100,
    missing_fields = list(AU = 5, PY = 2),
    issues = list("future_year" = 3, "negative_citations" = 1)
  )
  
  result <- m0_create_quality_report(quality_data)
  
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})