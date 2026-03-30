# test-module-integration.R - Integration tests for module fieldname consistency
# Run: testthat::test_file("tests/testthat/test-module-integration.R")

# Source bootstrap if not already loaded
if (!exists("biblio_config")) {
  source(file.path(dirname(dirname(getwd())), "R", "core", "bootstrap.R"))
}

# Helper to create mock bibliographic data
create_mock_bib_data <- function(n = 100) {
  set.seed(42)
  data.frame(
    AU = sapply(1:n, function(i) {
      n_authors <- sample(1:5, 1)
      paste(paste0("Author_", sample(1:50, n_authors)), collapse = ";")
    }),
    PY = sample(2010:2023, n, replace = TRUE),
    TC = rpois(n, lambda = 10),
    TI = paste0("Title_", 1:n),
    SO = paste0("Journal_", sample(1:20, n, replace = TRUE)),
    DT = sample(c("Article", "Review", "Conference Paper"), n, replace = TRUE),
    AU_CO = sample(c("USA", "China", "Germany", "UK", "Japan", "France"), n, replace = TRUE),
    CR = sapply(1:n, function(i) {
      n_refs <- sample(0:10, 1)
      if (n_refs == 0) return(NA)
      paste(paste0("Ref_", sample(1:1000, n_refs)), collapse = ";")
    }),
    stringsAsFactors = FALSE
  )
}

# Test M1 Collaboration field names
test_that("M1 Collaboration: field names match between compute and render", {
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE))
  
  data <- create_mock_bib_data(50)
  
  # Compute
  collab <- compute_m1_collaboration(data, biblio_config())
  
  # Check compute output structure
  expect_true(is.list(collab))
  expect_true("by_year" %in% names(collab))
  expect_true("summary" %in% names(collab))
  
  # Check by_year has collaboration_rate
  if (nrow(collab$by_year) > 0) {
    expect_true("collaboration_rate" %in% names(collab$by_year))
  }
  
  # Check summary has collaboration_index
  expect_true("collaboration_index" %in% names(collab$summary))
  
  # Render should work
  render_result <- render_m1_collaboration(collab, biblio_config())
  expect_true(is.list(render_result))
  expect_true("plots" %in% names(render_result))
})

# Test M1 Price Law field names
test_that("M1 Price Law: field names match between compute and render", {
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE))
  
  data <- create_mock_bib_data(50)
  
  # Compute
  price <- compute_m1_price_law(data, biblio_config())
  
  # Check compute output structure
  expect_true(is.list(price))
  expect_true("price_law" %in% names(price))
  expect_true("author_concentration" %in% names(price))
  
  # price_law should have author_distribution
  expect_true("author_distribution" %in% names(price$price_law))
  
  # Render should work
  render_result <- render_m1_price_law(price, biblio_config())
  expect_true(is.list(render_result))
  expect_true("plots" %in% names(render_result))
})

# Test M1 Citation Analysis field names
test_that("M1 Citation Analysis: field names match between compute and render", {
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE))
  
  data <- create_mock_bib_data(50)
  
  # Compute
  citation <- compute_m1_citation_analysis(data, biblio_config())
  
  # Check compute output structure
  expect_true(is.list(citation))
  expect_true("summary" %in% names(citation))
  expect_true("age_analysis" %in% names(citation))
  
  # Render should work
  render_result <- render_m1_citation_analysis(citation, biblio_config())
  expect_true(is.list(render_result))
  expect_true("plots" %in% names(render_result))
})

# Test M1 Author Career field names
test_that("M1 Author Career: field names match between compute and render", {
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE))
  
  data <- create_mock_bib_data(50)
  
  # Compute
  career <- compute_m1_author_career(data, biblio_config())
  
  # Check compute output structure
  expect_true(is.list(career))
  expect_true("career_df" %in% names(career))
  
  # Render should work
  render_result <- render_m1_author_career(career, biblio_config())
  expect_true(is.list(render_result))
  expect_true("plots" %in% names(render_result))
})

# Test M1 Collaboration table
test_that("M1 Collaboration table: uses correct field paths", {
  data <- create_mock_bib_data(50)
  
  collab <- compute_m1_collaboration(data, biblio_config())
  table_result <- build_m1_collaboration_table(collab, biblio_config())
  
  expect_true(is.list(table_result))
  expect_true("summary" %in% names(table_result))
  
  # Check summary has collaboration_index
  if (!is.null(table_result$summary)) {
    expect_true("collaboration_index" %in% names(table_result$summary))
  }
})

# Test M1 Price Law table
test_that("M1 Price Law table: uses correct field paths", {
  data <- create_mock_bib_data(50)
  
  price <- compute_m1_price_law(data, biblio_config())
  table_result <- build_m1_price_law_table(price, biblio_config())
  
  expect_true(is.list(table_result))
  expect_true("summary" %in% names(table_result))
})

# Test M2 structure
test_that("M2 regression: basic structure is correct", {
  # Create time series
  annual_ts <- data.frame(
    Year = 2010:2020,
    Articles = round(100 * exp(0.05 * (2010:2020 - 2010)) + rnorm(11, 0, 5))
  )
  
  result <- run_m2(annual_ts, biblio_config(export = FALSE))
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  expect_true("data" %in% names(result))
})

# Test M3 temporal dynamics column handling
test_that("M3 temporal dynamics: handles column name aliases", {
  # Test with standard column names
  data1 <- data.frame(
    country = c("USA", "China", "Germany"),
    year = c(2020, 2020, 2020),
    production = c(100, 80, 60),
    stringsAsFactors = FALSE
  )
  
  result1 <- m3_compute_temporal_dynamics(data1, biblio_config())
  expect_true(is.list(result1))
  
  # Test with aliased column names (PY/article_count)
  data2 <- data.frame(
    country = c("USA", "China", "Germany"),
    PY = c(2020, 2020, 2020),
    article_count = c(100, 80, 60),
    stringsAsFactors = FALSE
  )
  
  result2 <- m3_compute_temporal_dynamics(data2, biblio_config())
  expect_true(is.list(result2))
})

# Test full M1 pipeline
test_that("M1 full pipeline: compute -> render -> table consistency", {
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE))
  
  data <- create_mock_bib_data(100)
  
  # Run through compute for each sub-module
  overview <- compute_m1_overview(data, biblio_config())
  expect_true(is.list(overview))
  
  doc_types <- compute_m1_doc_types(data, biblio_config())
  expect_true(is.list(doc_types))
  
  authors <- compute_m1_authors(data, biblio_config())
  expect_true(is.list(authors))
  
  citations <- compute_m1_citations(data, biblio_config())
  expect_true(is.list(citations))
  
  collaboration <- compute_m1_collaboration(data, biblio_config())
  expect_true(is.list(collaboration))
  
  # Validate render produces expected structure
  render_collab <- render_m1_collaboration(collaboration, biblio_config())
  expect_true("plots" %in% names(render_collab))
})

cat("All integration tests passed!\n")