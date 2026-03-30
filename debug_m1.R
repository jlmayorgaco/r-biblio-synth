# Debug script for M1 module
# Run this in R/RStudio to diagnose issues

# Set working directory to project root
setwd("C:/Users/walla/Documents/Github/r-biblio-synth")

# Source bootstrap
cat("=== Loading bootstrap ===\n")
tryCatch({
  source("R/core/bootstrap.R")
  cat("Bootstrap loaded successfully!\n")
}, error = function(e) {
  cat("ERROR loading bootstrap:", e$message, "\n")
  cat("Trying alternative approach...\n")
  
  # Alternative: source files manually
  r_files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  for (f in r_files) {
    tryCatch(source(f), error = function(e2) cat("Failed:", f, "-", e2$message, "\n"))
  }
})

# Test 1: Check config
cat("\n=== Testing biblio_config ===\n")
tryCatch({
  cfg <- biblio_config(verbose = FALSE)
  cat("Config created successfully\n")
  cat("  output_dir:", cfg$output_dir, "\n")
  cat("  verbose:", cfg$verbose, "\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

# Test 2: Load test data
cat("\n=== Loading test data ===\n")
test_file <- "examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib"
if (file.exists(test_file)) {
  cat("Found:", test_file, "\n")
  tryCatch({
    sources <- list(scopus = list(file = test_file, db = "scopus", format = "bibtex"))
    load_config <- biblio_config(verbose = FALSE)
    raw_list <- m0_load_all_sources(sources, config = load_config)
    bib_data <- raw_list$scopus
    cat("Loaded", nrow(bib_data), "documents\n")
    cat("Columns:", paste(names(bib_data), collapse = ", "), "\n")
  }, error = function(e) {
    cat("ERROR loading data:", e$message, "\n")
  })
} else {
  cat("Test file not found\n")
}

# Test 3: Run M1 compute functions one by one
cat("\n=== Testing M1 compute functions ===\n")
test_compute <- function(name, fn, input) {
  cat("Testing", name, "... ")
  tryCatch({
    result <- fn(input, biblio_config(verbose = FALSE))
    if (is.list(result)) {
      cat("OK - status:", result$status %||% "no status", "\n")
      cat("    Fields:", paste(names(result), collapse = ", "), "\n")
    } else {
      cat("OK\n")
    }
    return(TRUE)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(FALSE)
  })
}

if (exists("bib_data") && !is.null(bib_data)) {
  test_compute("overview", compute_m1_overview, bib_data)
  test_compute("doc_types", compute_m1_doc_types, bib_data)
  test_compute("authors", compute_m1_authors, bib_data)
  test_compute("collaboration", compute_m1_collaboration, bib_data)
  test_compute("price_law", compute_m1_price_law, bib_data)
  test_compute("citation_analysis", compute_m1_citation_analysis, bib_data)
  test_compute("author_career", compute_m1_author_career, bib_data)
}

# Test 4: Check field name consistency
cat("\n=== Checking field name consistency ===\n")
check_field_match <- function(compute_result, render_fn, name) {
  cat("Checking", name, "...\n")
  tryCatch({
    render_result <- render_fn(compute_result, biblio_config(verbose = FALSE))
    cat("  Render status:", render_result$status %||% "no status", "\n")
    cat("  Plot count:", length(render_result$plots), "\n")
    return(TRUE)
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    return(FALSE)
  })
}

if (exists("bib_data") && !is.null(bib_data)) {
  cat("\n--- Collaboration ---\n")
  collab <- compute_m1_collaboration(bib_data, biblio_config(verbose = FALSE))
  cat("Compute fields:", paste(names(collab), collapse = ", "), "\n")
  if (!is.null(collab$by_year) && nrow(collab$by_year) > 0) {
    cat("by_year columns:", paste(names(collab$by_year), collapse = ", "), "\n")
  }
  check_field_match(collab, render_m1_collaboration, "collaboration")
  
  cat("\n--- Price Law ---\n")
  price <- compute_m1_price_law(bib_data, biblio_config(verbose = FALSE))
  cat("Compute fields:", paste(names(price), collapse = ", "), "\n")
  if (!is.null(price$price_law)) {
    cat("price_law fields:", paste(names(price$price_law), collapse = ", "), "\n")
  }
  check_field_match(price, render_m1_price_law, "price_law")
}

cat("\n=== Debug complete ===\n")