# ============================================================================
# examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r
# RBiblioSynth - New Architecture Example
# ============================================================================

# --------------------------------------------------- #
# Configuration
# --------------------------------------------------- #
config <- list(
  output_dir    = "results",
  export_plots  = TRUE,
  export_json   = TRUE,
  export_reports = TRUE,
  dpi           = 600,
  verbose       = TRUE
)

# --------------------------------------------------- #
# Load Data
# --------------------------------------------------- #
cat("Loading bibliographic data...\n")

# Try different methods to load scopus.bib
bib_data <- tryCatch({
  # Method 1: Direct convert2df
  bibliometrix::convert2df(
    file = "data/scopus.bib",
    dbsource = "scopus",
    format = "bibtex"
  )
}, error = function(e1) {
  cat("Method 1 failed, trying readFiles...\n")
  tryCatch({
    # Method 2: readFiles + convert2df
    raw <- bibliometrix::readFiles("data/scopus.bib")
    bibliometrix::convert2df(raw, dbsource = "scopus", format = "bibtex")
  }, error = function(e2) {
    cat("Method 2 failed, trying direct read...\n")
    # Method 3: Direct read
    raw_lines <- readLines("data/scopus.bib", warn = FALSE)
    bib_text <- paste(raw_lines, collapse = "\n")
    bibliometrix::convert2df(bib_text, dbsource = "scopus", format = "bibtex")
  })
})

cat("Loaded", nrow(bib_data), "documents with", ncol(bib_data), "columns\n")

# --------------------------------------------------- #
# Run M1: Main Information
# --------------------------------------------------- #
cat("\n=== Running M1: Main Information ===\n")

m1_result <- run_m1(bib_data, config = config, export = TRUE)

cat("\nM1 Status:", m1_result$status, "\n")
cat("Data slots:", paste(names(m1_result$data), collapse = ", "), "\n")
cat("Artifacts:", paste(names(m1_result$artifacts), collapse = ", "), "\n")

# --------------------------------------------------- #
# Access Results
# --------------------------------------------------- #

# Overview
cat("\n--- Overview ---\n")
overview <- m1_result$data$overview$main_indicators
cat("Timespan:", overview$timespan, "\n")
cat("Documents:", overview$documents, "\n")
cat("Sources:", overview$sources, "\n")
cat("Authors:", overview$authors, "\n")

# Document Types
cat("\n--- Document Types ---\n")
print(m1_result$data$doc_types$doc_type_table)

# Top Authors
cat("\n--- Top Authors ---\n")
print(m1_result$data$authors$top_authors)

# Top Citations
cat("\n--- Top Cited Papers ---\n")
print(m1_result$data$citations$top_cited_documents)

# Top Countries
cat("\n--- Top Countries ---\n")
print(m1_result$data$countries$top_countries_by_articles)

# Top Sources
cat("\n--- Top Sources ---\n")
print(m1_result$data$sources$top_sources)

# Bradford
cat("\n--- Bradford Zones ---\n")
print(m1_result$data$bradford$zone_summary)

# Manifest
cat("\n--- Manifest ---\n")
manifest <- m1_result$artifacts$manifest
cat("Status:", manifest$status, "\n")
cat("Plots:", length(manifest$plots), "\n")
cat("JSONs:", length(manifest$files), "\n")
cat("Reports:", length(manifest$reports), "\n")

cat("\n=== M1 Complete ===\n")
cat("Results saved to:", file.path(config$output_dir, "m1"), "\n")
