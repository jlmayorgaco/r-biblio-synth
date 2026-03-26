# ============================================================================
# examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r
# RBiblioSynth - New Architecture Example
#
# Usage:
#   Rscript run_main.R
#   (Or source this file after sourcing all package R files)
# ============================================================================

# --------------------------------------------------- #
# Configuration
# --------------------------------------------------- #
config <- list(
  output_dir     = "results",
  export_plots   = TRUE,
  export_json    = TRUE,
  export_reports = TRUE,
  dpi            = 300,
  plot_width     = 3.5,
  plot_height    = 2.5,
  verbose        = TRUE
)

# --------------------------------------------------- #
# Load Data
# --------------------------------------------------- #
cat("Loading bibliographic data from scopus.bib...\n")

# Read file and convert
raw_bib <- readLines("data/scopus.bib", warn = FALSE)
tmp_file <- tempfile(fileext = ".bib")
writeLines(raw_bib, tmp_file, useBytes = TRUE)

bib_data <- bibliometrix::convert2df(
  file = tmp_file,
  dbsource = "scopus",
  format = "bibtex"
)

unlink(tmp_file)
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

cat("\n--- Overview ---\n")
overview <- m1_result$data$overview$main_indicators
cat("Timespan:", overview$timespan, "\n")
cat("Documents:", overview$documents, "\n")
cat("Sources:", overview$sources, "\n")
cat("Authors:", overview$authors, "\n")

cat("\n--- Document Types ---\n")
print(m1_result$data$doc_types$doc_type_table)

cat("\n--- Top Authors ---\n")
print(m1_result$data$authors$top_authors)

cat("\n--- Top Cited Papers ---\n")
print(m1_result$data$citations$top_cited_documents)

cat("\n--- Top Countries ---\n")
print(m1_result$data$countries$top_countries_by_articles)

cat("\n--- Top Sources ---\n")
print(m1_result$data$sources$top_sources)

cat("\n--- Bradford Zones ---\n")
print(m1_result$data$bradford$zone_summary)

cat("\n--- Manifest ---\n")
manifest <- m1_result$artifacts$manifest
cat("Status:", manifest$status, "\n")
cat("Plots:", length(manifest$plots), "\n")
cat("JSONs:", length(manifest$files), "\n")
cat("Reports:", length(manifest$reports), "\n")

cat("\n=== M1 Complete ===\n")
cat("Results saved to:", file.path(config$output_dir, "m1"), "\n")
