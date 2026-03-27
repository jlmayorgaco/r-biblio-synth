# ============================================================================
# examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r
# RBiblioSynth - New Architecture Example
# ============================================================================

# --------------------------------------------------- #
# 1. Bootstrap: Auto-install dependencies & load functions
# --------------------------------------------------- #
cat("Bootstrap: Installing dependencies and loading functions...\n")

# Determine the project root (two levels up from this script)
# This allows the script to be run from any location
script_path <- tryCatch({
  # Works when run with source()
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
}, error = function(e) {
  # Fallback: assume script is in current directory
  getwd()
})

# Find project root
project_root <- normalizePath(file.path(script_path, "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  # Try current working directory as fallback
  project_root <- getwd()
}

bootstrap_path <- file.path(project_root, "R", "core", "bootstrap.R")
if (!file.exists(bootstrap_path)) {
  stop("Cannot find bootstrap.R at: ", bootstrap_path,
       "\nPlease run this script from within the project directory.")
}

source(bootstrap_path)

# --------------------------------------------------- #
# 2. Configuration
# --------------------------------------------------- #
config <- list(
  output_dir     = "results",
  export_plots   = TRUE,
  export_json    = TRUE,
  export_reports = TRUE,
  dpi            = 300,
  plot_width     = 3.5,
  plot_height    = 2.5,
  verbose        = TRUE,
  top_n_countries = 10,
  top_n_authors   = 10,
  top_n_sources   = 10
)

# --------------------------------------------------- #
# 3. Load Data
# --------------------------------------------------- #
cat("\nLoading bibliographic data from data/scopus.bib...\n")

file_path <- file.path(script_path, "data", "scopus.bib")
if (!file.exists(file_path)) {
  # Try alternate path
  file_path <- file.path(script_path, "data", "scopus.bib")
  if (!file.exists(file_path)) {
    stop("Cannot find data file at: data/scopus.bib\n",
         "Please ensure the data directory exists and contains 'scopus.bib'")
  }
}

# Load with bibliometrix
bib_data <- tryCatch({
  bibliometrix::convert2df(
    file = file_path,
    dbsource = "scopus",
    format = "bibtex"
  )
}, error = function(e) {
  stop("Failed to load bibliographic data: ", e$message,
       "\nPlease check the file format and ensure it's valid BibTeX.")
})

cat("Loaded", nrow(bib_data), "documents with", ncol(bib_data), "columns\n")

# --------------------------------------------------- #
# 4. Run M1: Main Information
# --------------------------------------------------- #
cat("\n=== Running M1: Main Information ===\n")

m1_result <- tryCatch({
  run_m1(bib_data, config = config, export = TRUE)
}, error = function(e) {
  warning("M1 failed: ", e$message)
  list(status = "error", data = list())
})

cat("\nM1 Status:", m1_result$status, "\n")

if (!is.null(m1_result$data$overview$main_indicators)) {
  cat("\n--- Overview ---\n")
  overview <- m1_result$data$overview$main_indicators
  for (nm in names(overview)) {
    if (!is.null(overview[[nm]])) {
      cat(nm, ":", overview[[nm]], "\n")
    }
  }
}

if (!is.null(m1_result$data$doc_types$doc_type_table)) {
  cat("\n--- Document Types ---\n")
  print(utils::head(m1_result$data$doc_types$doc_type_table, 10))
}

# --------------------------------------------------- #
# 5. Run M2: Annual Production
# --------------------------------------------------- #
cat("\n=== Running M2: Annual Production ===\n")

# Derive Year x Articles time series
py_col <- if ("PY" %in% names(bib_data)) "PY" else NULL

if (!is.null(py_col)) {
  # Extract valid years (non-NA, reasonable range)
  valid_years <- bib_data[[py_col]]
  valid_years <- valid_years[!is.na(valid_years) & valid_years > 1900 & valid_years <= 2100]
  
  if (length(valid_years) > 0) {
    annual_counts <- as.integer(table(valid_years))
    annual_years  <- as.integer(names(table(valid_years)))
    annual_ts <- data.frame(Year = annual_years, Articles = annual_counts)
    
    # Require at least 5 years for meaningful regression
    if (nrow(annual_ts) >= 5) {
      m2_result <- tryCatch({
        run_m2(annual_ts, config = config, export = TRUE)
      }, error = function(e) {
        warning("M2 failed: ", e$message)
        list(status = "error", data = list())
      })
      
      cat("\nM2 Status:", m2_result$status, "\n")
      
      if (!is.null(m2_result$data$regression$best_model$name)) {
        cat("Best regression model:", m2_result$data$regression$best_model$name,
            "| R2 =", round(m2_result$data$regression$best_model$R2, 4), "\n")
      }
    } else {
      cat("M2 skipped: fewer than 5 unique publication years in dataset.\n")
      m2_result <- NULL
    }
  } else {
    cat("M2 skipped: no valid publication years found.\n")
    m2_result <- NULL
  }
} else {
  cat("M2 skipped: PY (publication year) column not found in data.\n")
  m2_result <- NULL
}

# --------------------------------------------------- #
# 6. Run M3: Countries Analysis
# --------------------------------------------------- #
cat("\n=== Running M3: Countries Analysis ===\n")

m3_result <- tryCatch({
  run_m3(bib_data, config = config, export = TRUE)
}, error = function(e) {
  warning("M3 failed: ", e$message)
  list(status = "error", data = list())
})

cat("\nM3 Status:", m3_result$status, "\n")

# Production summary
if (!is.null(m3_result$data$production$production_summary)) {
  ps <- m3_result$data$production$production_summary
  cat("\n--- Production Summary ---\n")
  cat("  Countries active:", ps$total_countries, "\n")
  cat("  Total articles  :", ps$total_articles, "\n")
  if (!is.na(ps$gini_articles)) {
    cat("  Gini (articles) :", round(ps$gini_articles, 4), "\n")
  }
}

# Citations summary  
if (!is.null(m3_result$data$citations$citation_summary)) {
  cs <- m3_result$data$citations$citation_summary
  cat("\n--- Citation Summary ---\n")
  cat("  Total citations :", cs$total_citations, "\n")
  if (!is.na(cs$gini_citations)) {
    cat("  Gini (citations):", round(cs$gini_citations, 4), "\n")
  }
}

# SCP/MCP summary
if (!is.null(m3_result$data$scp_mcp$scp_mcp_summary)) {
  sm <- m3_result$data$scp_mcp$scp_mcp_summary
  cat("\n--- SCP/MCP Summary ---\n")
  cat("  Total SCP      :", sm$total_scp, "\n")
  cat("  Total MCP      :", sm$total_mcp, "\n")
  if (!is.na(sm$mcp_ratio)) {
    cat("  Overall MCP %  :", round(sm$mcp_ratio, 1), "%\n")
  }
}

# Inequality summary
if (!is.null(m3_result$data$inequality$inequality_summary)) {
  is <- m3_result$data$inequality$inequality_summary
  cat("\n--- Inequality Summary ---\n")
  if (!is.null(is$production)) {
    cat("  Production Gini:", round(is$production$gini, 4), "\n")
    cat("  Top 5% share   :", round(is$production$top5_share * 100, 2), "%\n")
  }
  if (!is.null(is$citations)) {
    cat("  Citations Gini :", round(is$citations$gini, 4), "\n")
    cat("  Top 5% share   :", round(is$citations$top5_share * 100, 2), "%\n")
  }
}

# Similarity/Clustering summary
if (!is.null(m3_result$data$similarity_clustering$similarity_summary)) {
  sims <- m3_result$data$similarity_clustering$similarity_summary
  cat("\n--- Similarity/Clustering Summary ---\n")
  cat("  Countries analyzed:", sims$n_countries, "\n")
  cat("  Features used    :", sims$n_features, "\n")
  cat("  Clustering       :", sims$method, "\n")
}

# --------------------------------------------------- #
# 7. Pipeline Summary
# --------------------------------------------------- #
cat("\n============================================================\n")
cat("Pipeline complete: M1 -> M2 -> M3\n")
cat("  M1:", m1_result$status, "\n")
cat("  M2:", if (!is.null(m2_result)) m2_result$status else "skipped", "\n")
cat("  M3:", m3_result$status, "\n")
cat("  Output root:", config$output_dir, "\n")
cat("============================================================\n")

# Return results invisibly for programmatic use
invisible(list(
  m1 = m1_result,
  m2 = m2_result,
  m3 = m3_result,
  config = config
))