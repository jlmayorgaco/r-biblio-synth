# ============================================================================
# examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r
# RBiblioSynth - New Architecture Example
# ============================================================================

# Clear workspace
rm(list = ls())

# Set null graphics device for Rscript to prevent hanging
if (interactive() == FALSE) {
  # Use a null device that doesn't open windows
  options(device = function(file, ...) {
    if (missing(file) || is.null(file)) {
      # Return a null device for plotting
      return(grDevices::pdf(NULL))
    } else {
      return(grDevices::pdf(file, ...))
    }
  })
}

# --------------------------------------------------- #
# 1. Bootstrap: Auto-install dependencies & load functions
# --------------------------------------------------- #
cat("Bootstrap: Installing dependencies and loading functions...\n")

curr <- getwd()
if (basename(curr) == "SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR") {
  project_root <- dirname(dirname(curr))
} else {
  project_root <- curr
}

bootstrap_path <- file.path(project_root, "R", "core", "bootstrap.R")
if (!file.exists(bootstrap_path)) {
  stop("Cannot find bootstrap.R at: ", bootstrap_path)
}

source(bootstrap_path)

# --------------------------------------------------- #
# 2. Configuration
# --------------------------------------------------- #
config <- biblio_config(
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
cat("\nLoading bibliographic data...\n")

# Try multiple paths for data file
possible_paths <- c(
  file.path(getwd(), "data", "scopus.bib"),
  file.path("data", "scopus.bib")
)

file_path <- NULL
for (p in possible_paths) {
  if (file.exists(p)) {
    file_path <- p
    cat("Found data file at:", p, "\n")
    break
  }
}

if (is.null(file_path)) {
  stop("Cannot find scopus.bib in any expected location")
}

# Load and merge Scopus with all Web of Science plaintext exports
bib_data <- tryCatch({
  data_dir <- dirname(file_path)
  wos_files <- sort(Sys.glob(file.path(data_dir, "wos_*.txt")))
  if (length(wos_files) == 0L) {
    stop("Cannot find Web of Science files matching wos_*.txt in ", data_dir)
  }

  sources <- list(
    scopus = list(file = file_path, db = "scopus", format = "bibtex"),
    wos = list(files = wos_files, db = "wos", format = "plaintext")
  )
  load_config <- biblio_config(
    output_dir = config$output_dir,
    verbose = FALSE,
    export_plots = FALSE,
    export_json = FALSE,
    export_reports = FALSE
  )
  m0_result <- run_m0(sources, config = load_config, export = FALSE)
  if (m0_result$status == "error") {
    stop("M0 returned error status")
  }
  m0_get_bib_data(m0_result)
}, error = function(e) {
  stop("Failed to load data: ", e$message)
})

cat("Loaded", nrow(bib_data), "documents\n")

# --------------------------------------------------- #
# 4. Run M1: Main Information
# --------------------------------------------------- #
cat("\n=== Running M1: Main Information ===\n")

# Set Cairo graphics backend for better Windows compatibility
if (.Platform$OS.type == "windows") {
  options(bitmapType = "cairo")
}

# First run without export to test computation
m1_result <- run_m1(bib_data, config = config, export = FALSE)

cat("M1 Computation Status:", m1_result$status, "\n")

# Then export separately if computation succeeded
if (m1_result$status != "error" && config$export_plots) {
  cat("Exporting M1 plots...\n")
  tryCatch({
    exported <- export_m1(m1_result, config)
    cat("Exported", length(exported$plots), "plots\n")
  }, error = function(e) {
    warning("M1 export error: ", e$message)
  })
}

cat("M1 Status:", m1_result$status, "\n")

# --------------------------------------------------- #
# 5. Run M2: Annual Production (if we have year data)
# --------------------------------------------------- #
cat("\n=== Skipping M2: Annual Production ===\n")
m2_result <- list(status = "skipped")

# --------------------------------------------------- #
# 6. Run M3: Countries Analysis
# --------------------------------------------------- #
cat("\n=== Skipping M3: Countries Analysis ===\n")
m3_result <- list(status = "skipped")

# --------------------------------------------------- #
# 7. Summary
# --------------------------------------------------- #
cat("\n============================================================\n")
cat("Pipeline complete:\n")
cat("  M1:", m1_result$status, "\n")
cat("  M2:", if (!is.null(m2_result)) m2_result$status else "skipped", "\n")
cat("  M3:", m3_result$status, "\n")
cat("============================================================\n")

invisible(list(m1 = m1_result, m2 = m2_result, m3 = m3_result))
