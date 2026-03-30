# ============================================================================
# examples/PRISMA_TEMPLATE/main_m0.r
# RBiblioSynth - M0 Data Orchestrator Example
# ============================================================================

# --------------------------------------------------- #
# 1. Bootstrap
# --------------------------------------------------- #
cat("Bootstrap: Installing dependencies and loading functions...\n")

script_path <- tryCatch({
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
}, error = function(e) {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("--file=", args, value = TRUE)
  if (length(script_arg) > 0) {
    normalizePath(dirname(sub("--file=", "", script_arg)), mustWork = FALSE)
  } else {
    getwd()
  }
})

project_root <- normalizePath(file.path(script_path, "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  project_root <- getwd()
}

source(file.path(project_root, "R", "core", "bootstrap.R"))

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
# 3. Define sources (Scopus + WoS + OpenAlex example)
# --------------------------------------------------- #
sources <- list(
  scopus = list(
    file = file.path(script_path, "..", "SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR",
                     "data", "scopus.bib"),
    db   = "scopus",
    format = "bibtex"
  )
  # Uncomment to add more sources:
  # wos = list(
  #   file = "data/wos_export.bib",
  #   db   = "wos",
  #   format = "bibtex"
  # ),
  # openalex = list(
  #   file = "data/openalex_export.csv",
  #   db   = "openalex",
  #   format = "csv"
  # )
)

# --------------------------------------------------- #
# 4. PRISMA specification
# --------------------------------------------------- #
prisma_spec <- file.path(script_path, "prisma_spec.json")

# --------------------------------------------------- #
# 5. Run M0: Data Orchestrator
# --------------------------------------------------- #
cat("\n=== Running M0: Data Orchestrator ===\n")

m0_result <- tryCatch({
  run_m0(sources, config = config, prisma_spec = prisma_spec, export = TRUE)
}, error = function(e) {
  warning("M0 failed: ", e$message)
  list(status = "error", data = list())
})

cat("\nM0 Status:", m0_result$status, "\n")

if (m0_is_valid(m0_result)) {
  cat("\n--- Data Summary ---\n")
  cat("  Sources loaded:", m0_result$inputs$n_sources, "\n")
  cat("  Total records :", m0_result$inputs$n_total, "\n")
  cat("  Source names  :", paste(m0_result$inputs$sources, collapse = ", "), "\n")

  # Show organized data
  org <- m0_result$data$organized
  cat("\n--- Organized Data ---\n")
  cat("  Authors     :", nrow(org$authors), "unique authors\n")
  cat("  Countries   :", nrow(org$countries), "countries\n")
  cat("  Sources     :", nrow(org$sources), "journals/sources\n")
  cat("  Keywords    :", nrow(org$keywords), "keywords\n")
  cat("  Doc types   :", nrow(org$doc_types), "types\n")
  cat("  Annual data :", nrow(org$annual), "years\n")
}

# --------------------------------------------------- #
# 6. PRISMA report
# --------------------------------------------------- #
if (!is.null(m0_get_prisma(m0_result))) {
  cat("\n--- PRISMA Summary ---\n")
  prisma <- m0_get_prisma(m0_result)
  cat("  Identified :", prisma$identification$records_database +
      prisma$identification$records_other, "records\n")
  cat("  Duplicates :", prisma$identification$duplicates_removed, "\n")
  cat("  Screened   :", prisma$screening$records_screened, "\n")
  cat("  Included   :", prisma$included$studies_included, "\n")
}

# --------------------------------------------------- #
# 7. Run downstream modules using M0 data
# --------------------------------------------------- #
cat("\n=== Running M1 with M0 data ===\n")

bib_data <- m0_get_bib_data(m0_result)

if (!is.null(bib_data)) {
  m1_result <- tryCatch({
    run_m1(bib_data, config = config, export = TRUE)
  }, error = function(e) {
    warning("M1 failed: ", e$message)
    list(status = "error", data = list())
  })
  cat("M1 Status:", m1_result$status, "\n")

  # M3 can use organized country data directly
  cat("\n=== Running M3 with M0 data ===\n")
  m3_result <- tryCatch({
    run_m3(bib_data, config = config, export = TRUE)
  }, error = function(e) {
    warning("M3 failed: ", e$message)
    list(status = "error", data = list())
  })
  cat("M3 Status:", m3_result$status, "\n")
}

# --------------------------------------------------- #
# 8. Pipeline Summary
# --------------------------------------------------- #
cat("\n============================================================\n")
cat("Pipeline complete: M0 -> M1 -> M3\n")
cat("  M0:", m0_result$status, "\n")
if (exists("m1_result")) cat("  M1:", m1_result$status, "\n")
if (exists("m3_result")) cat("  M3:", m3_result$status, "\n")
cat("  Output root:", config$output_dir, "\n")
cat("============================================================\n")

invisible(list(
  m0 = m0_result,
  m1 = if (exists("m1_result")) m1_result else NULL,
  m3 = if (exists("m3_result")) m3_result else NULL,
  config = config
))
