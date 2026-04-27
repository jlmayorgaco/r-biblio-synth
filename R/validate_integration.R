# ============================================================================
# R/validate_integration.R - Quick validation script
# ============================================================================
# Run this script to verify all new analyses are properly integrated.
# Usage: source("R/validate_integration.R")
# ============================================================================

if (isTRUE(getOption("rbibliosynth.run_dev_scripts", FALSE))) {

cat("\n")
cat("===========================================================\n")
cat("  RBiblioSynth Integration Validation\n")
cat("===========================================================\n\n")

errors <- character()
warnings <- character()
success_count <- 0

check_function <- function(name, module = "") {
  if (exists(name, mode = "function", envir = .GlobalEnv)) {
    cat("[PASS]", name, "\n")
    success_count <<- success_count + 1
    TRUE
  } else {
    cat("[FAIL]", name, "- function not found\n")
    errors <<- c(errors, paste0(name, " (", module, ")"))
    FALSE
  }
}

check_file <- function(path, description = "") {
  if (file.exists(path)) {
    cat("[PASS]", description, "\n")
    success_count <<- success_count + 1
    TRUE
  } else {
    cat("[FAIL]", description, "- file not found:", path, "\n")
    errors <<- c(errors, paste0("Missing file: ", path))
    FALSE
  }
}

# ---------------------------------------------------
# Bootstrap first
# ---------------------------------------------------
cat("Loading bootstrap...\n")
tryCatch({
  # Determine script location for reliable sourcing
  script_path <- tryCatch({
    normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
  }, error = function(e) {
    # If running via source() without a file, use current working directory
    getwd()
  })

  # Try to find bootstrap from script location or working directory
  bootstrap_paths <- c(
    file.path(script_path, "core", "bootstrap.R"),
    file.path(script_path, "..", "R", "core", "bootstrap.R"),
    file.path(getwd(), "R", "core", "bootstrap.R"),
    "R/core/bootstrap.R"
  )

  bootstrap_loaded <- FALSE
  for (bp in bootstrap_paths) {
    if (file.exists(bp)) {
      source(bp)
      cat("[PASS] Bootstrap loaded from:", bp, "\n\n")
      success_count <<- success_count + 1
      bootstrap_loaded <- TRUE
      break
    }
  }

  if (!bootstrap_loaded) {
    stop("Could not find bootstrap.R in any expected location")
  }
}, error = function(e) {
  cat("[FAIL] Bootstrap failed:", e$message, "\n")
  errors <<- c(errors, paste0("Bootstrap: ", e$message))
})

# ---------------------------------------------------
# M1 Compute Functions
# ---------------------------------------------------
cat("\n--- M1 Compute Functions ---\n")
check_function("compute_m1_overview", "M1")
check_function("compute_m1_doc_types", "M1")
check_function("compute_m1_authors", "M1")
check_function("compute_m1_author_indices", "M1")
check_function("compute_m1_citations", "M1")
check_function("compute_m1_countries", "M1")
check_function("compute_m1_sources", "M1")
check_function("compute_m1_keywords", "M1")
check_function("compute_m1_keyword_cooccurrence", "M1")
check_function("compute_m1_bradford", "M1")
check_function("compute_m1_lotka", "M1")
check_function("compute_m1_collaboration", "M1")
check_function("compute_m1_price_law", "M1")

# ---------------------------------------------------
# M1 Render Functions
# ---------------------------------------------------
cat("\n--- M1 Render Functions ---\n")
check_function("render_m1_overview", "M1")
check_function("render_m1_doc_types", "M1")
check_function("render_m1_authors", "M1")
check_function("render_m1_author_indices", "M1")
check_function("render_m1_citations", "M1")
check_function("render_m1_countries", "M1")
check_function("render_m1_sources", "M1")
check_function("render_m1_keywords", "M1")
check_function("render_m1_keyword_cooccurrence", "M1")
check_function("render_m1_bradford", "M1")
check_function("render_m1_lotka", "M1")
check_function("render_m1_collaboration", "M1")
check_function("render_m1_price_law", "M1")

# ---------------------------------------------------
# M1 Table Functions
# ---------------------------------------------------
cat("\n--- M1 Table Functions ---\n")
check_function("build_m1_overview_table", "M1")
check_function("build_m1_doc_types_table", "M1")
check_function("build_m1_authors_table", "M1")
check_function("build_m1_author_indices_table", "M1")
check_function("build_m1_citations_table", "M1")
check_function("build_m1_countries_table", "M1")
check_function("build_m1_sources_table", "M1")
check_function("build_m1_keywords_table", "M1")
check_function("build_m1_keyword_cooccurrence_table", "M1")
check_function("build_m1_bradford_table", "M1")
check_function("build_m1_lotka_table", "M1")
check_function("build_m1_collaboration_table", "M1")
check_function("build_m1_price_law_table", "M1")

# ---------------------------------------------------
# M2 Compute Functions
# ---------------------------------------------------
cat("\n--- M2 Compute Functions ---\n")
check_function("compute_m2_eda", "M2")
check_function("compute_m2_regression", "M2")
check_function("compute_m2_ridge", "M2")
check_function("compute_m2_changepoint", "M2")
check_function("compute_m2_stl", "M2")

# ---------------------------------------------------
# M2 Render Functions
# ---------------------------------------------------
cat("\n--- M2 Render Functions ---\n")
check_function("render_m2_eda", "M2")
check_function("render_m2_regression", "M2")

# ---------------------------------------------------
# M3 Compute Functions
# ---------------------------------------------------
cat("\n--- M3 Compute Functions ---\n")
check_function("prepare_m3_country_data", "M3")
check_function("m3_compute_production", "M3")
check_function("m3_compute_citations", "M3")
check_function("m3_compute_scp_mcp", "M3")
check_function("compute_m3_collaboration_indices", "M3")

# ---------------------------------------------------
# M3 Render Functions
# ---------------------------------------------------
cat("\n--- M3 Render Functions ---\n")
check_function("m3_render_productivity", "M3")
check_function("m3_render_citations", "M3")
check_function("m3_render_scp_mcp", "M3")
check_function("render_m3_collaboration_indices", "M3")

# ---------------------------------------------------
# M3 Table Functions
# ---------------------------------------------------
cat("\n--- M3 Table Functions ---\n")
check_function("m3_table_productivity", "M3")
check_function("m3_table_citations", "M3")
check_function("m3_table_scp_mcp", "M3")
check_function("m3_table_collaboration_indices", "M3")

# ---------------------------------------------------
# M1 Hypothesis Functions
# ---------------------------------------------------
cat("\n--- Testing M1 Hypothesis Functions ---\n")
check_function("compute_m1_hypotheses", "M1")

# ---------------------------------------------------
# M2 Hypothesis Functions
# ---------------------------------------------------
cat("\n--- Testing M2 Hypothesis Functions ---\n")
check_function("compute_m2_hypotheses", "M2")

# ---------------------------------------------------
# M3 Country Regressions
# ---------------------------------------------------
cat("\n--- Testing M3 Country Regressions ---\n")
check_function("m3_compute_country_regressions", "M3")
check_function("m3_compute_hypotheses", "M3")
check_function("render_m3_country_regressions", "M3")
check_function("m3_table_country_regressions", "M3")

# ---------------------------------------------------
# Runner Functions
# ---------------------------------------------------
cat("\n--- Runner Functions ---\n")
check_function("run_m1", "Main")
check_function("run_m2", "Main")
check_function("run_m3", "Main")

# ---------------------------------------------------
# Key Files
# ---------------------------------------------------
cat("\n--- Key Files ---\n")

# Get script directory for reliable file checking
script_dir <- tryCatch({
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
}, error = function(e) {
  getwd()
})

# Build paths relative to script or working directory
# Note: script_dir is the R/ folder, so modules are in subdirectories
key_files <- list(
  list(path = file.path(script_dir, "core", "bootstrap.R"), alt = file.path(getwd(), "R", "core", "bootstrap.R"), desc = "Bootstrap"),
  list(path = file.path(script_dir, "module_m1", "m1_run.R"), alt = file.path(getwd(), "R", "module_m1", "m1_run.R"), desc = "M1 Runner"),
  list(path = file.path(script_dir, "module_m2", "m2_run.R"), alt = file.path(getwd(), "R", "module_m2", "m2_run.R"), desc = "M2 Runner"),
  list(path = file.path(script_dir, "module_m3", "m3_run.R"), alt = file.path(getwd(), "R", "module_m3", "m3_run.R"), desc = "M3 Runner")
)

for (kf in key_files) {
  if (file.exists(kf$path)) {
    check_file(kf$path, kf$desc)
  } else if (file.exists(kf$alt)) {
    check_file(kf$alt, kf$desc)
  } else {
    # Try relative path as fallback
    rel_path <- gsub(paste0(getwd(), "/"), "", kf$alt, fixed = TRUE)
    check_file(rel_path, kf$desc)
  }
}

# ---------------------------------------------------
# Summary
# ---------------------------------------------------
cat("\n")
cat("===========================================================\n")
cat("  Validation Summary\n")
cat("===========================================================\n")
cat("  Passed:", success_count, "\n")
cat("  Errors:", length(errors), "\n")
cat("  Warnings:", length(warnings), "\n")

if (length(errors) > 0) {
  cat("\nErrors:\n")
  for (e in errors) cat("  -", e, "\n")
}

if (length(warnings) > 0) {
  cat("\nWarnings:\n")
  for (w in warnings) cat("  -", w, "\n")
}

if (length(errors) == 0) {
  cat("\n[SUCCESS] All integrations validated successfully!\n")
  cat("\nTo test with real data, run:\n")
  cat("  source('examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r')\n")
} else {
  cat("\n[FAILED] Some integrations have errors. Please fix them.\n")
}

cat("===========================================================\n\n")

invisible(list(
  success = length(errors) == 0,
  passed = success_count,
  errors = errors,
  warnings = warnings
))

}
