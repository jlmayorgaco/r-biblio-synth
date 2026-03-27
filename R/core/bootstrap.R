# ============================================================================
# bootstrap.R - Bootstrap script for RBiblioSynth package
# ============================================================================
# This script should be sourced before running any module.
# It auto-installs missing dependencies and loads all package functions.
#
# Usage:
#   source("R/core/bootstrap.R")
#
# From command line:
#   Rscript -e "source('R/core/bootstrap.R')"

# Start timing
start_time <- Sys.time()

# Determine the directory where THIS file is located
# This allows bootstrap to work regardless of where it's sourced from
bootstrap_dir <- tryCatch({
  # Works when sourced normally
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # Fallback: use the path of this script
  # Try to find the R/core directory from the current working directory
  if (file.exists("R/core/bootstrap.R")) {
    "R/core"
  } else if (file.exists("../../R/core/bootstrap.R")) {
    # Running from examples subdirectory
    "../../R/core"
  } else {
    # Last resort: try to find it
    found <- list.files(".", pattern = "bootstrap.R$", recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) {
      dirname(found[1])
    } else {
      "."
    }
  }
})

# Get project root (two levels up from R/core)
project_root <- tryCatch({
  normalizePath(file.path(bootstrap_dir, "..", ".."), mustWork = FALSE)
}, error = function(e) {
  normalizePath(file.path(bootstrap_dir, "..", ".."), mustWork = FALSE)
})

# Verify project root exists
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  # Try alternative methods
  candidate <- normalizePath(getwd(), mustWork = FALSE)
  if (file.exists(file.path(candidate, "DESCRIPTION"))) {
    project_root <- candidate
  }
}

cat("=== RBiblioSynth Bootstrap ===\n")
cat("Starting at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Project root:", project_root, "\n\n")

# Step 1: Auto-install dependencies
cat("Step 1: Checking dependencies...\n")
cat("----------------------------\n")

auto_install_path <- file.path(bootstrap_dir, "auto_install.R")
if (!file.exists(auto_install_path)) {
  # Try from project root
  auto_install_path <- file.path(project_root, "R", "core", "auto_install.R")
}

if (!file.exists(auto_install_path)) {
  stop("Cannot find auto_install.R. Please ensure bootstrap.R is in R/core/")
}

source(auto_install_path)

# Install core dependencies (from DESCRIPTION)
desc_path <- file.path(project_root, "DESCRIPTION")
install_result <- tryCatch({
  auto_install_dependencies(
    desc_path = desc_path,
    install_suggests = FALSE,
    quiet = FALSE,
    load = FALSE
  )
}, error = function(e) {
  cat("Warning: Could not auto-install:", e$message, "\n")
  cat("Attempting to continue with installed packages...\n")
  list(installed = character(), existing = character(), failed = character())
})

# Step 2: Load all package functions
cat("\nStep 2: Loading package functions...\n")
cat("-------------------------------\n")

# Get all R files recursively from project root
r_files <- list.files(
  path = file.path(project_root, "R"),
  pattern = "\\.[Rr]$",
  full.names = TRUE,
  recursive = TRUE
)

# Load files in dependency order: core first, then services, style, then modules
core_files <- r_files[grepl("R/core/", r_files)]
service_files <- r_files[grepl("R/services/", r_files)]
style_files <- r_files[grepl("R/style/", r_files)]
module_files <- r_files[grepl("R/module_", r_files)]

load_order <- c(core_files, service_files, style_files, module_files)

# Also include any files that might be in R/ directly (not in subdirs)
root_files <- setdiff(r_files, load_order)
load_order <- c(load_order, root_files)

loaded_count <- 0
failed_files <- character()

for (f in load_order) {
  tryCatch({
    source(f, local = FALSE)
    loaded_count <- loaded_count + 1
    if (loaded_count %% 20 == 0) {
      cat("  Loaded", loaded_count, "files...\n")
    }
  }, error = function(e) {
    warning("Failed to load ", f, ": ", e$message)
    failed_files <<- c(failed_files, f)
  })
}

cat("  Successfully loaded", loaded_count, "files\n")
if (length(failed_files) > 0) {
  cat("  Failed to load", length(failed_files), "files:\n")
  for (f in failed_files) {
    cat("    -", f, "\n")
  }
}

# Step 3: Verify key functions are available
cat("\nStep 3: Verifying key functions...\n")
cat("-------------------------------\n")

key_functions <- c(
  "biblio_config",
  "merge_biblio_config",
  "run_m1",
  "run_m2",
  "run_m3",
  "validate_m1_input",
  "validate_m2_input",
  "validate_m3_input",
  "new_module_result",
  "new_artifact_manifest"
)

for (fn in key_functions) {
  if (exists(fn, mode = "function")) {
    cat("  OK", fn, "\n")
  } else {
    cat("  MISSING", fn, "\n")
  }
}

# Step 4: Verify required packages are loaded
cat("\nStep 4: Verifying required packages...\n")
cat("------------------------------------\n")

required_packages <- c(
  "cli",
  "tibble",
  "dplyr",
  "tidyr",
  "ggplot2",
  "jsonlite",
  "stats",
  "utils",
  "methods"
)

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  OK", pkg, "\n")
  } else {
    cat("  MISSING", pkg, "\n")
  }
}

# End timing
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "secs")

cat("\n=== Bootstrap Complete ===\n")
cat("Duration:", round(as.numeric(duration), 2), "seconds\n")
cat("Ready to use RBiblioSynth modules.\n")

# Return invisible status
invisible(list(
  success = loaded_count == length(load_order),
  loaded_files = loaded_count,
  total_files = length(load_order),
  failed_files = failed_files,
  duration_secs = as.numeric(duration),
  project_root = project_root
))