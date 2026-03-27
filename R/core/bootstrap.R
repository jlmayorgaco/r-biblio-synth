# ============================================================================
# bootstrap.R - Bootstrap script for RBiblioSynth package
# ============================================================================
# This script should be sourced before running any module.
# It auto-installs missing dependencies and loads all package functions.
#
# Usage:
#   source("R/core/bootstrap.R")

# GUARD: Prevent multiple executions (must be at the VERY top)
if (exists(".RBiblioSynth_Bootstrapped", envir = .GlobalEnv) && 
    isTRUE(.RBiblioSynth_Bootstrapped)) {
  cat("Bootstrap already completed.\n")
  invisible(list(success = TRUE, already_bootstrapped = TRUE))
  return(invisible(list(success = TRUE, already_bootstrapped = TRUE)))
}

# Set guard IMMEDIATELY to prevent recursive calls
.RBiblioSynth_Bootstrapped <<- TRUE

# Start timing
start_time <- Sys.time()

# Determine project root
bootstrap_dir <- tryCatch({
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  if (file.exists("R/core/bootstrap.R")) {
    normalizePath("R/core", mustWork = FALSE)
  } else {
    getwd()
  }
})

project_root <- normalizePath(file.path(bootstrap_dir, "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  candidate <- normalizePath(getwd(), mustWork = FALSE)
  if (file.exists(file.path(candidate, "DESCRIPTION"))) {
    project_root <- candidate
  }
}

cat("=== RBiblioSynth Bootstrap ===\n")
cat("Starting at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Project root:", project_root, "\n\n")

# Step 1: Check and load packages
cat("Step 1: Loading packages...\n")
cat("--------------------------\n")

required_packages <- c(
  "cli", "rlang", "tibble", "ggplot2", "jsonlite", "dplyr", "tidyr",
  "scales", "stats", "utils", "bibliometrix", "stopwords", "zoo",
  "lomb", "lmtest", "splines", "treemapify", "ggrepel", 
  "rnaturalearth", "sf", "countrycode", "tseries", "nortest"
)

missing_packages <- character()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Load packages
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("  OK", pkg, "\n")
  }, error = function(e) {
    cat("  FAILED", pkg, ":", e$message, "\n")
  })
}

# Step 2: Load all R files
cat("\nStep 2: Loading R files...\n")
cat("------------------------\n")

r_files <- list.files(
  path = file.path(project_root, "R"),
  pattern = "\\.[Rr]$",
  full.names = TRUE,
  recursive = TRUE
)

# Sort files: core first, then services, style, then modules
# Exclude ALL bootstrap files to prevent recursion
core_files <- r_files[grepl("R/core/", r_files) & 
                      !grepl("bootstrap.*\\.R$", r_files) & 
                      !grepl("auto_install\\.R$", r_files)]
service_files <- r_files[grepl("R/services/", r_files)]
style_files <- r_files[grepl("R/style/", r_files)]
module_files <- r_files[grepl("R/module_", r_files)]
other_files <- setdiff(r_files, c(core_files, service_files, style_files, module_files))

load_order <- c(core_files, service_files, style_files, module_files, other_files)

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
    failed_files <- c(failed_files, f)
    cat("  FAILED:", basename(f), "-", e$message, "\n")
  })
}

cat("  Successfully loaded", loaded_count, "files\n")

# Step 3: Verify key functions
cat("\nStep 3: Verifying functions...\n")
cat("-----------------------------\n")

key_functions <- c("biblio_config", "merge_biblio_config", "run_m1", "run_m2", "run_m3")

for (fn in key_functions) {
  if (exists(fn, mode = "function")) {
    cat("  OK", fn, "\n")
  } else {
    cat("  MISSING", fn, "\n")
  }
}

# End timing
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "secs")

cat("\n=== Bootstrap Complete ===\n")
cat("Duration:", round(as.numeric(duration), 2), "seconds\n")
cat("Ready to use RBiblioSynth modules.\n")

invisible(list(
  success = loaded_count == length(load_order),
  loaded_files = loaded_count,
  total_files = length(load_order),
  failed_files = failed_files,
  duration_secs = as.numeric(duration),
  project_root = project_root
))