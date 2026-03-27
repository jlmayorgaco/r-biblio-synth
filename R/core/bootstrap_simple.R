# ============================================================================
# bootstrap_simple.R - Simple bootstrap for RBiblioSynth  
# ============================================================================
# Simplified version that loads files without recursion issues.
# Usage: source("R/core/bootstrap_simple.R")

# GUARD: Check if already loaded (use same variable as bootstrap.R)
if (exists(".RBiblioSynth_Bootstrapped", envir = .GlobalEnv) && isTRUE(.GlobalEnv$.RBiblioSynth_Bootstrapped)) {
  cat("RBiblioSynth already loaded.\n")
  invisible(TRUE)
  return(invisible(TRUE))
}

# Set guard immediately
.RBiblioSynth_Bootstrapped <<- TRUE

cat("Loading RBiblioSynth...\n")

# Find project root  
project_root <- tryCatch({
  normalizePath(file.path(dirname(sys.frame(1)$ofile), "..", ".."), mustWork = FALSE)
}, error = function(e) {
  if (file.exists("DESCRIPTION")) getwd()
  else normalizePath("../..", mustWork = FALSE)
})

# Verify project root
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  stop("Cannot find DESCRIPTION file. Please run from project root or examples directory.")
}

cat("Project root:", project_root, "\n")

# Load required packages
required_packages <- c(
  "cli", "tibble", "dplyr", "tidyr", "ggplot2", "jsonlite",
  "bibliometrix", "stats", "utils"
)

for (pkg in required_packages) {
  tryCatch(
    library(pkg, character.only = TRUE, quietly = TRUE),
    error = function(e) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  )
}

# Get all R files
r_files <- list.files(
  path = file.path(project_root, "R"),
  pattern = "[.][Rr]$",
  full.names = TRUE,
  recursive = TRUE
)

# Exclude ALL bootstrap files and auto_install to prevent recursion
r_files <- r_files[!grepl("bootstrap.*[.]R$", r_files)]
r_files <- r_files[!grepl("auto_install[.]R$", r_files)]

# Order by directory priority
core_files <- r_files[grepl("/core/", r_files)]
service_files <- r_files[grepl("/services/", r_files)]
style_files <- r_files[grepl("/style/", r_files)]
module_files <- r_files[grepl("/module_", r_files)]
other_files <- setdiff(r_files, c(core_files, service_files, style_files, module_files))

load_order <- c(core_files, service_files, style_files, module_files, other_files)

cat("Loading", length(load_order), "R files...\n")

# Source each file into GlobalEnv
loaded_count <- 0
failed_files <- character()

for (f in load_order) {
  tryCatch({
    source(f, local = FALSE)
    loaded_count <- loaded_count + 1
    if (loaded_count %% 20 == 0) {
      cat("  Loaded", loaded_count, "files\n")
    }
  }, error = function(e) {
    failed_files <- c(failed_files, f)
    cat("  Error loading", basename(f), ":", e$message, "\n")
  })
}

cat("Successfully loaded", loaded_count, "files\n")

# Verify key functions exist
key_functions <- c("biblio_config", "run_m1", "run_m2", "run_m3")
for (fn in key_functions) {
  if (exists(fn, envir = .GlobalEnv, mode = "function")) {
    cat("  OK:", fn, "\n")
  } else {
    cat("  MISSING:", fn, "\n")
  }
}

cat("\nReady. Use run_m1(), run_m2(), run_m3() to run modules.\n")

invisible(loaded_count)