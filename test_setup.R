# test_main_r.R - Quick test to verify main.r works
# Run this in RStudio before running main.r

cat("========================================\n")
cat("Testing RBiblioSynth Setup\n")
cat("========================================\n\n")

# Test 1: Check if we can find the project
cat("1. Checking project structure...\n")
project_root <- "C:/Users/walla/Documents/Github/r-biblio-synth"
setwd(project_root)

if (!file.exists("DESCRIPTION")) {
  cat("   ERROR: Not in project root!\n")
  cat("   Current directory:", getwd(), "\n")
  stop("Please run from project root")
}
cat("   OK: In project root\n\n")

# Test 2: Check if bootstrap can be sourced
cat("2. Loading bootstrap...\n")
tryCatch({
  source("R/core/bootstrap.R")
  cat("   OK: Bootstrap loaded\n\n")
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
  stop("Bootstrap failed")
})

# Test 3: Check if biblio_config works
cat("3. Testing biblio_config()...\n")
tryCatch({
  cfg <- biblio_config(verbose = FALSE)
  if (!is.list(cfg) || is.null(cfg$output_dir)) {
    stop("Invalid config structure")
  }
  cat("   OK: Config created with output_dir =", cfg$output_dir, "\n\n")
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
  stop("Config failed")
})

# Test 4: Check if runner functions exist
cat("4. Checking runner functions...\n")
runners <- c("run_m1", "run_m2", "run_m3")
for (r in runners) {
  if (exists(r, mode = "function")) {
    cat("   OK:", r, "\n")
  } else {
    cat("   ERROR:", r, "not found\n")
  }
}
cat("\n")

# Test 5: Check if key files exist
cat("5. Checking key files...\n")
files <- c(
  "R/core/bootstrap.R",
  "R/module_m1/m1_run.R",
  "R/module_m2/m2_run.R", 
  "R/module_m3/m3_run.R",
  "examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r",
  "examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib"
)

for (f in files) {
  if (file.exists(f)) {
    cat("   OK:", f, "\n")
  } else {
    cat("   MISSING:", f, "\n")
  }
}
cat("\n")

cat("========================================\n")
cat("Setup test complete!\n")
cat("\nTo run main.r, execute:\n")
cat('  setwd("C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR")\n')
cat('  source("main.r")\n')
cat("========================================\n")