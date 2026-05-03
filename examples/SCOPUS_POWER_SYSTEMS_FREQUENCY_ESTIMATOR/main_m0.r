# ============================================================================
# DEBUG MODE: RBiblioSynth - Frequency Estimator
# ============================================================================

message("--- Starting Debug Sequence ---\n")

# 1. Path Verification (The most likely silent killer)
# ---------------------------------------------------
cwd <- getwd()
message("Current Working Directory:", cwd, "\n")

# Manually verify the data file exists before calling any functions
data_dir <- file.path(cwd, "data")
bib_file <- file.path(data_dir, "scopus.bib")
wos_files <- sort(Sys.glob(file.path(data_dir, "wos_*.txt")))
if (!file.exists(bib_file)) {
  cat("✘ ERROR: Data file NOT found at:", bib_file, "\n")
  cat("Check if you are running the script from the correct folder.\n")
} else {
  cat("✔ SUCCESS: Found data file at:", bib_file, "\n")
}
cat("Found", length(wos_files), "Web of Science plaintext files\n")

# 2. Bootstrap sourcing with strict error
# ---------------------------------------------------
# If this fails, it usually stops the script entirely without a message 
# if 'source' is called inside another block.
cat("Attempting to find bootstrap.R...\n")

# Try to find the root by looking for the 'R' directory
find_root <- function(path) {
  if (file.exists(file.path(path, "R", "core", "bootstrap.R"))) return(path)
  parent <- dirname(path)
  if (parent == path) return(NULL)
  find_root(parent)
}

project_root <- find_root(cwd)

if (is.null(project_root)) {
  stop("✘ Critical Error: Could not locate the 'R/core/bootstrap.R' directory structure.")
} else {
  bootstrap_path <- file.path(project_root, "R", "core", "bootstrap.R")
  cat("✔ Found bootstrap at:", bootstrap_path, "\n")
  source(bootstrap_path)
}

# 3. Configuration
# ---------------------------------------------------
config <- biblio_config(
  output_dir     = "results",
  export_plots   = TRUE,
  verbose        = TRUE # Keep this TRUE for debugging
)

# 4. Run M0 with Full Error Reporting
# ---------------------------------------------------
cat("\n[STEP 1] Running M0: Data Orchestrator...\n")

sources <- list(
  scopus = list(file = bib_file, db = "scopus", format = "bibtex"),
  wos = list(files = wos_files, db = "wos", format = "plaintext")
)

# We remove the tryCatch here temporarily so R is FORCED to print the full stack trace
m0_result <- run_m0(sources, config = config, export = TRUE)

cat("✔ M0 completed with status:", m0_result$status, "\n")

# 5. Run M1
# ---------------------------------------------------
if (m0_result$status != "error") {
  cat("\n[STEP 2] Running M1: Main Information...\n")
  bib_data <- m0_get_bib_data(m0_result)
  
  if (is.null(bib_data) || nrow(bib_data) == 0) {
    stop("✘ M0 returned no data. Check your .bib file content.")
  }
  
  m1_result <- run_m1(bib_data, config = config, export = TRUE)
  cat("✔ M1 Status:", m1_result$status, "\n")
}

cat("\n--- Debug Sequence Complete ---\n")
