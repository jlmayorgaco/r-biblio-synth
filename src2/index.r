# ================================================================
# index.r — Entrypoint for R BiblioSynth
# ================================================================

suppressPackageStartupMessages({
  library(R6)
  library(jsonlite)
})

# ----------------------------------------------------------------
# 0. Resolve Root Path of src2/
# ----------------------------------------------------------------
# This ensures paths are always relative to this file, not the working dir
this_file <- normalizePath(sys.frame(1)$ofile, winslash = "/")
root <- dirname(this_file)

# ----------------------------------------------------------------
# 1. Load Dependencies & Config
# ----------------------------------------------------------------
source(file.path(root, "config", "column_maps.r"))
source(file.path(root, "config", "dependencies.r"))
source(file.path(root, "config", "globals.r"))
source(file.path(root, "config", "paths.r"))
source(file.path(root, "config", "settings.r"))

source(file.path(root, "stats", "robustness_engine.r"))

# ----------------------------------------------------------------
# 2. Load Helpers and Themes
# ----------------------------------------------------------------
helper_files <- list.files(file.path(root, "helpers"), pattern = "\\.r$", full.names = TRUE)
sapply(helper_files, source)

plots_files <- list.files(file.path(root, "plots"), pattern = "\\.r$", full.names = TRUE)
sapply(plots_files, source)

theme_files <- list.files(file.path(root, "themes", "ieee"), pattern = "\\.r$", full.names = TRUE)
sapply(theme_files, source)

# ----------------------------------------------------------------
# 3. Load Models (Base first, then subclasses)
# ----------------------------------------------------------------
base_model_file <- file.path(root, "stats", "base_model.R")
if (file.exists(base_model_file)) {
  source(base_model_file)
  cat("[index.r] Loaded base_model.R\n")
} else {
  stop("[index.r] ERROR: base_model.R not found in stats/")
}

MODEL_REGISTRY <- list()  # initialize registry

# Load BaseModel first
source(file.path(root, "stats", "base_model.r"))

# Auto-load model subclasses
model_files <- list.files(file.path(root, "stats", "models"), pattern = "\\.r$", full.names = TRUE)
model_files <- model_files[!grepl("base_model\\.r$", basename(model_files), ignore.case = TRUE)]
sapply(model_files, source)

cat("[index.r] Models loaded:", paste(names(MODEL_REGISTRY), collapse = ", "), "\n")

# ----------------------------------------------------------------
# 4. Load AbstractModule + All Analysis Modules
# ----------------------------------------------------------------
source(file.path(root, "modules", "M0_Abstract_Module.r"))

module_files <- list.files(file.path(root, "modules"), pattern = "^M[0-9]+_.*\\.r$", full.names = TRUE)
sapply(module_files, source)

# ----------------------------------------------------------------
# 5. AnalysisPipeline Class
# ----------------------------------------------------------------
AnalysisPipeline <- R6::R6Class(
  "AnalysisPipeline",
  
  public = list(
    bib_file = NULL,
    modules  = NULL,
    out_dir  = NULL,
    df       = NULL,
    results  = list(),
    available_models = NULL,  # << inject here
    
    # New bibliographic metadata
    title    = NULL,
    date     = NULL,
    query    = NULL,
    keywords = NULL,
    
    # Constructor
    initialize = function(bib_file,
                          modules = c("M1_DataIngestion",
                                      "M2_Production",
                                      "M3_Countries",
                                      "M4_World_Context"),
                          out_dir = "results/") {
      self$bib_file <- bib_file
      self$modules  <- modules
      self$out_dir  <- out_dir
      self$available_models <- MODEL_REGISTRY

      if (!dir.exists(self$out_dir)) {
        dir.create(self$out_dir, recursive = TRUE)
        cat("[INFO] Created output directory:", self$out_dir, "\n")
      }
    },
    
    # ------------------------
    # Setter Methods
    # ------------------------
    setTitle = function(title) {
      self$title <- title; invisible(self)
    },
    setDate = function(date) {
      self$date <- date; invisible(self)
    },
    setQuery = function(query) {
      self$query <- query; invisible(self)
    },
    setKeywords = function(keywords) {
      self$keywords <- keywords; invisible(self)
    },
    
    # ------------------------
    # Run Modules
    # ------------------------
    run = function() {
      if (dir.exists(self$out_dir)) unlink(self$out_dir, recursive = TRUE, force = TRUE)
      dir.create(self$out_dir, recursive = TRUE, showWarnings = FALSE)

      # -------------------------------------------------
      # Debug: print available models
      # -------------------------------------------------
      cat("\n[AnalysisPipeline][run] Available models:\n")
      if (is.null(self$available_models) || length(self$available_models) == 0) {
        cat("  (none found)\n")
      } else {
        for (m in names(self$available_models)) {
          cat("  -", m, "\n")
        }
      }
      cat("====================================================\n\n")

      for (mod_name in self$modules) {
        cat("\n[INFO] Running", mod_name, "...\n")
        
        if (!exists(mod_name)) stop(paste("[ERROR] Module class", mod_name, "not found."))
        
        module_class <- get(mod_name)
        mod <- if (is.null(self$df)) module_class$new(self$bib_file) else module_class$new(self$df)
        
        mod$run()
        mod$export(out_dir = self$out_dir)
        self$results[[mod_name]] <- mod$results
        
        if (!is.null(mod$results$df)) self$df <- mod$results$df
      }
      
      cat("\n[INFO] Analysis complete. Results saved to", self$out_dir, "\n")
      invisible(self)
    },
    
    # ------------------------
    # Export Metadata + Results
    # ------------------------
    export_results_index = function(filename = "results_index.json") {
      index_file <- file.path(self$out_dir, filename)
      index_data <- list(
        metadata = list(
          title    = self$title,
          date     = self$date,
          query    = self$query,
          keywords = self$keywords,
          bib_file = self$bib_file
        ),
        modules = self$results
      )
      jsonlite::write_json(index_data, index_file, pretty = TRUE, auto_unbox = TRUE)
      cat("[INFO] Results index saved:", index_file, "\n")
    }
  )
)
