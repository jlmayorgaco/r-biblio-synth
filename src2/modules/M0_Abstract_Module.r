# ================================================================
# M0_Abstract_Module — Base Class for All Bibliometric Modules
# ================================================================
M0_Abstract_Module <- R6::R6Class(
  "M0_Abstract_Module",
  
  public = list(
    input   = NULL,   # raw input (file path or dataframe)
    params  = list(), # parameters specific to the module
    results = list(), # analysis results
    plots   = list(), # ggplot objects
    tables  = list(), # optional summary tables

    module_index = "M0",
    
    # Constructor
    initialize = function(input = NULL, params = list(), module_index = "M0") {
      self$input <- input
      self$params <- params
      self$module_index <- module_index
    },
    
    # Abstract run()
    run = function() {
      stop("[AbstractModule] run() not implemented in child class.")
    },
    
    # ------------------------------------------------------------
    # Export function (JSON, CSV, PNG/SVG)
    # ------------------------------------------------------------
    export = function(out_dir = "results/", formats = c("json", "csv", "png", "svg")) {
      module_index <- self$module_index
      cat("[", module_index, "][export] Starting export...\n", sep = "")
      
      if (length(self$results) == 0) {
        stop("[", module_index, "][export] Nothing to export — run() first.")
      }
      
      module_dir <- file.path(out_dir, module_index)
      dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
      
      # --- JSON ---
      if ("json" %in% formats) {
        tryCatch({
          export_json(self$results, file.path(module_dir, paste0(module_index, "_results.json")))
        }, error = function(e) {
          warning("[", module_index, "][export] JSON export failed: ", e$message)
        })
      }
      
      # --- Plots ---
      if (any(formats %in% c("png", "svg"))) {
        for (name in names(self$plots)) {
          plot_obj <- self$plots[[name]]
          if (is.null(plot_obj)) next
          for (fmt in intersect(formats, c("png", "svg"))) {
            save_plot(
              plot_obj,
              file.path(module_dir, paste0(module_index, "_", name, ".", fmt)),
              device = fmt
            )
          }
        }
      }
      
      cat("[", module_index, "][export] Export complete. Files saved to ", module_dir, "\n", sep = "")
    },
    
    # ------------------------------------------------------------
    # Optional helpers
    # ------------------------------------------------------------
    save_plots = function(out_dir = "results/", prefix = "plot") {
      if (length(self$plots) == 0) {
        cat("[", self$module_index, "][save_plots] No plots to save.\n", sep = "")
        return(invisible(NULL))
      }
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      for (pname in names(self$plots)) {
        p <- self$plots[[pname]]
        file <- file.path(out_dir, paste0(prefix, "_", pname, ".png"))
        ggplot2::ggsave(file, p, dpi = 600, width = 3.5, height = 22.5)
        cat("[", self$module_index, "][save_plots] Saved plot:", file, "\n", sep = "")
      }
    },
    
    export_tables = function(out_dir = "results/", prefix = "table") {
      if (length(self$tables) == 0) {
        cat("[", self$module_index, "][export_tables] No tables to save.\n", sep = "")
        return(invisible(NULL))
      }
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      for (tname in names(self$tables)) {
        tbl <- self$tables[[tname]]
        file <- file.path(out_dir, paste0(prefix, "_", tname, ".csv"))
        utils::write.csv(tbl, file, row.names = FALSE)
        cat("[", self$module_index, "][export_tables] Saved table:", file, "\n", sep = "")
      }
    }
  )
)
