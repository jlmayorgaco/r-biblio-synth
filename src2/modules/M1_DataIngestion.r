# ================================================================
# M1_DataIngestion — Module 1: Load & Convert Bibliographic Data
# ================================================================

M1_DataIngestion <- R6::R6Class(
  "M1_DataIngestion",
  inherit = M0_Abstract_Module,
  public = list(
    
    initialize = function(bib_file, dbsource = "scopus", format = "bibtex") {
      self$input <- bib_file
      self$params$dbsource <- dbsource
      self$params$format   <- format
      self$results <- list()
      self$plots   <- list()
    },
    
    # ------------------------
    # Run ingestion pipeline
    # ------------------------
    run = function() {
      cat("[M1] Reading bibliographic data from:", self$input, "\n")
      
      # --- Load raw data ---
      df <- tryCatch({
        suppressMessages(
          suppressWarnings(
            bibliometrix::convert2df(
              file     = self$input,
              dbsource = self$params$dbsource,
              format   = self$params$format
            )
          )
        )
      }, error = function(e) {
        stop("[M1] Error: could not read input file. ", conditionMessage(e))
      })
      
      # --- Rename columns (from config/column_maps.r) ---
      common_cols <- intersect(names(scopus_column_map), colnames(df))
      if (length(common_cols) > 0) {
        colnames(df)[match(common_cols, colnames(df))] <- scopus_column_map[common_cols]
      }
      
      # --- Add Country_List (helpers/string_helpers.r + helpers/data_helpers.r) ---
      if ("Affiliations" %in% colnames(df)) {
        df$Country_List <- lapply(as.character(df$Affiliations), extract_country_from_affiliation)
        df$Country_List <- normalize_countries(df$Country_List)
        
        # Optional: add Main_Country for easy aggregations
        df$Main_Country <- vapply(df$Country_List, function(x) {
          if (is.null(x) || length(x) == 0) return(NA_character_)
          x[1]   # pick first occurrence
        }, FUN.VALUE = character(1))
      }
      
      # --- Store results ---
      self$results$df <- df
      self$results$metadata <- list(
        n_docs    = nrow(df),
        n_columns = ncol(df),
        columns   = colnames(df),
        source    = self$params$dbsource,
        format    = self$params$format
      )
      
      cat("[M1] Ingestion completed. Documents loaded:", nrow(df), "\n")
      invisible(self)
    },
    
    # ------------------------
    # Export results
    # ------------------------
    export = function(out_dir = "results/", formats = c("json","csv")) {
      if (is.null(self$results$df)) stop("[M1] Nothing to export — run() has not been executed.")
      
      df_export <- self$results$df
      
      # Flatten list-cols for CSV only
      if ("csv" %in% formats) {
        df_export <- flatten_list_columns_for_csv(df_export)
        csv_file  <- file.path(out_dir, "M1_DataIngestion.csv")
        utils::write.csv(df_export, csv_file, row.names = FALSE)
        cat("[M1] Exported CSV:", csv_file, "\n")
      }
      
      # Save metadata & full results in JSON
      if ("json" %in% formats) {
        json_file <- file.path(out_dir, "M1_DataIngestion.json")
        io_helpers$export_json(self$results, json_file)
        cat("[M1] Exported JSON:", json_file, "\n")
      }
    }
  )
)
