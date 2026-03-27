# ============================================================================
# examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r
# RBiblioSynth - New Architecture Example
# ============================================================================

# 1. CARGAR FUNCIONES DE LA ARQUITECTURA (Recursivo)
# --------------------------------------------------- #
cat("Loading architecture functions recursively...\n")

# Subimos dos niveles para llegar a la raíz del proyecto desde la carpeta del ejemplo
base_path <- "../../R"

if (dir.exists(base_path)) {
  # recursive = TRUE es la clave para entrar en module_m1, core, services, etc.
  function_files <- list.files(path = base_path, 
                               pattern = "\\.[Rr]$", 
                               full.names = TRUE, 
                               recursive = TRUE)
  
  # Cargamos todos los archivos encontrados
  if (length(function_files) > 0) {
    invisible(sapply(function_files, source))
    cat("  - Successfully loaded", length(function_files), "files from /R and subdirectories.\n")
  } else {
    stop("No se encontraron archivos .R en ", base_path)
  }
} else {
  stop("No se pudo encontrar la carpeta de origen: ", base_path)
}

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
  verbose        = TRUE
)

# 3. Load Data
# --------------------------------------------------- #
cat("\nLoading bibliographic data from data/scopus.bib...\n")

library(bibliometrix)
utils::data("bibtag", package = "bibliometrix", envir = environment())

file_path <- "data/scopus.bib"
if (!file.exists(file_path)) stop("Archivo no encontrado en data/scopus.bib")

bib_data <- bibliometrix::convert2df(
  file = file_path,
  dbsource = "scopus",
  format = "bibtex"
)

cat("Loaded", nrow(bib_data), "documents.\n")

# 4. Run M1: Main Information
# --------------------------------------------------- #
cat("\n=== Running M1: Main Information ===\n")

m1_result <- run_m1(bib_data, config = config, export = TRUE)
cat("\nM1 Status:", m1_result$status, "\n")

cat("\n--- Overview ---\n")
if (!is.null(m1_result$data$overview$main_indicators)) {
  print(m1_result$data$overview$main_indicators)
}

cat("\n=== M1 Complete ===\n")
cat("Results saved to:", file.path(config$output_dir, "m1"), "\n")

# 5. Run M2: Annual Production
# --------------------------------------------------- #
cat("\n=== Running M2: Annual Production ===\n")

# Derive a Year x Articles time series from the raw bibliographic data.
# PY is the Publication Year column produced by bibliometrix::convert2df.
py_col <- if ("PY" %in% names(bib_data)) "PY" else NULL

if (!is.null(py_col)) {
  annual_counts <- as.integer(table(bib_data[[py_col]]))
  annual_years  <- as.integer(names(table(bib_data[[py_col]])))
  annual_ts <- data.frame(Year = annual_years, Articles = annual_counts)

  # Sanity check: require at least 5 years for meaningful regression
  if (nrow(annual_ts) >= 5) {
    m2_result <- run_m2(annual_ts, config = config, export = TRUE)
    cat("\nM2 Status:", m2_result$status, "\n")
    if (!is.null(m2_result$data$regression$best_model$name)) {
      cat("Best regression model:", m2_result$data$regression$best_model$name,
          "| R2 =", m2_result$data$regression$best_model$R2, "\n")
    }
    cat("Results saved to:", file.path(config$output_dir, "m2"), "\n")
  } else {
    cat("M2 skipped: fewer than 5 publication years in dataset.\n")
    m2_result <- NULL
  }
} else {
  cat("M2 skipped: PY (publication year) column not found in bib_data.\n")
  m2_result <- NULL
}

cat("\n=== M2 Complete ===\n")

# 6. Run M3: Countries
# --------------------------------------------------- #
cat("\n=== Running M3: Countries ===\n")

# M3 consumes the same raw bibliographic data frame as M1.
# It extracts country information from the AU_CO column (author countries)
# produced by bibliometrix::convert2df.
m3_result <- run_m3(bib_data, config = config, export = TRUE)
cat("\nM3 Status:", m3_result$status, "\n")

if (!is.null(m3_result$data$production$production_summary)) {
  ps <- m3_result$data$production$production_summary
  cat("  Countries active:", ps$total_countries, "\n")
  cat("  Total articles  :", ps$total_articles,  "\n")
  if (!is.na(ps$gini_articles))
    cat("  Gini (articles) :", round(ps$gini_articles, 4), "\n")
}

cat("Results saved to:", file.path(config$output_dir, "m3"), "\n")
cat("\n=== M3 Complete ===\n")

# 7. Pipeline Summary
# --------------------------------------------------- #
cat("\n============================================================\n")
cat("Pipeline complete: M1 -> M2 -> M3\n")
cat("  M1:", m1_result$status, "\n")
cat("  M2:", if (!is.null(m2_result)) m2_result$status else "skipped", "\n")
cat("  M3:", m3_result$status, "\n")
cat("  Output root:", config$output_dir, "\n")
cat("============================================================\n")