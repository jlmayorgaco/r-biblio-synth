# ============================================================================
# config.R - Enhanced Configuration Management for RBiblioSynth
# ============================================================================

#' Get default bibliometric configuration
#'
#' Returns a list with default configuration values for the package.
#' Now accepts parameters for easy customization.
#'
#' @param output_dir Output directory for results (default: "results")
#' @param export_plots Whether to export plots (default: TRUE)
#' @param export_json Whether to export JSON data (default: TRUE)
#' @param export_reports Whether to export text reports (default: TRUE)
#' @param theme_name Plot theme name: "ieee", "nature", "lancet" (default: "ieee")
#' @param verbose Print progress messages (default: TRUE)
#' @param top_n_countries Number of top countries to analyze (default: 10)
#' @param top_n_authors Number of top authors to analyze (default: 10)
#' @param top_n_sources Number of top sources to analyze (default: 10)
#' @param top_n_keywords Number of top keywords to analyze (default: 20)
#' @param dpi Plot resolution in DPI (default: 300)
#' @param plot_width Plot width in inches (default: 3.5)
#' @param plot_height Plot height in inches (default: 2.5)
#' @param parallel Enable parallel processing (default: FALSE)
#' @param n_cores Number of cores for parallel processing (NULL = auto-detect)
#' @param cache_dir Directory for caching results (default: "cache")
#' @param cache_enabled Enable result caching (default: TRUE)
#' @param log_level Logging level: "DEBUG", "INFO", "WARN", "ERROR" (default: "INFO")
#' @param log_file Optional log file path (default: NULL = console only)
#' @param ... Additional custom parameters
#'
#' @return A named list with configuration keys and values.
#' @export
#' @examples
#' # Default configuration
#' cfg <- biblio_config()
#'
#' # Custom configuration
#' cfg <- biblio_config(
#'   verbose = FALSE,
#'   parallel = TRUE,
#'   n_cores = 4,
#'   output_dir = "my_results"
#' )
biblio_config <- function(output_dir = "results",
                          export_plots = TRUE,
                          export_json = TRUE,
                          export_reports = TRUE,
                          theme_name = "ieee",
                          verbose = TRUE,
                          top_n_countries = 10,
                          top_n_authors = 10,
                          top_n_sources = 10,
                          top_n_keywords = 20,
                          dpi = 300,
                          plot_width = 3.5,
                          plot_height = 2.5,
                          parallel = FALSE,
                          n_cores = NULL,
                          cache_dir = "cache",
                          cache_enabled = TRUE,
                          log_level = "INFO",
                          log_file = NULL,
                          ...) {
  
  # Build configuration list
  config <- list(
    output_dir      = output_dir,
    export_plots    = export_plots,
    export_json     = export_json,
    export_reports  = export_reports,
    theme_name      = theme_name,
    verbose         = verbose,
    top_n_countries = top_n_countries,
    top_n_authors   = top_n_authors,
    top_n_sources   = top_n_sources,
    top_n_keywords  = top_n_keywords,
    dpi             = dpi,
    plot_width      = plot_width,
    plot_height     = plot_height,
    parallel        = parallel,
    n_cores         = n_cores,
    cache_dir       = cache_dir,
    cache_enabled   = cache_enabled,
    log_level       = log_level,
    log_file        = log_file
  )
  
  # Add any additional custom parameters
  custom_params <- list(...)
  if (length(custom_params) > 0) {
    config <- c(config, custom_params)
  }
  
  # Auto-detect cores if parallel enabled but n_cores not specified
  if (config$parallel && is.null(config$n_cores)) {
    config$n_cores <- max(1, parallel::detectCores() - 1)
  }
  
  # Validate configuration
  config <- validate_config(config)
  
  config
}

#' Validate configuration values
#' @keywords internal
validate_config <- function(config) {
  # Validate numeric parameters
  numeric_params <- c("dpi", "plot_width", "plot_height", 
                     "top_n_countries", "top_n_authors", 
                     "top_n_sources", "top_n_keywords")
  
  for (param in numeric_params) {
    if (param %in% names(config)) {
      val <- config[[param]]
      if (!is.null(val) && (!is.numeric(val) || val <= 0)) {
        warning(sprintf("Invalid value for %s: %s. Using default.", 
                       param, as.character(val)))
        config[[param]] <- biblio_config()[[param]]
      }
    }
  }
  
  # Validate logical parameters
  logical_params <- c("export_plots", "export_json", "export_reports", 
                     "verbose", "parallel", "cache_enabled")
  
  for (param in logical_params) {
    if (param %in% names(config)) {
      val <- config[[param]]
      if (!is.null(val) && !is.logical(val)) {
        warning(sprintf("Invalid value for %s: expected logical. Using default.", 
                       param))
        config[[param]] <- biblio_config()[[param]]
      }
    }
  }
  
  # Validate theme
  valid_themes <- c("ieee", "nature", "lancet", "default")
  if (!config$theme_name %in% valid_themes) {
    warning(sprintf("Invalid theme: %s. Using 'ieee'.", config$theme_name))
    config$theme_name <- "ieee"
  }
  
  # Validate log level
  valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (!config$log_level %in% valid_levels) {
    warning(sprintf("Invalid log level: %s. Using 'INFO'.", config$log_level))
    config$log_level <- "INFO"
  }
  
  # Ensure output directory exists
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Ensure cache directory exists if caching enabled
  if (config$cache_enabled && !dir.exists(config$cache_dir)) {
    dir.create(config$cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  config
}

#' Merge user configuration with defaults
#'
#' Merges a user-supplied configuration list with default values from
#' \code{\link{biblio_config}}. User values override defaults.
#'
#' @param config A named list of user configuration values.
#' @return A named list with merged configuration.
#' @export
#' @examples
#' cfg <- merge_biblio_config(list(verbose = FALSE, parallel = TRUE))
merge_biblio_config <- function(config = list()) {
  # If config has explicit parameters (not just a list), use them
  if (length(config) == 0) {
    return(biblio_config())
  }
  
  # Get defaults
  defaults <- biblio_config()
  
  # Merge user values
  for (nm in names(config)) {
    defaults[[nm]] <- config[[nm]]
  }
  
  # Re-validate merged config
  validate_config(defaults)
}

#' Create production-optimized configuration
#'
#' Returns a configuration optimized for production use with:
#' - Parallel processing enabled
#' - Caching enabled
#' - Logging to file
#' - Reduced verbosity
#'
#' @param output_dir Output directory
#' @param n_cores Number of cores (NULL = auto-detect)
#' @param log_file Log file path (default: "logs/pipeline.log")
#' @return Configuration list
#' @export
#' @examples
#' cfg <- config_production("results", n_cores = 8)
config_production <- function(output_dir = "results", 
                              n_cores = NULL,
                              log_file = "logs/pipeline.log") {
  biblio_config(
    output_dir = output_dir,
    verbose = FALSE,
    parallel = TRUE,
    n_cores = n_cores,
    cache_enabled = TRUE,
    log_level = "INFO",
    log_file = log_file,
    export_plots = TRUE,
    export_json = TRUE,
    export_reports = TRUE
  )
}

#' Create development/debug configuration
#'
#' Returns a configuration optimized for development with:
#' - Verbose output
#' - Debug logging
#' - No caching (to ensure fresh results)
#' - Sequential processing (easier debugging)
#'
#' @param output_dir Output directory
#' @return Configuration list
#' @export
#' @examples
#' cfg <- config_development("debug_results")
config_development <- function(output_dir = "results") {
  biblio_config(
    output_dir = output_dir,
    verbose = TRUE,
    parallel = FALSE,
    cache_enabled = FALSE,
    log_level = "DEBUG",
    export_plots = TRUE,
    export_json = TRUE,
    export_reports = TRUE
  )
}

#' Save configuration to file
#'
#' @param config Configuration list
#' @param file Output file path
#' @export
#' @examples
#' cfg <- biblio_config()
#' save_config(cfg, "my_config.json")
save_config <- function(config, file) {
  jsonlite::write_json(config, file, pretty = TRUE, auto_unbox = TRUE)
  invisible(TRUE)
}

#' Load configuration from file
#'
#' @param file JSON file path
#' @return Configuration list
#' @export
#' @examples
#' cfg <- load_config("my_config.json")
load_config <- function(file) {
  if (!file.exists(file)) {
    stop("Config file not found: ", file)
  }
  
  config <- jsonlite::fromJSON(file, simplifyVector = TRUE)
  validate_config(config)
}

#' Print configuration summary
#'
#' @param config Configuration list
#' @export
print_config <- function(config) {
  cat("\n=== RBiblioSynth Configuration ===\n\n")
  
  cat("Output Settings:\n")
  cat("  Output directory:", config$output_dir, "\n")
  cat("  Export plots:", config$export_plots, "\n")
  cat("  Export JSON:", config$export_json, "\n")
  cat("  Export reports:", config$export_reports, "\n")
  
  cat("\nVisualization Settings:\n")
  cat("  Theme:", config$theme_name, "\n")
  cat("  DPI:", config$dpi, "\n")
  cat("  Plot size:", config$plot_width, "x", config$plot_height, "inches\n")
  
  cat("\nAnalysis Settings:\n")
  cat("  Top N countries:", config$top_n_countries, "\n")
  cat("  Top N authors:", config$top_n_authors, "\n")
  cat("  Top N sources:", config$top_n_sources, "\n")
  cat("  Top N keywords:", config$top_n_keywords, "\n")
  
  cat("\nPerformance Settings:\n")
  cat("  Parallel processing:", config$parallel, "\n")
  if (config$parallel) {
    cat("  Number of cores:", config$n_cores, "\n")
  }
  cat("  Caching enabled:", config$cache_enabled, "\n")
  if (config$cache_enabled) {
    cat("  Cache directory:", config$cache_dir, "\n")
  }
  
  cat("\nLogging Settings:\n")
  cat("  Log level:", config$log_level, "\n")
  cat("  Log file:", config$log_file %||% "console only", "\n")
  cat("  Verbose:", config$verbose, "\n")
  
  cat("\n=====================================\n")
}