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
#' @param counting_mode Counting mode for shared entities: "full" or "fractional".
#' @param dedup_method Deduplication strategy: "doi_exact",
#'   "title_year_normalized", or "title_year_fuzzy".
#' @param report_format Pipeline reporting format: "none", "quarto_html",
#'   "quarto_pdf", or "latex_bundle".
#' @param validate_strict Fail fast on broken contracts when TRUE.
#' @param source_priority Preferred source order when resolving field conflicts.
#' @param api_email Optional email passed to open scholarly APIs that support it.
#' @param api_limit Default maximum rows fetched per API request.
#' @param api_timeout Timeout in seconds for API calls.
#' @param openalex_api_key Optional OpenAlex API key.
#' @param orcid_access_token Optional ORCID bearer token for public/member API access.
#' @param enable_enrichment Whether to enrich merged records using API providers.
#' @param enrichment_sources Providers used when enrichment is enabled.
#' @param m0_chunk_size_rows Default chunk size used by large CSV readers in M0.
#' @param m0_csv_delimiter Default delimiter used by CSV readers in M0.
#' @param m0_csv_encoding Default encoding used by CSV readers in M0.
#' @param advanced_analytics Enable optional journal-grade advanced analytics.
#' @param advanced_fail_policy Failure policy for optional advanced analytics:
#'   "soft" records omissions, "hard" raises errors.
#' @param bootstrap_n Bootstrap replications for advanced uncertainty summaries.
#' @param forecast_horizon Forecast horizon used by temporal models.
#' @param rolling_origin_min_train Minimum training length for rolling-origin CV.
#' @param min_years_for_advanced_ts Minimum years required by advanced M2 analyses.
#' @param min_countries_for_advanced_geo Minimum countries required by advanced M3 analyses.
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
                           export = NULL,
                           export_plots = TRUE,
                           export_json = TRUE,
                           export_reports = TRUE,
                           theme_name = "ieee",
                           verbose = TRUE,
                           top_n_countries = 10,
                           top_n_authors = 10,
                           top_n_sources = 10,
                           top_n_keywords = 20,
                           top_n_citations = 10,
                           dpi = 300,
                           plot_width = 3.5,
                           plot_height = 2.5,
                           parallel = FALSE,
                           n_cores = NULL,
                           cache_dir = "cache",
                           cache_enabled = TRUE,
                           counting_mode = "full",
                           dedup_method = "title_year_normalized",
                           report_format = "none",
                           validate_strict = TRUE,
                           source_priority = c("WOS", "SCOPUS", "PUBMED", "CROSSREF", "OPENALEX", "GENERIC", "ORCID", "ROR"),
                           api_email = NULL,
                           api_limit = 50,
                           api_timeout = 30,
                           openalex_api_key = NULL,
                           orcid_access_token = NULL,
                           enable_enrichment = FALSE,
                           enrichment_sources = c("crossref", "openalex", "orcid", "ror"),
                           m0_chunk_size_rows = 50000,
                           m0_csv_delimiter = ",",
                           m0_csv_encoding = "UTF-8",
                           advanced_analytics = TRUE,
                           advanced_fail_policy = "soft",
                           bootstrap_n = 500,
                           forecast_horizon = 5,
                           rolling_origin_min_train = 8,
                           min_years_for_advanced_ts = 12,
                           min_countries_for_advanced_geo = 8,
                           log_level = "INFO",
                           log_file = NULL,
                           ...) {

  if (!is.null(export)) {
    export_plots <- export
    export_json <- export
    export_reports <- export
  }
  
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
    top_n_citations = top_n_citations,
    dpi             = dpi,
    plot_width      = plot_width,
    plot_height     = plot_height,
    parallel        = parallel,
    n_cores         = n_cores,
    cache_dir       = cache_dir,
    cache_enabled   = cache_enabled,
    counting_mode   = counting_mode,
    dedup_method    = dedup_method,
    report_format   = report_format,
    validate_strict = validate_strict,
    source_priority = source_priority,
    api_email       = api_email,
    api_limit       = api_limit,
    api_timeout     = api_timeout,
    openalex_api_key = openalex_api_key,
    orcid_access_token = orcid_access_token,
    enable_enrichment = enable_enrichment,
    enrichment_sources = enrichment_sources,
    m0_chunk_size_rows = m0_chunk_size_rows,
    m0_csv_delimiter = m0_csv_delimiter,
    m0_csv_encoding = m0_csv_encoding,
    advanced_analytics = advanced_analytics,
    advanced_fail_policy = advanced_fail_policy,
    bootstrap_n = bootstrap_n,
    forecast_horizon = forecast_horizon,
    rolling_origin_min_train = rolling_origin_min_train,
    min_years_for_advanced_ts = min_years_for_advanced_ts,
    min_countries_for_advanced_geo = min_countries_for_advanced_geo,
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
                      "top_n_sources", "top_n_keywords", "top_n_citations",
                      "api_limit", "api_timeout", "m0_chunk_size_rows",
                      "bootstrap_n", "forecast_horizon", "rolling_origin_min_train",
                      "min_years_for_advanced_ts", "min_countries_for_advanced_geo")
  
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
                     "verbose", "parallel", "cache_enabled", "validate_strict",
                     "enable_enrichment", "advanced_analytics")
  
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

  valid_counting_modes <- c("full", "fractional")
  if (!config$counting_mode %in% valid_counting_modes) {
    warning(sprintf("Invalid counting mode: %s. Using 'full'.", config$counting_mode))
    config$counting_mode <- "full"
  }

  valid_dedup_methods <- c("doi_exact", "title_year_normalized", "title_year_fuzzy")
  dedup_method <- config$dedup_method
  if (is.null(dedup_method) || length(dedup_method) == 0) {
    dedup_method <- "title_year_normalized"
  }
  dedup_method <- unique(as.character(dedup_method))
  invalid_dedup <- setdiff(dedup_method, valid_dedup_methods)
  if (length(invalid_dedup) > 0) {
    warning(sprintf(
      "Invalid dedup_method value(s): %s. Using 'title_year_normalized'.",
      paste(invalid_dedup, collapse = ", ")
    ))
    dedup_method <- "title_year_normalized"
  }
  if (!"doi_exact" %in% dedup_method) {
    dedup_method <- c("doi_exact", dedup_method)
  }
  config$dedup_method <- dedup_method

  valid_report_formats <- c("none", "quarto_html", "quarto_pdf", "latex_bundle")
  if (!config$report_format %in% valid_report_formats) {
    warning(sprintf("Invalid report format: %s. Using 'none'.", config$report_format))
    config$report_format <- "none"
  }

  valid_advanced_fail_policy <- c("soft", "hard")
  if (is.null(config$advanced_fail_policy) || !config$advanced_fail_policy %in% valid_advanced_fail_policy) {
    warning(sprintf("Invalid advanced_fail_policy: %s. Using 'soft'.", as.character(config$advanced_fail_policy)))
    config$advanced_fail_policy <- "soft"
  }

  if (is.null(config$source_priority) || length(config$source_priority) == 0) {
    config$source_priority <- biblio_config()$source_priority
  }
  config$source_priority <- unique(toupper(as.character(config$source_priority)))

  if (is.null(config$api_email) || !nzchar(trimws(as.character(config$api_email)))) {
    config$api_email <- NULL
  } else {
    config$api_email <- trimws(as.character(config$api_email))
  }

  if (is.null(config$openalex_api_key) || !nzchar(trimws(as.character(config$openalex_api_key)))) {
    env_key <- Sys.getenv("OPENALEX_API_KEY", unset = "")
    config$openalex_api_key <- if (nzchar(env_key)) env_key else NULL
  }

  if (is.null(config$orcid_access_token) || !nzchar(trimws(as.character(config$orcid_access_token)))) {
    env_token <- Sys.getenv("ORCID_ACCESS_TOKEN", unset = "")
    config$orcid_access_token <- if (nzchar(env_token)) env_token else NULL
  }

  if (is.null(config$enrichment_sources) || length(config$enrichment_sources) == 0) {
    config$enrichment_sources <- c("crossref", "openalex", "orcid", "ror")
  }
  config$enrichment_sources <- unique(tolower(as.character(config$enrichment_sources)))

  if (is.null(config$m0_csv_delimiter) || !nzchar(as.character(config$m0_csv_delimiter))) {
    config$m0_csv_delimiter <- ","
  } else {
    config$m0_csv_delimiter <- as.character(config$m0_csv_delimiter)[1]
  }

  if (is.null(config$m0_csv_encoding) || !nzchar(as.character(config$m0_csv_encoding))) {
    config$m0_csv_encoding <- "UTF-8"
  } else {
    config$m0_csv_encoding <- as.character(config$m0_csv_encoding)[1]
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
  cat("  Counting mode:", config$counting_mode, "\n")
  cat("  Deduplication:", paste(config$dedup_method, collapse = ", "), "\n")
  cat("  Report format:", config$report_format, "\n")
  cat("  Strict validation:", config$validate_strict, "\n")
  cat("  Source priority:", paste(config$source_priority, collapse = " > "), "\n")
  cat("  API limit:", config$api_limit, "\n")
  cat("  Enrichment enabled:", config$enable_enrichment, "\n")
  cat("  Enrichment providers:", paste(config$enrichment_sources, collapse = ", "), "\n")

  cat("\nAdvanced Analytics:\n")
  cat("  Enabled:", config$advanced_analytics, "\n")
  cat("  Failure policy:", config$advanced_fail_policy, "\n")
  cat("  Bootstrap n:", config$bootstrap_n, "\n")
  cat("  Forecast horizon:", config$forecast_horizon, "\n")
  cat("  Rolling-origin min train:", config$rolling_origin_min_train, "\n")
  cat("  Min years for advanced TS:", config$min_years_for_advanced_ts, "\n")
  cat("  Min countries for advanced GEO:", config$min_countries_for_advanced_geo, "\n")
  
  cat("\nLogging Settings:\n")
  cat("  Log level:", config$log_level, "\n")
  cat("  Log file:", config$log_file %||% "console only", "\n")
  cat("  Verbose:", config$verbose, "\n")
  
  cat("\n=====================================\n")
}
