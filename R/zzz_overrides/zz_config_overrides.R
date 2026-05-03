# ============================================================================
# zz_config_overrides.R - Late-binding config overrides for journal-grade M0
# ============================================================================

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

  config <- list(
    output_dir = output_dir,
    export_plots = export_plots,
    export_json = export_json,
    export_reports = export_reports,
    theme_name = theme_name,
    verbose = verbose,
    top_n_countries = top_n_countries,
    top_n_authors = top_n_authors,
    top_n_sources = top_n_sources,
    top_n_keywords = top_n_keywords,
    top_n_citations = top_n_citations,
    dpi = dpi,
    plot_width = plot_width,
    plot_height = plot_height,
    parallel = parallel,
    n_cores = n_cores,
    cache_dir = cache_dir,
    cache_enabled = cache_enabled,
    counting_mode = counting_mode,
    dedup_method = dedup_method,
    report_format = report_format,
    validate_strict = validate_strict,
    source_priority = source_priority,
    api_email = api_email,
    api_limit = api_limit,
    api_timeout = api_timeout,
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
    log_level = log_level,
    log_file = log_file
  )

  custom_params <- list(...)
  if (length(custom_params) > 0) {
    config <- c(config, custom_params)
  }

  if (config$parallel && is.null(config$n_cores)) {
    config$n_cores <- max(1, parallel::detectCores() - 1)
  }

  validate_config(config)
}

validate_config <- function(config) {
  numeric_params <- c(
    "dpi", "plot_width", "plot_height",
    "top_n_countries", "top_n_authors", "top_n_sources",
    "top_n_keywords", "top_n_citations", "api_limit",
    "api_timeout", "m0_chunk_size_rows", "bootstrap_n",
    "forecast_horizon", "rolling_origin_min_train",
    "min_years_for_advanced_ts", "min_countries_for_advanced_geo"
  )

  for (param in numeric_params) {
    if (param %in% names(config)) {
      val <- config[[param]]
      if (!is.null(val) && (!is.numeric(val) || val <= 0)) {
        warning(sprintf("Invalid value for %s: %s. Using default.", param, as.character(val)))
        config[[param]] <- biblio_config()[[param]]
      }
    }
  }

  logical_params <- c(
    "export_plots", "export_json", "export_reports",
    "verbose", "parallel", "cache_enabled",
    "validate_strict", "enable_enrichment", "advanced_analytics"
  )

  for (param in logical_params) {
    if (param %in% names(config)) {
      val <- config[[param]]
      if (!is.null(val) && !is.logical(val)) {
        warning(sprintf("Invalid value for %s: expected logical. Using default.", param))
        config[[param]] <- biblio_config()[[param]]
      }
    }
  }

  valid_themes <- c("ieee", "nature", "lancet", "default")
  if (!config$theme_name %in% valid_themes) {
    warning(sprintf("Invalid theme: %s. Using 'ieee'.", config$theme_name))
    config$theme_name <- "ieee"
  }

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

  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (isTRUE(config$cache_enabled) && !dir.exists(config$cache_dir)) {
    dir.create(config$cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  config
}

merge_biblio_config <- function(config = list()) {
  if (length(config) == 0) {
    return(biblio_config())
  }

  defaults <- biblio_config()
  for (nm in names(config)) {
    defaults[[nm]] <- config[[nm]]
  }
  validate_config(defaults)
}
