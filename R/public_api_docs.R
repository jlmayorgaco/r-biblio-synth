#' RBiblioSynth configuration helpers
#'
#' Create and normalize configuration lists for the public RBiblioSynth API.
#'
#' @name biblio_config
#' @aliases biblio_config merge_biblio_config print_config
#' @usage biblio_config(output_dir = "results", export = NULL, export_plots = TRUE,
#' export_json = TRUE, export_reports = TRUE, theme_name = "ieee",
#' verbose = TRUE, top_n_countries = 10, top_n_authors = 10,
#' top_n_sources = 10, top_n_keywords = 20, top_n_citations = 10,
#' dpi = 300, plot_width = 3.5, plot_height = 2.5, parallel = FALSE,
#' n_cores = NULL, cache_dir = "cache", cache_enabled = TRUE,
#' counting_mode = "full", dedup_method = "title_year_normalized",
#' report_format = "none", validate_strict = TRUE,
#' source_priority = c("WOS", "SCOPUS", "PUBMED", "CROSSREF", "OPENALEX", "GENERIC", "ORCID", "ROR"),
#' api_email = NULL, api_limit = 50, api_timeout = 30,
#' openalex_api_key = NULL, orcid_access_token = NULL,
#' enable_enrichment = FALSE,
#' enrichment_sources = c("crossref", "openalex", "orcid", "ror"),
#' m0_chunk_size_rows = 50000, m0_csv_delimiter = ",",
#' m0_csv_encoding = "UTF-8", advanced_analytics = TRUE,
#' advanced_fail_policy = "soft", bootstrap_n = 500, forecast_horizon = 5,
#' rolling_origin_min_train = 8, min_years_for_advanced_ts = 12,
#' min_countries_for_advanced_geo = 8, log_level = "INFO", log_file = NULL, ...)
#' merge_biblio_config(config = list())
#' print_config(config)
#' @param advanced_analytics Logical; enable optional journal-grade advanced
#'   analytics in modules that support them.
#' @param advanced_fail_policy Failure policy for optional advanced analytics:
#'   `"soft"` records omissions, `"hard"` raises errors.
#' @param bootstrap_n Bootstrap replications used by advanced uncertainty
#'   summaries.
#' @param forecast_horizon Forecast horizon used by temporal forecasting
#'   modules.
#' @param rolling_origin_min_train Minimum training length for rolling-origin
#'   cross-validation.
#' @param min_years_for_advanced_ts Minimum annual observations required by
#'   advanced temporal analytics.
#' @param min_countries_for_advanced_geo Minimum country coverage required by
#'   advanced geographic analytics.
#' @param ... Named configuration overrides.
#' @param config A configuration list.
#' @return `biblio_config()` returns a named list of normalized configuration
#'   values. `merge_biblio_config()` fills missing keys with package defaults.
#'   `print_config()` prints a concise summary and returns the input invisibly.
#' @rawNamespace export(biblio_config)
#' @rawNamespace export(merge_biblio_config)
#' @rawNamespace export(print_config)
NULL

#' RBiblioSynth pipeline runners
#'
#' Execute the hardened M0-M3 core either module by module or through the
#' official end-to-end pipeline entrypoint. For methodology-heavy reviews, use
#' the dedicated B-SLR workflow helpers, including journal-style paper
#' assembly.
#'
#' @name run_pipeline
#' @aliases run_pipeline run_m0 run_m1 run_m2 run_m3 validate_pipeline_result run_bslr bslr_protocol_template validate_bslr_protocol bslr_protocol_to_search_metadata bslr_assemble_paper
#' @usage run_pipeline(sources, modules = c("m0", "m1", "m2", "m3"), config = biblio_config(), prisma_spec = NULL, export = TRUE, screening_ledger = NULL, search_metadata = NULL, bslr_protocol = NULL, bslr = !is.null(bslr_protocol) || "bslr" %in% tolower(as.character(modules)))
#' run_m0(sources, config = biblio_config(), prisma_spec = NULL, screening_ledger = NULL, search_metadata = NULL, enrich = FALSE, export = TRUE)
#' run_m1(input, config = biblio_config(), export = TRUE)
#' run_m2(input, config = biblio_config(), export = TRUE)
#' run_m3(input, config = biblio_config(), export = TRUE)
#' run_bslr(sources, protocol, screening_ledger = NULL, prisma_spec = "auto", modules = c("m1", "m2", "m3"), config = biblio_config(), export = TRUE)
#' bslr_protocol_template(title = "B-SLR Study")
#' validate_bslr_protocol(protocol)
#' bslr_protocol_to_search_metadata(protocol)
#' bslr_assemble_paper(bslr_result, config = biblio_config(), render = TRUE)
#' validate_pipeline_result(x)
#' @param sources Named list of bibliographic source specifications.
#' @param modules Character vector selecting which public modules to run.
#' @param config Configuration list created with [biblio_config()].
#' @param prisma_spec Optional PRISMA specification for `run_m0()`.
#' @param screening_ledger Optional screening decisions ledger used by `run_m0()`
#'   and `run_bslr()`.
#' @param search_metadata Optional human-defined methodological metadata passed to
#'   `run_m0()`.
#' @param enrich Logical or character vector enabling M0 enrichment providers.
#' @param export Logical; when `TRUE`, write artifacts to disk.
#' @param bslr_protocol Optional B-SLR protocol. When supplied, `run_pipeline()`
#'   delegates orchestration to `run_bslr()` and returns M0-M3 plus `bslr`.
#' @param bslr Logical; force B-SLR pipeline mode.
#' @param input Module input object. For `run_m1()` and `run_m3()`, this is a
#'   bibliographic data frame. For `run_m2()`, this is an annual production
#'   table.
#' @param protocol A B-SLR protocol list or a JSON/YAML file path.
#' @param bslr_result Result object returned by [run_bslr()].
#' @param render Logical; when `TRUE`, attempt to render the paper bundle with
#'   Quarto if the executable is available.
#' @param title Title used in a generated B-SLR protocol template.
#' @param x Object to validate.
#' @return `run_pipeline()` returns a `biblio_pipeline_result`. Each `run_m*()`
#'   function returns a `biblio_module_result`. `run_bslr()` returns a
#'   `biblio_module_result` with nested `M0`-`M3` outputs plus methodological
#'   checkpoints, bibliometric mapping, and SLR/theorising scaffolds.
#'   `bslr_assemble_paper()` returns a paper-bundle descriptor containing Quarto
#'   and LaTeX sources plus any rendered outputs.
#' @rawNamespace export(run_pipeline)
#' @rawNamespace export(run_m0)
#' @rawNamespace export(run_m1)
#' @rawNamespace export(run_m2)
#' @rawNamespace export(run_m3)
#' @rawNamespace export(run_bslr)
#' @rawNamespace export(bslr_protocol_template)
#' @rawNamespace export(validate_bslr_protocol)
#' @rawNamespace export(bslr_protocol_to_search_metadata)
#' @rawNamespace export(bslr_assemble_paper)
#' @rawNamespace export(validate_pipeline_result)
NULL

#' M0 accessors and helpers
#'
#' Helper functions for retrieving validated artifacts from an `M0` result.
#'
#' @name m0_get
#' @aliases m0_get m0_get_bib_data m0_is_valid m0_prisma_template m0_get_authors m0_get_countries m0_get_sources m0_get_prisma
#' @usage m0_get(m0_result, slot)
#' m0_get_bib_data(m0_result)
#' m0_is_valid(m0_result)
#' m0_prisma_template(title = NULL)
#' m0_get_authors(m0_result)
#' m0_get_countries(m0_result)
#' m0_get_sources(m0_result)
#' m0_get_prisma(m0_result)
#' @param m0_result Result returned by [run_m0()].
#' @param slot Character key identifying the organized dataset to retrieve.
#' @param title Title used in the generated PRISMA template.
#' @return Accessors return organized M0 artifacts or validation metadata.
#'   `m0_prisma_template()` returns a PRISMA 2020 template list.
#' @rawNamespace export(m0_get)
#' @rawNamespace export(m0_get_bib_data)
#' @rawNamespace export(m0_is_valid)
#' @rawNamespace export(m0_prisma_template)
#' @rawNamespace export(m0_get_authors)
#' @rawNamespace export(m0_get_countries)
#' @rawNamespace export(m0_get_sources)
#' @rawNamespace export(m0_get_prisma)
NULL

#' Bootstrap utilities
#'
#' Convenience wrappers for bootstrap-based confidence intervals.
#'
#' @name bootstrap_ci
#' @aliases bootstrap_ci bootstrap_project bootstrap_mean bootstrap_median bootstrap_proportion bootstrap_gini
#' @usage bootstrap_project(project_root = getwd(), install_deps = TRUE, quiet = TRUE)
#' bootstrap_ci(data, statistic, R = 1000, conf_level = 0.95, method = "percentile", seed = NULL)
#' bootstrap_mean(x, R = 1000, conf_level = 0.95, seed = NULL)
#' bootstrap_median(x, R = 1000, conf_level = 0.95, seed = NULL)
#' bootstrap_proportion(successes, trials = NULL, R = 1000, conf_level = 0.95, seed = NULL)
#' bootstrap_gini(x, R = 1000, conf_level = 0.95, seed = NULL)
#' @param project_root Project root directory for local bootstrap.
#' @param install_deps Logical; when `TRUE`, install missing dependencies for
#'   local development workflows.
#' @param quiet Logical flag forwarded to dependency bootstrap.
#' @param data Vector or data frame to bootstrap.
#' @param statistic Function used to compute the statistic of interest.
#' @param R Number of bootstrap replicates.
#' @param conf_level Confidence level for the interval estimate.
#' @param method Bootstrap interval method.
#' @param seed Optional random seed.
#' @param x Numeric vector.
#' @param successes Either a binary vector or a scalar count of successes.
#' @param trials Optional number of trials when `successes` is scalar.
#' @return A list with point estimate, confidence interval bounds, bootstrap
#'   diagnostics, and status metadata.
#' @rawNamespace export(bootstrap_project)
#' @rawNamespace export(bootstrap_ci)
#' @rawNamespace export(bootstrap_mean)
#' @rawNamespace export(bootstrap_median)
#' @rawNamespace export(bootstrap_proportion)
#' @rawNamespace export(bootstrap_gini)
NULL

#' Safe arithmetic helpers
#'
#' Small utilities used throughout the package to avoid fragile arithmetic edge
#' cases in user-facing workflows.
#'
#' @name safe_divide
#' @aliases safe_divide safe_proportion safe_percentage
#' @usage safe_divide(numerator, denominator, default = 0)
#' safe_proportion(part, total, default = 0)
#' safe_percentage(part, total, default = NA)
#' @param numerator Numeric scalar or vector.
#' @param denominator Numeric scalar or vector.
#' @param default Fallback value returned for invalid operations.
#' @param part Numerator component of a proportion.
#' @param total Denominator component of a proportion.
#' @return A numeric value or vector with invalid divisions mapped to
#'   `default`.
#' @rawNamespace export(safe_divide)
#' @rawNamespace export(safe_proportion)
#' @rawNamespace export(safe_percentage)
NULL

#' Module registry helpers
#'
#' Inspect the public module registry exposed by RBiblioSynth.
#'
#' @name get_available_modules
#' @aliases get_available_modules get_module_metadata
#' @usage get_available_modules()
#' get_module_metadata(module_id)
#' @param module_id Module identifier such as `"m0"` or `"m3"`.
#' @return `get_available_modules()` returns the list of supported public
#'   modules. `get_module_metadata()` returns metadata for one module.
#' @rawNamespace export(get_available_modules)
#' @rawNamespace export(get_module_metadata)
NULL

#' Example bibliometric datasets
#'
#' Bundled sample datasets used in examples, smoke tests, and documentation.
#'
#' @name biblio_sample
#' @aliases biblio_sample annual_sample country_sample
#' @usage biblio_sample
#' annual_sample
#' country_sample
#' @format `biblio_sample` is a bibliographic data frame. `annual_sample` is an
#'   aggregated annual production table. `country_sample` is a country-level
#'   summary table derived from the sample bibliographic data.
#' @rawNamespace export(biblio_sample)
#' @rawNamespace export(annual_sample)
#' @rawNamespace export(country_sample)
NULL
