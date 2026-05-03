# ============================================================================
# m1_run.R - Orchestrator for M1 (REFACTORED)
# ============================================================================

#' @export
run_m1 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)

  validation <- validate_m1_input(input, config)
  if (!validation$ok) {
    msg <- paste(validation$missing_columns, collapse = ", ")
    if (config$validate_strict) {
      cli::cli_abort("M1 validation failed: {msg}")
    }
    cli::cli_warn("M1 validation: {msg} missing")
  }

  data <- m1_compute_all(input, config)
  result <- m1_build_result(data, validation)
  result <- m1_render_all(result, data, config)
  result <- m1_build_tables(result, data, config)

  report <- build_m1_report(result, config)
  result <- attach_report_to_result(result, report)

  if (export) {
    exported <- export_m1(result, config)
    manifest <- build_m1_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

#' Compute all M1 metrics
m1_compute_all <- function(input, config) {
  data <- list()
  data$overview             <- compute_m1_overview(input, config)
  data$doc_types            <- compute_m1_doc_types(input, config)
  data$authors              <- compute_m1_authors(input, config)
  data$author_indices       <- compute_m1_author_indices(input, config)
  data$citations            <- compute_m1_citations(input, config)
  data$countries            <- compute_m1_countries(input, config)
  data$sources              <- compute_m1_sources(input, config)
  data$keywords             <- compute_m1_keywords(input, config)
  data$keyword_cooccurrence <- compute_m1_keyword_cooccurrence(input, config)
  data$keyword_burst        <- compute_m1_keyword_burst(input, config)
  data$bradford             <- compute_m1_bradford(input, config)
  data$lotka                <- compute_m1_lotka(input, config)
  data$collaboration        <- compute_m1_collaboration(input, config)
  data$price_law            <- compute_m1_price_law(input, config)
  data$hypotheses           <- compute_m1_hypotheses(input, config)
  data$topic_modeling       <- compute_m1_topic_modeling(input, config)
  data$semantic_advanced    <- compute_m1_semantic_advanced(input, data, config)
  data$citation_analysis    <- compute_m1_citation_analysis(input, config)
  data$author_career        <- compute_m1_author_career(input, config)
  data$narrative            <- compute_m1_narrative(data, config)
  data
}

m1_compute_all_debug <- function(input, config) {
  error_count <- 0
  steps <- list(
    list(name = "overview", fn = function() compute_m1_overview(input, config)),
    list(name = "doc_types", fn = function() compute_m1_doc_types(input, config)),
    list(name = "authors", fn = function() compute_m1_authors(input, config)),
    list(name = "author_indices", fn = function() compute_m1_author_indices(input, config)),
    list(name = "citations", fn = function() compute_m1_citations(input, config)),
    list(name = "countries", fn = function() compute_m1_countries(input, config)),
    list(name = "sources", fn = function() compute_m1_sources(input, config)),
    list(name = "keywords", fn = function() compute_m1_keywords(input, config)),
    list(name = "keyword_cooccurrence", fn = function() compute_m1_keyword_cooccurrence(input, config)),
    list(name = "keyword_burst", fn = function() compute_m1_keyword_burst(input, config)),
    list(name = "bradford", fn = function() compute_m1_bradford(input, config)),
    list(name = "lotka", fn = function() compute_m1_lotka(input, config)),
    list(name = "collaboration", fn = function() compute_m1_collaboration(input, config)),
    list(name = "price_law", fn = function() compute_m1_price_law(input, config)),
    list(name = "hypotheses", fn = function() compute_m1_hypotheses(input, config)),
    list(name = "topic_modeling", fn = function() compute_m1_topic_modeling(input, config)),
    list(name = "citation_analysis", fn = function() compute_m1_citation_analysis(input, config)),
    list(name = "author_career", fn = function() compute_m1_author_career(input, config))
  )
  
  result <- list()
  for (step in steps) {
    start_time <- Sys.time()
    cat("  [M1] Computing", step$name, "...\n")
    result[[step$name]] <- tryCatch({
      res <- step$fn()
      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
      
      if (is.null(res) || (is.list(res) && !is.null(res$status) && res$status != "success")) {
        cat(sprintf(" WARNING \n      [%s] internal status: %s\n", 
                    step$name, if(is.list(res)) res$status else "NULL"))
        error_count <- error_count + 1
      } else {
        cat(sprintf(" DONE ( %.2f s)\n", elapsed))
      }
      res
    }, error = function(e) {
      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
      cat(sprintf("\n  [M1] ERROR in %s after %.2f s: %s \n", step$name, elapsed, e$message))
      error_count <<- error_count + 1
      list(status = "error", message = e$message)
    })
    flush.console()
  }
  result
}

#' Build module result
m1_build_result <- function(data, validation) {
  new_module_result(
    module_id   = "m1",
    module_name = "Main Information",
    status      = if (validation$ok) "success" else "warning",
    inputs      = list(n_rows = validation$n_rows, n_cols = validation$n_cols),
    data        = data,
    diagnostics = list(warnings = character(), checks = list(validation = validation), notes = character())
  )
}

#' Render all plots
m1_render_all <- function(result, data, config) {
  result$artifacts$plots <- list(
    overview             = render_m1_overview(data$overview, config),
    doc_types            = render_m1_doc_types(data$doc_types, config),
    authors              = render_m1_authors(data$authors, config),
    author_indices       = render_m1_author_indices(data$author_indices, config),
    citations            = render_m1_citations(data$citations, config),
    countries            = render_m1_countries(data$countries, config),
    sources              = render_m1_sources(data$sources, config),
    keywords             = render_m1_keywords(data$keywords, config),
    keyword_cooccurrence = render_m1_keyword_cooccurrence(data$keyword_cooccurrence, config),
    keyword_burst        = render_m1_keyword_burst(data$keyword_burst, config),
    bradford             = render_m1_bradford(data$bradford, config),
    lotka                = render_m1_lotka(data$lotka, config),
    collaboration        = render_m1_collaboration(data$collaboration, config),
    price_law            = render_m1_price_law(data$price_law, config),
    hypotheses           = render_m1_hypotheses(data$hypotheses, config),
    topic_modeling       = render_m1_topic_modeling(data$topic_modeling, config),
    semantic_advanced    = render_m1_semantic_advanced(data$semantic_advanced, config),
    citation_analysis    = render_m1_citation_analysis(data$citation_analysis, config),
    author_career        = render_m1_author_career(data$author_career, config),
    narrative            = render_m1_narrative(data$narrative, config)
  )
  result$artifacts$plots <- m1_fill_core_plot_placeholders(result$artifacts$plots)
  result
}

m1_fill_core_plot_placeholders <- function(plot_sections) {
  core_specs <- list(
    overview = list(
      title = "Descriptive overview unavailable",
      message = "The bibliographic data did not contain enough valid records to render the overview summary.",
      layout = "single"
    ),
    authors = list(
      title = "Author productivity unavailable",
      message = "Author fields were missing or too sparse to support a publication-grade productivity chart.",
      layout = "single"
    ),
    citations = list(
      title = "Citation ranking unavailable",
      message = "Citation totals or readable citation labels were not sufficient to render a robust impact ranking.",
      layout = "single"
    ),
    countries = list(
      title = "Country contribution unavailable",
      message = "Country metadata could not be normalized into enough observations for a reliable comparison.",
      layout = "full"
    ),
    sources = list(
      title = "Source distribution unavailable",
      message = "Source or journal labels were missing or too sparse to identify a stable publication outlet profile.",
      layout = "single"
    ),
    keywords = list(
      title = "Keyword structure unavailable",
      message = "Keyword fields were absent or insufficient after cleaning, so no interpretable term-frequency chart was generated.",
      layout = "single"
    ),
    bradford = list(
      title = "Bradford zoning unavailable",
      message = "The source distribution did not support a stable Bradford-zone summary.",
      layout = "single"
    ),
    lotka = list(
      title = "Lotka distribution unavailable",
      message = "The author-publication distribution lacked enough non-zero observations for Lotka analysis.",
      layout = "single"
    ),
    collaboration = list(
      title = "Collaboration profile unavailable",
      message = "Author and affiliation information did not support a reliable collaboration profile.",
      layout = "full"
    ),
    hypotheses = list(
      title = "Automatic hypotheses unavailable",
      message = "The module did not produce enough statistical evidence to render a hypothesis-summary plot.",
      layout = "full"
    ),
    topic_modeling = list(
      title = "Topic model unavailable",
      message = "Text fields were too sparse after cleaning to fit and label a human-readable topic model.",
      layout = "full"
    )
  )

  for (section_nm in names(core_specs)) {
    section <- plot_sections[[section_nm]]
    has_content <- is.list(section) && is.list(section$plots) && length(Filter(Negate(is.null), section$plots)) > 0
    if (!has_content) {
      spec <- core_specs[[section_nm]]
      plot_sections[[section_nm]] <- list(
        status = "placeholder",
        plots = list(
          insufficient_data = ieee_no_data_plot(
            title = spec$title,
            message = spec$message,
            layout = spec$layout
          )
        )
      )
    }
  }

  plot_sections
}

#' Build all tables
m1_build_tables <- function(result, data, config) {
  result$artifacts$tables <- list(
    overview             = build_m1_overview_table(data$overview, config),
    doc_types            = build_m1_doc_types_table(data$doc_types, config),
    authors              = build_m1_authors_table(data$authors, config),
    author_indices       = build_m1_author_indices_table(data$author_indices, config),
    citations            = build_m1_citations_table(data$citations, config),
    countries            = build_m1_countries_table(data$countries, config),
    sources              = build_m1_sources_table(data$sources, config),
    keywords             = build_m1_keywords_table(data$keywords, config),
    keyword_cooccurrence = build_m1_keyword_cooccurrence_table(data$keyword_cooccurrence, config),
    keyword_burst        = build_m1_keyword_burst_table(data$keyword_burst, config),
    bradford             = build_m1_bradford_table(data$bradford, config),
    lotka                = build_m1_lotka_table(data$lotka, config),
    collaboration        = build_m1_collaboration_table(data$collaboration, config),
    price_law            = build_m1_price_law_table(data$price_law, config),
    hypotheses           = build_m1_hypotheses_table(data$hypotheses, config),
    topic_modeling       = build_m1_topic_modeling_table(data$topic_modeling, config),
    semantic_advanced    = build_m1_semantic_advanced_table(data$semantic_advanced, config),
    citation_analysis    = build_m1_citation_analysis_table(data$citation_analysis, config),
    author_career        = build_m1_author_career_table(data$author_career, config),
    narrative            = build_m1_narrative_table(data$narrative, config)
  )
  result
}

#' @export
export_m1 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots   <- character()
  exported_tables  <- character()
  exported_reports <- character()
  exported_jsons   <- character()

  if (config$export_plots) {
    for (nm in names(result$artifacts$plots)) {
      plot_section <- result$artifacts$plots[[nm]]
      if (!is.list(plot_section) || is.null(plot_section$plots) || length(plot_section$plots) == 0) {
        next
      }
      for (pnm in names(plot_section$plots)) {
        plot_obj <- plot_section$plots[[pnm]]
        if (is.null(plot_obj)) {
          next
        }
        plot_obj <- ieee_prepare_plot_for_export(
          plot_obj,
          module_id = "m1",
          section_id = nm,
          plot_id = pnm,
          config = config
        )
        spec <- ieee_get_plot_export_spec(
          plot_obj,
          config = config,
          section_id = nm,
          plot_id = pnm
        )
        p <- build_artifact_path("m1", "plots", paste0("m1_", nm, "_", pnm), "png", config)
        tryCatch({
          exported_paths <- export_plot_artifact(plot_obj, tools::file_path_sans_ext(p),
                                                 width = spec$width, height = spec$height, dpi = spec$dpi)
          exported_plots <- c(exported_plots, unname(exported_paths[!is.na(exported_paths)]))
        }, error = function(e) cli::cli_warn("Plot export failed: {pnm} - {e$message}"))
      }
    }
  }

  if (config$export_json) {
    for (nm in names(result$data)) {
      j <- build_artifact_path("m1", "json", paste0("m1_", nm), "json", config)
      tryCatch({
        write_json_artifact(result$data[[nm]], j)
        exported_jsons <- c(exported_jsons, j)
      }, error = function(e) cli::cli_warn("JSON export failed: {nm} - {e$message}"))
    }
  }

  if (length(result$artifacts$tables) > 0) {
    for (section_nm in names(result$artifacts$tables)) {
      table_section <- result$artifacts$tables[[section_nm]]
      flat_tables <- m1_flatten_table_collection(table_section)
      for (table_nm in names(flat_tables)) {
        csv_path <- build_artifact_path(
          "m1", "tables",
          paste0("m1_", section_nm, "_", table_nm), "csv", config
        )
        dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
        tryCatch({
          utils::write.csv(flat_tables[[table_nm]], csv_path, row.names = FALSE, na = "")
          exported_tables <- c(exported_tables, csv_path)
        }, error = function(e) cli::cli_warn("Table export failed: {table_nm} - {e$message}"))
      }
    }
  }

  if (config$export_reports && length(result$artifacts$reports) > 0) {
    report <- result$artifacts$reports[[1]]
    if (!is.null(report$lines) && length(report$lines) > 0) {
      r <- build_artifact_path("m1", "reports", "m1_report", "txt", config)
      write_text_report(report$lines, r)
      exported_reports <- c(exported_reports, r)
    }
    if (!is.null(report$tex) && length(report$tex) > 0) {
      t <- build_artifact_path("m1", "reports", "m1_report", "tex", config)
      writeLines(report$tex, t)
      exported_reports <- c(exported_reports, t)
    }
  }

  list(plots = exported_plots, tables = exported_tables, reports = exported_reports, files = exported_jsons)
}

m1_flatten_table_collection <- function(x, prefix = NULL) {
  if (is.data.frame(x)) {
    table_name <- if (is.null(prefix) || identical(prefix, "")) "table" else prefix
    out <- list(x)
    names(out) <- table_name
    return(out)
  }

  if (!is.list(x) || length(x) == 0) {
    return(list())
  }

  if ("table" %in% names(x) && is.data.frame(x$table)) {
    table_name <- if (is.null(prefix) || identical(prefix, "")) "table" else prefix
    out <- list(x$table)
    names(out) <- table_name
    return(out)
  }

  flattened <- list()
  child_names <- names(x)
  if (is.null(child_names)) {
    child_names <- as.character(seq_along(x))
  }

  for (i in seq_along(x)) {
    child_prefix <- if (is.null(prefix) || identical(prefix, "")) {
      child_names[i]
    } else {
      paste(prefix, child_names[i], sep = "_")
    }
    flattened <- c(flattened, m1_flatten_table_collection(x[[i]], child_prefix))
  }

  flattened
}
