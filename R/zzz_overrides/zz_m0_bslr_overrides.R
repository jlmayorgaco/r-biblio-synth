# ============================================================================
# zz_m0_bslr_overrides.R - Late-binding M0 overrides for B-SLR integration
# ============================================================================

#' @export
run_m0 <- function(sources,
                   config     = biblio_config(),
                   prisma_spec = NULL,
                   screening_ledger = NULL,
                   search_metadata = NULL,
                   enrich = FALSE,
                   export     = TRUE) {
  config <- merge_biblio_config(config)

  validation <- m0_validate_sources(sources)
  if (!validation$ok) {
    cli::cli_abort("M0 source validation failed: {validation$error}")
  }

  raw_list <- m0_load_all_sources(sources, config)
  loaded_validation <- m0_validate_loaded(raw_list)
  if (!loaded_validation$ok) {
    cli::cli_abort("M0 loading failed: {loaded_validation$error}")
  }

  merged <- m0_merge_sources(raw_list, config)
  merged_validation <- m0_validate_merged(merged)
  type_validation <- m0_validate_column_types(merged)

  enrichment <- list()
  enrichment_enabled <- isTRUE(config$enable_enrichment) || !identical(enrich, FALSE)
  if (enrichment_enabled) {
    enrichment_bundle <- m0_enrich_merged_records(
      merged,
      config,
      providers = if (isTRUE(enrich)) NULL else enrich
    )
    merged <- enrichment_bundle$merged
    enrichment <- enrichment_bundle$enrichment %||% list()
  }

  screening_df <- m0_read_screening_ledger(screening_ledger, merged)
  screening_summary <- m0_build_screening_summary(screening_df, merged)
  screening_template <- m0_screening_ledger_template(
    merged = merged,
    reviewers = search_metadata$reviewers %||% c("reviewer_1", "reviewer_2")
  )

  organized <- m0_organize_for_modules(merged, config)
  organized$provenance <- m0_build_provenance_table(merged)
  organized$screening_template <- screening_template
  if (nrow(screening_df) > 0) {
    organized$screening_ledger <- screening_df
  }
  if (isTRUE(screening_summary$ok) && is.data.frame(screening_summary$consensus)) {
    organized$screening_consensus <- screening_summary$consensus
    organized$screening_stage_counts <- screening_summary$stage_counts
    organized$screening_exclusion_reasons <- screening_summary$exclusion_reasons
    organized$screening_decision_matrix <- screening_summary$decision_matrix
  }
  if (length(enrichment) > 0) {
    for (nm in names(enrichment)) {
      if (is.data.frame(enrichment[[nm]]) || is.list(enrichment[[nm]])) {
        organized[[nm]] <- enrichment[[nm]]
      }
    }
  }

  source_summary <- m0_build_source_summary(raw_list, sources)
  dedup_summary <- m0_build_dedup_summary(raw_list, merged, config)
  quality <- m0_check_data_quality(merged)
  quality_report <- m0_create_quality_report(quality)

  result <- m0_build_result(raw_list, merged, organized, validation)
  result$data$source_summary <- source_summary
  result$data$dedup_summary <- dedup_summary
  result$data$quality <- quality
  result$data$quality_report <- quality_report
  result$data$screening_ledger <- screening_df
  result$data$screening_summary <- screening_summary
  result$data$screening_template <- screening_template
  result$data$search_metadata <- search_metadata
  result$data$enrichment <- enrichment
  result$diagnostics$checks$loaded <- loaded_validation
  result$diagnostics$checks$merged <- merged_validation
  result$diagnostics$checks$column_types <- type_validation
  result$diagnostics$checks$quality <- quality
  if (enrichment_enabled) {
    result$diagnostics$notes <- c(result$diagnostics$notes, "API enrichment applied")
  }
  if (nrow(screening_df) > 0) {
    result$diagnostics$checks$screening <- screening_summary
  }
  if (!isTRUE(type_validation$ok)) {
    result$diagnostics$warnings <- c(
      result$diagnostics$warnings,
      paste("M0 column type issues:", paste(names(type_validation$issues), collapse = ", "))
    )
  }
  if (!isTRUE(merged_validation$ok)) {
    result$diagnostics$warnings <- c(
      result$diagnostics$warnings,
      paste("Merged data missing columns:", paste(merged_validation$missing, collapse = ", "))
    )
  }
  if (!is.null(loaded_validation$warning)) {
    result$diagnostics$warnings <- c(result$diagnostics$warnings, loaded_validation$warning)
  }
  result$artifacts$tables <- c(
    result$artifacts$tables,
    list(screening_template = screening_template)
  )
  if (isTRUE(screening_summary$ok)) {
    result$artifacts$tables <- c(
      result$artifacts$tables,
      list(
        screening_ledger = screening_df,
        screening_consensus = screening_summary$consensus %||% data.frame(),
        screening_stage_counts = screening_summary$stage_counts %||% data.frame(),
        screening_exclusion_reasons = screening_summary$exclusion_reasons %||% data.frame(),
        screening_decision_matrix = screening_summary$decision_matrix %||% data.frame()
      )
    )
    result$artifacts$reports <- c(
      result$artifacts$reports,
      list(screening = m0_build_screening_report(screening_summary))
    )
  }

  if (!is.null(prisma_spec)) {
    user_prisma <- if (!identical(prisma_spec, "auto")) m0_read_prisma_spec(prisma_spec) else NULL
    prisma_bundle <- m0_build_prisma_bundle(
      raw_list = raw_list,
      merged = merged,
      sources = sources,
      prisma_spec = user_prisma,
      screening_summary = screening_summary,
      search_metadata = search_metadata,
      config = config
    )
    result$artifacts$reports <- c(
      result$artifacts$reports,
      list(
        prisma = prisma_bundle$report,
        prisma_methodology = prisma_bundle$methodology
      )
    )
    result$artifacts$plots <- c(result$artifacts$plots, list(prisma = prisma_bundle$diagram))
    result$data$prisma <- prisma_bundle$spec
    result$data$prisma_validation <- prisma_bundle$validation
    result$diagnostics$checks$prisma <- prisma_bundle$validation
    if (!isTRUE(prisma_bundle$validation$ok)) {
      result$diagnostics$warnings <- c(result$diagnostics$warnings, prisma_bundle$validation$warnings)
    }
  }

  if (export) {
    exported <- m0_export(result, config)
    manifest <- m0_build_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

m0_build_prisma_bundle <- function(raw_list,
                                   merged,
                                   sources,
                                   prisma_spec = NULL,
                                   screening_summary = NULL,
                                   search_metadata = NULL,
                                   config = biblio_config()) {
  user_spec <- if (!is.null(prisma_spec) && !identical(prisma_spec, "auto")) prisma_spec else NULL
  source_summary <- m0_build_source_summary(raw_list, sources)
  dedup_summary <- m0_build_dedup_summary(raw_list, merged, config)
  prisma_data <- m0_autobuild_prisma_spec(raw_list, merged, sources, user_spec, screening_summary, config)
  if (!is.null(search_metadata)) {
    prisma_data$methods <- m0_recursive_fill(prisma_data$methods %||% list(), list(search_metadata = search_metadata))
  }
  validation <- m0_validate_prisma_flow(prisma_data)
  report <- m0_build_prisma_report(prisma_data, config)
  methodology <- m0_build_prisma_methodology(
    prisma_data,
    source_summary,
    dedup_summary,
    screening_summary = screening_summary,
    search_metadata = search_metadata,
    config = config
  )
  diagram <- m0_render_prisma_diagram(prisma_data, config)

  list(
    spec = prisma_data,
    validation = validation,
    report = report,
    methodology = methodology,
    diagram = diagram
  )
}

m0_build_prisma_methodology <- function(prisma_data,
                                        source_summary,
                                        dedup_summary,
                                        screening_summary = NULL,
                                        search_metadata = NULL,
                                        config = biblio_config()) {
  source_lines <- if (is.data.frame(source_summary) && nrow(source_summary) > 0) {
    apply(source_summary, 1, function(row) {
      paste0(
        "  - ", row[["source_name"]], " [", row[["db"]], ", ", row[["prisma_role"]], ", ", row[["mode"]] %||% "file", "]: ",
        row[["n_records"]], " records",
        if (!is.na(row[["search_date"]]) && nzchar(row[["search_date"]])) paste0(", search date ", row[["search_date"]]) else "",
        if (!is.na(row[["query"]]) && nzchar(row[["query"]])) paste0(", query: ", row[["query"]]) else "",
        if (!is.na(row[["endpoint"]]) && nzchar(row[["endpoint"]])) paste0(", endpoint: ", row[["endpoint"]]) else ""
      )
    })
  } else {
    character()
  }

  dd <- if (is.data.frame(dedup_summary) && nrow(dedup_summary) > 0) dedup_summary[1, , drop = FALSE] else data.frame()
  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()
  methods <- prisma_data$methods %||% list()
  search_metadata <- search_metadata %||% methods$search_metadata %||% list()
  search_queries <- search_metadata$queries %||% list()
  primary_query <- search_queries$primary %||% NULL
  query_map <- search_queries$by_database %||% list()
  secondary_databases <- paste(search_metadata$secondary_databases %||% character(), collapse = ", ")
  doc_types <- paste(search_metadata$document_types %||% character(), collapse = ", ")
  reviewers <- paste(search_metadata$reviewers %||% character(), collapse = ", ")
  methodological_mode <- if (isTRUE(screening_summary$ok)) "full-review" else "counts-only"
  reliability <- screening_summary$reliability %||% list()
  pending_actions <- unique(c(
    if (!isTRUE(screening_summary$ok)) {
      c(
        "Provide a reviewer-level screening ledger with stage, decision, reason, and decision date.",
        "Re-run M0 with explicit screening decisions to promote PRISMA from counts-only to full-review."
      )
    },
    if (isTRUE(screening_summary$ok)) screening_summary$pending_actions %||% character(),
    if (length(search_metadata$reviewers %||% character()) == 0) {
      "Declare the reviewers responsible for manual screening and adjudication."
    },
    if (bslr_is_empty_field(search_metadata$conflict_rule %||% NULL)) {
      "Declare the conflict-resolution rule used when reviewers disagree."
    },
    if (bslr_is_empty_field(search_metadata$screening_tool %||% NULL)) {
      "Declare the screening platform or workflow used for manual review."
    }
  ))

  lines <- c(
    "============================================================",
    "PRISMA 2020 Methodology Narrative",
    paste("Generated:", Sys.time()),
    "============================================================",
    ""
  )

  if (!is.null(prisma_data$title)) {
    lines <- c(lines, paste("Review:", prisma_data$title), "")
  }

  if (length(search_metadata) > 0) {
    lines <- c(
      lines,
      "Review Perimeter",
      paste0("  Topic: ", search_metadata$review_topic %||% "NA"),
      paste0("  Research question: ", search_metadata$research_question %||% "NA"),
      if (!bslr_is_empty_field(search_metadata$research_gap %||% NULL)) paste0("  Research gap: ", search_metadata$research_gap) else NULL,
      paste0("  Inclusion criteria: ", paste(search_metadata$inclusion_criteria %||% character(), collapse = " | ")),
      paste0("  Exclusion criteria: ", paste(search_metadata$exclusion_criteria %||% character(), collapse = " | ")),
      ""
    )
  }

  lines <- c(
    lines,
    "Methodological Mode",
    paste0("  Mode: ", methodological_mode),
    paste0(
      "  Status: ",
      if (identical(methodological_mode, "full-review")) {
        "PRISMA counts were derived from explicit screening decisions."
      } else {
        "PRISMA remains partially auto-derived from runtime counts because no usable screening ledger was supplied."
      }
    ),
    "",
    "Data Sources",
    if (length(source_lines) > 0) source_lines else "  - No source registry available.",
    "",
    "Workflow",
    paste0(
      "  Records were identified from database sources (n = ", id$records_database %||% 0,
      ") and other sources (n = ", id$records_other %||% 0, ")."
    ),
    paste0(
      "  Deduplication removed ", id$duplicates_removed %||% 0,
      " records using: ", methods$deduplication %||% "unspecified", "."
    ),
    if (nrow(dd) > 0) paste0(
      "  Raw records: ", dd$total_raw_records,
      "; unique records after deduplication: ", dd$unique_records,
      "; multi-source records retained: ", dd$multi_source_records, "."
    ) else "  Deduplication summary unavailable.",
    paste0(
      "  Screening considered ", sc$records_screened %||% 0,
      " records, with ", sc$excluded_screening %||% 0,
      " exclusions at title/abstract level."
    ),
    paste0(
      "  Eligibility review assessed ", el$fulltext_assessed %||% 0,
      " full-text records and excluded ", el$excluded_fulltext %||% 0, "."
    ),
    paste0(
      "  The final synthesis included ", inc$studies_included %||% 0, " studies."
    ),
    ""
  )

  if (length(search_metadata) > 0) {
    lines <- c(
      lines,
      "Search Metadata",
      paste0("  Primary database: ", search_metadata$primary_database %||% "NA"),
      paste0("  Secondary databases: ", if (nzchar(secondary_databases)) secondary_databases else "NA"),
      paste0("  Search date: ", search_metadata$first_run_date %||% "NA"),
      paste0("  Cross-check date: ", search_metadata$crosscheck_date %||% "NA"),
      paste0("  Time span: ", search_metadata$time_span$start %||% "NA", " to ", search_metadata$time_span$end %||% "NA"),
      paste0("  Language restriction: ", search_metadata$language %||% "NA"),
      paste0("  Document types: ", if (nzchar(doc_types)) doc_types else "NA"),
      paste0("  Primary query: ", primary_query %||% "NA"),
      if (length(query_map) > 0) paste0("  Database-specific queries: ", paste(sprintf("%s=%s", names(query_map), unlist(query_map)), collapse = " | ")) else NULL,
      ""
    )
  }

  if (!is.null(el$excluded_reasons) && length(el$excluded_reasons) > 0) {
    lines <- c(lines, "Exclusion Reasons")
    for (reason in names(el$excluded_reasons)) {
      lines <- c(lines, paste0("  - ", reason, ": ", el$excluded_reasons[[reason]]))
    }
    lines <- c(lines, "")
  }

  if (!is.null(prisma_data$quality) && !is.null(prisma_data$quality$tool) && nzchar(prisma_data$quality$tool %||% "")) {
    qa <- prisma_data$quality
    lines <- c(
      lines,
      "Quality Assessment",
      paste0("  Tool: ", qa$tool),
      paste0("  Low risk: ", qa$low_risk %||% 0, "; High risk: ", qa$high_risk %||% 0, "; Unclear: ", qa$unclear %||% 0),
      ""
    )
  }

  if (isTRUE(methods$auto_generated)) {
    auto_flags <- methods$auto_derived_stages %||% list()
    auto_stages <- names(auto_flags)[unlist(auto_flags)]
    if (length(auto_stages) == 0) auto_stages <- "none"
    lines <- c(
      lines,
      paste0(
        "Note: the following PRISMA stages were auto-derived from M0 runtime counts: ",
        paste(auto_stages, collapse = ", "), "."
      ),
      ""
    )
  }

  runtime_counts <- methods$runtime_counts %||% list()
  if (length(runtime_counts) > 0) {
    lines <- c(
      lines,
      "Runtime Import Counts",
      paste0("  Database records loaded by M0: ", runtime_counts$records_database %||% 0, "."),
      paste0("  Other-source records loaded by M0: ", runtime_counts$records_other %||% 0, "."),
      paste0("  Unique records after runtime deduplication: ", runtime_counts$records_after_dedup %||% 0, "."),
      ""
    )
  }

  consistency_warnings <- methods$consistency_warnings %||% character()
  if (length(consistency_warnings) > 0) {
    lines <- c(lines, "Consistency Notes", paste0("  - ", consistency_warnings), "")
  }

  if (isTRUE(screening_summary$ok)) {
    lines <- c(
      lines,
      "Screening Ledger",
      paste0("  Screening decisions recorded: ", screening_summary$n_decisions %||% 0, "."),
      paste0("  Documents with explicit screening decisions: ", screening_summary$n_documents %||% 0, "."),
      paste0("  Full-review ready: ", if (isTRUE(screening_summary$full_review_ready)) "yes" else "no", "."),
      paste0("  Unresolved conflicts: ", screening_summary$unresolved_conflicts %||% 0, "."),
      paste0("  Reviewers: ", if (nzchar(reviewers)) reviewers else "NA"),
      paste0("  Conflict rule: ", search_metadata$conflict_rule %||% "NA"),
      paste0("  Screening tool: ", search_metadata$screening_tool %||% "NA"),
      paste0("  Quality tool: ", search_metadata$quality_tool %||% "NA")
    )
    if (identical(reliability$status %||% "", "success")) {
      lines <- c(
        lines,
        paste0("  Reviewer agreement (mean Cohen's kappa): ", round(reliability$cohens_kappa_mean %||% NA_real_, 3), "."),
        paste0("  Fleiss' kappa: ", round(reliability$fleiss_kappa %||% NA_real_, 3), "."),
        paste0("  Krippendorff's alpha: ", round(reliability$krippendorffs_alpha %||% NA_real_, 3), "."),
        paste0("  Percent agreement: ", round(100 * (reliability$percent_agreement %||% NA_real_), 1), "%.")
      )
    } else {
      lines <- c(
        lines,
        "  Reviewer agreement statistics are not available yet because overlapping final decisions were insufficient."
      )
    }
    lines <- c(lines, "")
  } else {
    lines <- c(
      lines,
      "Screening Ledger",
      "  No usable reviewer-level screening ledger was supplied.",
      "  The current PRISMA narrative is therefore counts-only and cannot substitute for a documented manual screening workflow.",
      ""
    )
  }

  if (length(pending_actions) > 0) {
    lines <- c(
      lines,
      "Pending Human Actions",
      paste0("  - ", pending_actions),
      ""
    )
  }

  tex <- c(
    "\\section*{PRISMA 2020 Methodology}",
    paste0("Methodological mode: ", m0_escape_latex(methodological_mode), "."),
    paste0(
      "Records were identified from database sources (n = ", id$records_database %||% 0,
      ") and other sources (n = ", id$records_other %||% 0, "). "
    ),
    paste0(
      "Deduplication removed ", id$duplicates_removed %||% 0,
      " records using ", m0_escape_latex(methods$deduplication %||% "unspecified"), "."
    ),
    paste0(
      "Screening considered ", sc$records_screened %||% 0, " records; ",
      sc$excluded_screening %||% 0, " were excluded at title/abstract level. "
    ),
    paste0(
      "Eligibility assessment reviewed ", el$fulltext_assessed %||% 0,
      " full-text records and excluded ", el$excluded_fulltext %||% 0, ". "
    ),
    paste0(
      "The final synthesis included ", inc$studies_included %||% 0, " studies."
    )
  )

  if (length(search_metadata) > 0) {
    tex <- c(
      tex,
      paste0(
        " The primary database was ", m0_escape_latex(search_metadata$primary_database %||% "NA"),
        " and the main search string was ", m0_escape_latex(primary_query %||% "NA"), "."
      ),
      paste0(
        " The search covered ", m0_escape_latex(search_metadata$time_span$start %||% "NA"),
        " to ", m0_escape_latex(search_metadata$time_span$end %||% "NA"),
        ", restricted to ", m0_escape_latex(search_metadata$language %||% "NA"),
        " and document types ", m0_escape_latex(doc_types), "."
      )
    )
  }

  if (isTRUE(screening_summary$ok)) {
    tex <- c(
      tex,
      paste0(
        " Explicit screening decisions were recorded for ",
        screening_summary$n_documents %||% 0,
        " documents."
      ),
      if (identical(reliability$status %||% "", "success")) {
        paste0(
          " Reviewer agreement yielded a mean Cohen's $\\kappa$ of ",
          round(reliability$cohens_kappa_mean %||% NA_real_, 3),
          " and Fleiss' $\\kappa$ of ",
          round(reliability$fleiss_kappa %||% NA_real_, 3),
          ", with Krippendorff's $\\alpha$ of ",
          round(reliability$krippendorffs_alpha %||% NA_real_, 3),
          "."
        )
      } else {
        " Inter-rater reliability statistics were not available because overlapping reviewer decisions were insufficient."
      }
    )
  } else {
    tex <- c(
      tex,
      " No reviewer-level screening ledger was supplied, so PRISMA remains a counts-only reconstruction rather than a documented full-review workflow."
    )
  }

  if (length(pending_actions) > 0) {
    tex <- c(
      tex,
      paste0(
        " Pending human actions: ",
        m0_escape_latex(paste(pending_actions, collapse = " ; ")),
        "."
      )
    )
  }

  list(lines = lines, tex = tex)
}

#' @export
m0_export <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_files   <- character()
  exported_reports <- character()
  exported_plots   <- character()
  exported_tables  <- character()

  if (config$export_json) {
    rds_path <- build_artifact_path("m0", "files", "bib_merged", "rds", config)
    tryCatch({
      saveRDS(result$data$bib_merged, rds_path)
      exported_files <- c(exported_files, rds_path)
    }, error = function(e) cli::cli_warn("M0 RDS export failed: {e$message}"))

    for (nm in names(result$data$organized)) {
      j <- build_artifact_path("m0", "json", paste0("m0_org_", nm), "json", config)
      tryCatch({
        write_json_artifact(result$data$organized[[nm]], j)
        exported_files <- c(exported_files, j)
      }, error = function(e) cli::cli_warn("M0 JSON export failed [{nm}]: {e$message}"))
    }

    optional_payloads <- list(
      source_summary = result$data$source_summary,
      dedup_summary = result$data$dedup_summary,
      quality = result$data$quality,
      prisma = result$data$prisma,
      prisma_validation = result$data$prisma_validation,
      screening_summary = result$data$screening_summary,
      search_metadata = result$data$search_metadata
    )

    for (nm in names(optional_payloads)) {
      payload <- optional_payloads[[nm]]
      if (!is.null(payload)) {
        j <- build_artifact_path("m0", "json", paste0("m0_", nm), "json", config)
        tryCatch({
          write_json_artifact(payload, j)
          exported_files <- c(exported_files, j)
        }, error = function(e) cli::cli_warn("M0 JSON export failed [{nm}]: {e$message}"))
      }
    }
  }

  if (config$export_reports && !is.null(result$data$quality_report$report_lines)) {
    qp <- build_artifact_path("m0", "reports", "m0_quality_report", "txt", config)
    write_text_report(result$data$quality_report$report_lines, qp)
    exported_reports <- c(exported_reports, qp)
  }

  if (config$export_reports && "screening" %in% names(result$artifacts$reports)) {
    screening_lines <- result$artifacts$reports$screening$lines
    if (!is.null(screening_lines) && length(screening_lines) > 0) {
      sp <- build_artifact_path("m0", "reports", "m0_screening_summary", "txt", config)
      write_text_report(screening_lines, sp)
      exported_reports <- c(exported_reports, sp)
    }
  }

  if (length(result$artifacts$tables) > 0) {
    for (nm in names(result$artifacts$tables)) {
      tab <- result$artifacts$tables[[nm]]
      if (!is.data.frame(tab)) next
      path <- build_artifact_path("m0", "tables", paste0("m0_", nm), "csv", config)
      utils::write.csv(tab, path, row.names = FALSE, na = "")
      exported_tables <- c(exported_tables, path)
    }
  }

  if (config$export_reports && "prisma" %in% names(result$artifacts$reports)) {
    prisma_lines <- result$artifacts$reports$prisma$lines
    if (!is.null(prisma_lines) && length(prisma_lines) > 0) {
      rp <- build_artifact_path("m0", "reports", "m0_prisma_report", "txt", config)
      write_text_report(prisma_lines, rp)
      exported_reports <- c(exported_reports, rp)
    }

    prisma_tex <- result$artifacts$reports$prisma$tex
    if (!is.null(prisma_tex) && length(prisma_tex) > 0) {
      rp_tex <- build_artifact_path("m0", "reports", "m0_prisma_report", "tex", config)
      writeLines(prisma_tex, rp_tex)
      exported_reports <- c(exported_reports, rp_tex)
    }
  }

  if (config$export_reports && "prisma_methodology" %in% names(result$artifacts$reports)) {
    meth_lines <- result$artifacts$reports$prisma_methodology$lines
    if (!is.null(meth_lines) && length(meth_lines) > 0) {
      mp <- build_artifact_path("m0", "reports", "m0_prisma_methodology", "txt", config)
      write_text_report(meth_lines, mp)
      exported_reports <- c(exported_reports, mp)
    }

    meth_tex <- result$artifacts$reports$prisma_methodology$tex
    if (!is.null(meth_tex) && length(meth_tex) > 0) {
      mp_tex <- build_artifact_path("m0", "reports", "m0_prisma_methodology", "tex", config)
      writeLines(meth_tex, mp_tex)
      exported_reports <- c(exported_reports, mp_tex)
    }
  }

  if (config$export_plots && "prisma" %in% names(result$artifacts$plots)) {
    prisma_plot <- result$artifacts$plots$prisma
    if (!is.null(prisma_plot)) {
      pp <- build_artifact_path("m0", "plots", "m0_prisma_diagram", "png", config)
      tryCatch({
        plot_paths <- export_plot_artifact(
          prisma_plot,
          tools::file_path_sans_ext(pp),
          width = 8,
          height = 10,
          dpi = config$dpi
        )
        exported_plots <- c(exported_plots, stats::na.omit(unname(plot_paths)))
      }, error = function(e) cli::cli_warn("M0 PRISMA diagram export failed: {e$message}"))
    }
  }

  list(plots = exported_plots, tables = exported_tables, reports = exported_reports, files = exported_files)
}
