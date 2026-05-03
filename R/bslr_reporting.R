# ============================================================================
# bslr_reporting.R - Reports, scaffolds, plots, and exports for B-SLR
# ============================================================================

#' Build B-SLR checkpoint summaries
#' @keywords internal
bslr_build_checkpoints <- function(protocol_validation,
                                   m0_result,
                                   bibliometric_map,
                                   screening_ledger = NULL) {
  screening_summary <- m0_result$data$screening_summary %||% list()
  reliability <- screening_summary$reliability %||% list()
  screening_present <- !is.null(screening_ledger) &&
    ((is.data.frame(screening_ledger) && nrow(screening_ledger) > 0) ||
       (is.character(screening_ledger) && length(screening_ledger) == 1 && file.exists(screening_ledger)))
  screening_present <- screening_present || isTRUE(screening_summary$ok)

  m0_docs <- nrow(m0_get_bib_data(m0_result) %||% data.frame())
  raw_records <- sum((m0_result$data$source_summary$n_records %||% numeric(0)), na.rm = TRUE)
  refined_records <- m0_docs

  cp1_items <- data.frame(
    checkpoint = "checkpoint_1_data_consolidation",
    item = c(
      "research_question_defined",
      "criteria_defined",
      "search_string_defined",
      "databases_defined",
      "raw_dataset_available",
      "refined_dataset_available",
      "manual_screening_recorded",
      "reviewer_agreement_available"
    ),
    status = c(
      !("research_question" %in% protocol_validation$missing_required),
      !any(c("inclusion_criteria", "exclusion_criteria") %in% protocol_validation$missing_required),
      !("primary_query" %in% protocol_validation$missing_required),
      !("primary_database" %in% protocol_validation$missing_required),
      raw_records > 0,
      refined_records > 0,
      screening_present,
      identical(reliability$status %||% "", "success")
    ),
    stringsAsFactors = FALSE
  )

  cp2_items <- data.frame(
    checkpoint = "checkpoint_2_preliminary_results",
    item = c(
      "bibliometric_approach_executed",
      "clusters_identified",
      "sample_ranking_available",
      "sample_selection_available"
    ),
    status = c(
      identical(bibliometric_map$status, "success"),
      (bibliometric_map$metrics$n_clusters %||% 0) > 0,
      is.data.frame(bibliometric_map$sample_ranking) && nrow(bibliometric_map$sample_ranking) > 0,
      is.data.frame(bibliometric_map$sample_selected) && nrow(bibliometric_map$sample_selected) > 0
    ),
    stringsAsFactors = FALSE
  )

  cp3_items <- data.frame(
    checkpoint = "checkpoint_3_contribution_assessment",
    item = c(
      "cluster_review_scaffold",
      "holistic_review_scaffold",
      "theorising_scaffold"
    ),
    status = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  items <- rbind(cp1_items, cp2_items, cp3_items)
  summary <- items |>
    dplyr::group_by(checkpoint) |>
    dplyr::summarise(
      completed = sum(status, na.rm = TRUE),
      total = dplyr::n(),
      share = safe_divide(completed, total, default = 0),
      state = ifelse(completed == total, "complete", ifelse(completed == 0, "not_started", "partial")),
      .groups = "drop"
    )

  list(items = items, summary = as.data.frame(summary, stringsAsFactors = FALSE))
}

bslr_build_journal_grade_assessment <- function(protocol_validation,
                                                m0_result,
                                                bibliometric_map,
                                                checkpoints,
                                                scaffolds = list()) {
  screening_summary <- m0_result$data$screening_summary %||% list()
  reliability <- screening_summary$reliability %||% list()
  checkpoint_summary <- checkpoints$summary %||% data.frame()
  sample_selected <- bibliometric_map$sample_selected %||% data.frame()
  extraction_template <- scaffolds$extraction_template %||% data.frame()

  gate_rows <- data.frame(
    gate = c(
      "protocol_required_fields",
      "human_review_metadata",
      "journal_grade_protocol_enrichment",
      "screening_ledger_available",
      "reviewer_agreement_statistics",
      "prisma_full_review_mode",
      "bibliometric_clusters_available",
      "sample_selection_available",
      "extraction_template_available",
      "checkpoint_1_complete",
      "checkpoint_2_complete",
      "checkpoint_3_complete"
    ),
    status = c(
      isTRUE(protocol_validation$ok),
      isTRUE(protocol_validation$human_gate_ready),
      isTRUE(protocol_validation$journal_ready),
      isTRUE(screening_summary$ok),
      identical(reliability$status %||% "", "success"),
      isTRUE(screening_summary$ok),
      (bibliometric_map$metrics$n_clusters %||% 0) > 0,
      is.data.frame(sample_selected) && nrow(sample_selected) > 0,
      is.data.frame(extraction_template) && nrow(extraction_template) > 0,
      any(checkpoint_summary$checkpoint == "checkpoint_1_data_consolidation" & checkpoint_summary$state == "complete"),
      any(checkpoint_summary$checkpoint == "checkpoint_2_preliminary_results" & checkpoint_summary$state == "complete"),
      any(checkpoint_summary$checkpoint == "checkpoint_3_contribution_assessment" & checkpoint_summary$state == "complete")
    ),
    evidence = c(
      if (length(protocol_validation$missing_required %||% character()) == 0) "All core protocol fields are present." else paste("Missing:", paste(protocol_validation$missing_required, collapse = ", ")),
      if (length(protocol_validation$missing_human_review %||% character()) == 0) "Reviewer metadata are complete." else paste("Missing:", paste(protocol_validation$missing_human_review, collapse = ", ")),
      if (length(protocol_validation$missing_journal_grade %||% character()) == 0) "All journal-grade enrichment fields are present." else paste("Missing:", paste(protocol_validation$missing_journal_grade, collapse = ", ")),
      if (isTRUE(screening_summary$ok)) paste("Documents with decisions:", screening_summary$n_documents %||% 0) else "No usable screening ledger was supplied.",
      if (identical(reliability$status %||% "", "success")) paste("Krippendorff's alpha =", round(reliability$krippendorffs_alpha %||% NA_real_, 3)) else "Inter-rater reliability was not available.",
      if (isTRUE(screening_summary$ok)) "PRISMA can be derived from explicit screening decisions." else "PRISMA remains counts-only.",
      paste("Clusters identified:", bibliometric_map$metrics$n_clusters %||% 0),
      paste("Selected documents:", if (is.data.frame(sample_selected)) nrow(sample_selected) else 0),
      paste("Rows queued for coding:", if (is.data.frame(extraction_template)) nrow(extraction_template) else 0),
      bslr_checkpoint_gate_evidence(checkpoint_summary, "checkpoint_1_data_consolidation"),
      bslr_checkpoint_gate_evidence(checkpoint_summary, "checkpoint_2_preliminary_results"),
      bslr_checkpoint_gate_evidence(checkpoint_summary, "checkpoint_3_contribution_assessment")
    ),
    action = c(
      "Complete the minimal protocol fields before claiming B-SLR compliance.",
      "Add reviewers, conflict rule, screening tool, and quality appraisal metadata.",
      "Fill the enrichment fields that support journal-grade Methods writing.",
      "Provide a reviewer-level screening ledger with stage, decision, reason, and dates.",
      "Ensure at least two reviewers overlap on decisions so agreement coefficients can be computed.",
      "Re-run M0 with the completed screening ledger to promote PRISMA from counts-only to full-review.",
      "Adjust cluster parameters or approach if the bibliometric map is not meaningful.",
      "Review the ranked sample and confirm it is suitable for qualitative coding.",
      "Complete the extraction template article by article before synthesis.",
      "Close all protocol and dataset consolidation gaps before reporting Methods.",
      "Inspect cluster quality and sample representativity before thematic reading.",
      "Translate the scaffold into explicit synthesis claims and theoretical contribution."
    ),
    stringsAsFactors = FALSE
  )

  complete_n <- sum(gate_rows$status, na.rm = TRUE)
  total_n <- nrow(gate_rows)
  readiness_score <- safe_divide(complete_n, total_n, default = 0)
  methodological_mode <- if (isTRUE(screening_summary$ok)) "full-review" else "counts-only"
  readiness_band <- if (!isTRUE(protocol_validation$ok)) {
    "protocol_incomplete"
  } else if (!isTRUE(screening_summary$ok)) {
    "counts_only"
  } else if (!isTRUE(protocol_validation$human_gate_ready)) {
    "full_review_human_metadata_incomplete"
  } else if (!isTRUE(protocol_validation$journal_ready)) {
    "full_review_pending_journal_enrichment"
  } else if (all(checkpoint_summary$state == "complete")) {
    "journal_grade_ready"
  } else {
    "analysis_ready_pending_synthesis"
  }

  pending_actions <- unique(c(
    protocol_validation$required_actions %||% character(),
    gate_rows$action[!gate_rows$status]
  ))

  list(
    summary = list(
      readiness_score = readiness_score,
      completed_gates = complete_n,
      total_gates = total_n,
      methodological_mode = methodological_mode,
      readiness_band = readiness_band,
      journal_grade_ready = identical(readiness_band, "journal_grade_ready")
    ),
    gates = gate_rows,
    pending_actions = pending_actions
  )
}

bslr_checkpoint_gate_evidence <- function(checkpoint_summary, checkpoint_id) {
  if (!is.data.frame(checkpoint_summary) || nrow(checkpoint_summary) == 0) {
    return("Checkpoint status unavailable.")
  }
  row <- checkpoint_summary[checkpoint_summary$checkpoint == checkpoint_id, , drop = FALSE]
  if (nrow(row) == 0) {
    return("Checkpoint status unavailable.")
  }
  paste0(row$completed[1], "/", row$total[1], " items complete (", row$state[1], ").")
}

#' Build SLR and theorising scaffolds
#' @keywords internal
bslr_build_slr_scaffolds <- function(protocol, bibliometric_map) {
  protocol <- bslr_read_protocol(protocol)
  cluster_summary <- bibliometric_map$cluster_summary %||% data.frame()
  sample_selected <- bibliometric_map$sample_selected %||% data.frame()

  holistic_questions <- protocol$slr$holistic_questions %||% protocol$objectives %||% character()
  if (length(holistic_questions) == 0) {
    holistic_questions <- c(protocol$research_question %||% "Holistic synthesis question")
  }

  holistic <- data.frame(
    question = as.character(holistic_questions),
    evidence_scope = "Use the full refined dataset and cluster-level triangulation.",
    synthesis_prompt = "Summarize the state of knowledge, tensions, and gaps with explicit support from representative documents.",
    output_slot = "holistic_analysis",
    stringsAsFactors = FALSE
  )

  cluster_rows <- if (is.data.frame(cluster_summary) && nrow(cluster_summary) > 0) {
    lapply(seq_len(nrow(cluster_summary)), function(i) {
      row <- cluster_summary[i, , drop = FALSE]
      sample_titles <- sample_selected$title[sample_selected$cluster_id == row$cluster_id]
      data.frame(
        cluster_id = row$cluster_id,
        cluster_label = row$cluster_label,
        article_count = row$article_count,
        representative_documents = paste(utils::head(sample_titles, 5), collapse = " | "),
        thematic_prompt = paste(
          "Review this cluster as a coherent theme. Explain its focus, internal debates, methods, datasets, and unresolved gaps."
        ),
        output_slot = "cluster_thematic_analysis",
        stringsAsFactors = FALSE
      )
    })
  } else {
    list(data.frame(
      cluster_id = integer(),
      cluster_label = character(),
      article_count = integer(),
      representative_documents = character(),
      thematic_prompt = character(),
      output_slot = character(),
      stringsAsFactors = FALSE
    ))
  }
  cluster_scaffold <- dplyr::bind_rows(cluster_rows)

  theory_modes <- protocol$theorising$modes %||% c("research_agenda", "taxonomy", "conceptual_framework")
  theory_scaffold <- data.frame(
    mode = theory_modes,
    prompt = c(
      research_agenda = "Extract future research questions, unresolved tensions, and high-value directions grounded in the clusters.",
      taxonomy = "Group the literature into defensible dimensions and categories with explicit inclusion logic.",
      conceptual_framework = "Propose constructs, relationships, and boundary conditions supported by cluster evidence.",
      metatheory = "Synthesize assumptions, epistemic stances, and explanatory mechanisms across clusters."
    )[theory_modes],
    expected_output = c(
      research_agenda = "priority_research_questions",
      taxonomy = "taxonomy_dimensions_and_categories",
      conceptual_framework = "construct_relationships_and_boundaries",
      metatheory = "meta_assumptions_and_explanatory_logic"
    )[theory_modes],
    stringsAsFactors = FALSE
  )

  extraction_fields <- protocol$data_extraction$fields %||%
    c("aim", "theory_lens", "method", "data_context", "key_findings", "limitations", "contribution")
  extraction_template <- bslr_build_extraction_template(sample_selected, extraction_fields)
  evidence_matrix <- bslr_build_evidence_matrix(cluster_summary, sample_selected)
  manuscript_outline <- bslr_build_manuscript_outline(protocol, cluster_summary)

  list(
    holistic = holistic,
    cluster = cluster_scaffold,
    theorising = theory_scaffold,
    extraction_template = extraction_template,
    evidence_matrix = evidence_matrix,
    manuscript_outline = manuscript_outline
  )
}

#' Build a B-SLR methods report
#' @keywords internal
bslr_build_methods_report <- function(protocol,
                                      protocol_validation,
                                      m0_result,
                                      bibliometric_map,
                                      checkpoints,
                                      journal_assessment = NULL) {
  protocol <- bslr_read_protocol(protocol)
  search_metadata <- bslr_protocol_to_search_metadata(protocol)
  source_summary <- m0_result$data$source_summary %||% data.frame()
  screening_summary <- m0_result$data$screening_summary %||% list()
  reliability <- screening_summary$reliability %||% list()
  journal_assessment <- journal_assessment %||% bslr_build_journal_grade_assessment(
    protocol_validation,
    m0_result,
    bibliometric_map,
    checkpoints
  )
  readiness <- journal_assessment$summary %||% list()

  raw_records <- sum((source_summary$n_records %||% numeric(0)), na.rm = TRUE)
  refined_records <- nrow(m0_get_bib_data(m0_result) %||% data.frame())
  primary_query <- protocol$search$queries$primary %||% "NA"
  secondary_dbs <- paste(protocol$search$secondary_databases %||% character(), collapse = ", ")
  doc_types <- paste(protocol$search$document_types %||% character(), collapse = ", ")
  modes <- paste(protocol$theorising$modes %||% character(), collapse = ", ")
  methodological_mode <- if (isTRUE(screening_summary$ok)) "full-review" else "counts-only"
  human_actions <- unique(c(
    protocol_validation$required_actions %||% character(),
    journal_assessment$pending_actions %||% character()
  ))

  lines <- c(
    "============================================================",
    "B-SLR Methodology Report",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    paste0("This study applies a Bibliometric-Systematic Literature Review workflow implemented in RBiblioSynth on top of M0-M3."),
    "",
    "Protocol",
    paste0("  Title: ", protocol$title %||% "NA"),
    paste0("  Review topic: ", search_metadata$review_topic %||% "NA"),
    paste0("  Research question: ", search_metadata$research_question %||% "NA"),
    if (!bslr_is_empty_field(protocol$research_gap)) paste0("  Research gap: ", protocol$research_gap) else NULL,
    "",
    "Inclusion / Exclusion",
    paste0("  Inclusion criteria: ", paste(protocol$inclusion_criteria %||% character(), collapse = " | ")),
    paste0("  Exclusion criteria: ", paste(protocol$exclusion_criteria %||% character(), collapse = " | ")),
    "",
    "Search strategy",
    paste0("  Primary database: ", protocol$search$primary_database %||% "NA"),
    paste0("  Secondary databases: ", if (nzchar(secondary_dbs)) secondary_dbs else "NA"),
    paste0("  Search string: ", primary_query),
    if (!bslr_is_empty_field(protocol$search$search_string_validation)) paste0("  Search string validation: ", protocol$search$search_string_validation) else NULL,
    if (!bslr_is_empty_field(protocol$search$database_rationale)) paste0("  Database rationale: ", protocol$search$database_rationale) else NULL,
    paste0("  Search date: ", protocol$search$first_run_date %||% "NA"),
    paste0("  Time span: ", protocol$search$time_span$start %||% "NA", " to ", protocol$search$time_span$end %||% "NA"),
    paste0("  Language: ", protocol$search$language %||% "NA"),
    paste0("  Document types: ", if (nzchar(doc_types)) doc_types else "NA"),
    "",
    "Data consolidation",
    paste0("  Raw records extracted: ", raw_records),
    paste0("  Refined records after M0 merge/dedup: ", refined_records),
    paste0("  PRISMA mode: ", methodological_mode),
    "",
    "Screening",
    paste0("  Reviewers: ", paste(protocol$screening$reviewers %||% character(), collapse = ", ")),
    paste0("  Conflict rule: ", protocol$screening$conflict_rule %||% "NA"),
    paste0("  Screening tool: ", protocol$screening$screening_tool %||% "NA"),
    paste0("  Quality tool: ", protocol$screening$quality_tool %||% "NA"),
    if (identical(reliability$status %||% "", "success")) paste0("  Mean Cohen's kappa: ", round(reliability$cohens_kappa_mean %||% NA_real_, 3)) else "  Mean Cohen's kappa: not available",
    if (identical(reliability$status %||% "", "success")) paste0("  Fleiss' kappa: ", round(reliability$fleiss_kappa %||% NA_real_, 3)) else "  Fleiss' kappa: not available",
    if (identical(reliability$status %||% "", "success")) paste0("  Krippendorff's alpha: ", round(reliability$krippendorffs_alpha %||% NA_real_, 3)) else "  Krippendorff's alpha: not available",
    paste0("  Target agreement threshold: ", protocol$screening$agreement_target %||% "NA"),
    "",
    "Bibliometric mapping",
    paste0("  Approach: ", bibliometric_map$approach %||% "NA"),
    paste0("  Minimum cluster size: ", protocol$bibliometric$min_cluster_size %||% "NA"),
    paste0("  Resolution parameter: ", protocol$bibliometric$resolution %||% "NA"),
    paste0("  Ranking rule: ", protocol$bibliometric$ranking_rule %||% "NA"),
    paste0("  Cluster labeling process: ", protocol$bibliometric$cluster_labeling_process %||% "NA"),
    paste0("  Sample selection criteria: ", protocol$bibliometric$sample_selection_criteria %||% "NA"),
    paste0("  Clusters identified: ", bibliometric_map$metrics$n_clusters %||% 0),
    paste0("  Sample top-n per cluster: ", protocol$bibliometric$sample_top_n %||% 5L),
    paste0("  Extraction fields: ", paste(protocol$data_extraction$fields %||% character(), collapse = ", ")),
    "",
    "Theorising",
    paste0("  Requested synthesis modes: ", if (nzchar(modes)) modes else "NA"),
    if (!bslr_is_empty_field(protocol$theorising$perimeter)) paste0("  Theorising perimeter: ", protocol$theorising$perimeter) else NULL,
    if (!bslr_is_empty_field(protocol$theorising$rationale)) paste0("  Rationale: ", protocol$theorising$rationale) else NULL,
    if (!bslr_is_empty_field(protocol$theorising$contribution_goal)) paste0("  Contribution goal: ", protocol$theorising$contribution_goal) else NULL,
    "",
    "Methodological maturity",
    if (identical(methodological_mode, "full-review")) {
      "  Full-review mode is active: screening decisions and human-review metadata were available for methodological reporting."
    } else {
      "  Counts-only mode is active: the bibliometric pipeline ran successfully, but a journal-grade B-SLR still requires a completed screening ledger and full human-review metadata."
    },
    paste0(
      "  Journal-grade readiness: ",
      readiness$readiness_band %||% "unknown",
      " (",
      round(100 * (readiness$readiness_score %||% 0), 1),
      "% of gates complete)."
    ),
    "",
    "B-SLR step alignment",
    "  Step 1: topic choice, gap definition, and research question framing were captured in the protocol.",
    "  Step 2: the validated search string was recorded in the search strategy.",
    "  Step 3: primary and secondary databases were declared before extraction.",
    "  Step 4: screening and cross-checking rules were recorded together with reviewer metadata.",
    "  Step 5: M0 produced the refined bibliometric dataset after merge, harmonization, and deduplication.",
    "  Step 6: the bibliometric approach and network parameters were declared before mapping.",
    "  Step 7: clusters were derived from the internal bibliometric engine and labeled for review.",
    "  Step 8: a ranked sample per cluster was generated to support focused reading and coding.",
    "  Step 9: holistic and cluster-specific SLR scaffolds were generated for synthesis.",
    "  Step 10: theorising scaffolds were generated for research agenda, taxonomy, and conceptual framing.",
    "",
    "Checkpoint status"
  )

  if (is.data.frame(checkpoints$summary) && nrow(checkpoints$summary) > 0) {
    for (i in seq_len(nrow(checkpoints$summary))) {
      row <- checkpoints$summary[i, , drop = FALSE]
      lines <- c(lines, paste0("  - ", row$checkpoint, ": ", row$state, " (", row$completed, "/", row$total, ")"))
    }
  }

  if (length(human_actions) > 0) {
    lines <- c(lines, "", "Human actions required", paste0("  - ", human_actions))
  }

  tex <- c(
    "\\section*{B-SLR Methodology}",
    paste0("This study applies a Bibliometric-Systematic Literature Review workflow implemented in RBiblioSynth. "),
    paste0("The review topic was ", m0_escape_latex(search_metadata$review_topic %||% "NA"),
           " and the research question was ", m0_escape_latex(search_metadata$research_question %||% "NA"), ". "),
    paste0("The primary database was ", m0_escape_latex(protocol$search$primary_database %||% "NA"),
           " using the query ", m0_escape_latex(primary_query), ". "),
    paste0("The search covered ", m0_escape_latex(protocol$search$time_span$start %||% "NA"),
           " to ", m0_escape_latex(protocol$search$time_span$end %||% "NA"),
           ", restricted to ", m0_escape_latex(protocol$search$language %||% "NA"),
           " and document types ", m0_escape_latex(doc_types), ". "),
    paste0("M0 consolidated ", raw_records, " raw records into ", refined_records, " refined records. "),
    paste0("The workflow remained in ", m0_escape_latex(methodological_mode),
           " mode", if (identical(methodological_mode, "counts-only")) {
             ", meaning that the PRISMA narrative remains partial until a completed screening ledger and reviewer agreement statistics are supplied. "
           } else {
             ". "
           }),
    paste0("Screening relied on ", m0_escape_latex(protocol$screening$screening_tool %||% "NA"),
           " with conflict resolution by ", m0_escape_latex(protocol$screening$conflict_rule %||% "NA"), ". "),
    paste0("Inter-rater reliability was summarised with Cohen's kappa, Fleiss' kappa, and Krippendorff's alpha when available. "),
    paste0("Bibliometric mapping used ", m0_escape_latex(bibliometric_map$approach %||% "NA"),
           " and identified ", bibliometric_map$metrics$n_clusters %||% 0, " clusters. "),
    paste0("Theorising scaffolds were prepared for: ", m0_escape_latex(modes), ". "),
    paste0("Journal-grade readiness was assessed as ", m0_escape_latex(readiness$readiness_band %||% "unknown"),
           " with ", round(100 * (readiness$readiness_score %||% 0), 1), "\\% of methodological gates completed.")
  )

  list(lines = lines, tex = tex)
}

bslr_build_methods_template_report <- function(protocol,
                                               protocol_validation,
                                               m0_result,
                                               bibliometric_map,
                                               journal_assessment) {
  protocol <- bslr_read_protocol(protocol)
  search_metadata <- bslr_protocol_to_search_metadata(protocol)
  source_summary <- m0_result$data$source_summary %||% data.frame()
  screening_summary <- m0_result$data$screening_summary %||% list()
  reliability <- screening_summary$reliability %||% list()
  readiness <- journal_assessment$summary %||% list()

  raw_records <- sum((source_summary$n_records %||% numeric(0)), na.rm = TRUE)
  refined_records <- nrow(m0_get_bib_data(m0_result) %||% data.frame())
  databases <- paste(c(
    protocol$search$primary_database %||% character(),
    protocol$search$secondary_databases %||% character()
  ), collapse = ", ")
  doc_types <- paste(protocol$search$document_types %||% character(), collapse = ", ")
  modes <- paste(protocol$theorising$modes %||% character(), collapse = ", ")

  paragraph_1 <- paste0(
    "This study applies a Bibliometric-Systematic Literature Review (B-SLR) workflow implemented in RBiblioSynth to examine ",
    search_metadata$review_topic %||% "the focal topic",
    ". The guiding research question was: ",
    search_metadata$research_question %||% "NA",
    ". The conceptual perimeter was defined ex ante through the following inclusion criteria: ",
    paste(protocol$inclusion_criteria %||% character(), collapse = "; "),
    ". Exclusion criteria were defined as: ",
    paste(protocol$exclusion_criteria %||% character(), collapse = "; "),
    "."
  )

  paragraph_2 <- paste0(
    "The search strategy was executed in ",
    protocol$search$primary_database %||% "NA",
    if (nzchar(paste(protocol$search$secondary_databases %||% character(), collapse = ", "))) {
      paste0(", with cross-checking against ", paste(protocol$search$secondary_databases %||% character(), collapse = ", "))
    } else {
      ""
    },
    ", using the query string \"",
    protocol$search$queries$primary %||% "NA",
    "\". The search covered ",
    protocol$search$time_span$start %||% "NA",
    " to ",
    protocol$search$time_span$end %||% "NA",
    ", limited to ",
    protocol$search$language %||% "NA",
    " and document types ",
    doc_types,
    ". M0 consolidated ",
    raw_records,
    " raw records into ",
    refined_records,
    " refined records after harmonization and deduplication."
  )

  paragraph_3 <- if (isTRUE(screening_summary$ok)) {
    paste0(
      "Manual screening decisions were recorded for ",
      screening_summary$n_documents %||% 0,
      " documents using ",
      protocol$screening$screening_tool %||% "NA",
      ", with conflict resolution by ",
      protocol$screening$conflict_rule %||% "NA",
      ". Reviewer agreement was assessed with Cohen's kappa, Fleiss' kappa, and Krippendorff's alpha",
      if (identical(reliability$status %||% "", "success")) {
        paste0(
          " (Krippendorff's alpha = ",
          round(reliability$krippendorffs_alpha %||% NA_real_, 3),
          ")."
        )
      } else {
        "."
      }
    )
  } else {
    "A completed reviewer-level screening ledger was not available; therefore, the workflow remains in counts-only mode and the PRISMA narrative should be reported as partial until manual screening decisions are supplied."
  }

  paragraph_4 <- paste0(
    "Bibliometric mapping relied on the internal RBiblioSynth engine using ",
    bibliometric_map$approach %||% "NA",
    " with minimum cluster size ",
    protocol$bibliometric$min_cluster_size %||% "NA",
    ", resolution ",
    protocol$bibliometric$resolution %||% "NA",
    ", and ranking rule ",
    protocol$bibliometric$ranking_rule %||% "NA",
    ". This produced ",
    bibliometric_map$metrics$n_clusters %||% 0,
    " clusters and a ranked within-cluster sample for focused reading. The selected documents should then be coded with the extraction fields defined in the protocol, and the synthesis should be developed through ",
    modes,
    "."
  )

  lines <- c(
    "============================================================",
    "B-SLR Journal Methods Template",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    "Copy-ready paragraphs",
    paragraph_1,
    "",
    paragraph_2,
    "",
    paragraph_3,
    "",
    paragraph_4,
    "",
    paste0("Readiness band: ", readiness$readiness_band %||% "unknown"),
    paste0("Readiness score: ", round(100 * (readiness$readiness_score %||% 0), 1), "%")
  )

  tex <- c(
    "\\section*{B-SLR Methods Template}",
    m0_escape_latex(paragraph_1),
    "",
    m0_escape_latex(paragraph_2),
    "",
    m0_escape_latex(paragraph_3),
    "",
    m0_escape_latex(paragraph_4)
  )

  list(lines = lines, tex = tex)
}

#' Build an executive B-SLR report
#' @keywords internal
bslr_build_executive_report <- function(protocol, module_results, bibliometric_map, checkpoints, journal_assessment = NULL) {
  protocol <- bslr_read_protocol(protocol)
  m0_result <- module_results$m0
  m1_result <- module_results$m1 %||% list()
  m2_result <- module_results$m2 %||% list()
  m3_result <- module_results$m3 %||% list()
  screening_summary <- m0_result$data$screening_summary %||% list()
  journal_assessment <- journal_assessment %||% bslr_build_journal_grade_assessment(
    protocol_validation = list(ok = TRUE, missing_required = character(), missing_human_review = character(), missing_journal_grade = character(), human_gate_ready = TRUE, journal_ready = FALSE, required_actions = character()),
    m0_result = m0_result,
    bibliometric_map = bibliometric_map,
    checkpoints = checkpoints
  )
  readiness <- journal_assessment$summary %||% list()

  top_cluster <- if (is.data.frame(bibliometric_map$cluster_summary) && nrow(bibliometric_map$cluster_summary) > 0) {
    bibliometric_map$cluster_summary$cluster_label[1]
  } else {
    "NA"
  }

  lines <- c(
    "============================================================",
    "B-SLR Executive Report",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    paste0("Title: ", protocol$title %||% "NA"),
    paste0("Research question: ", protocol$research_question %||% "NA"),
    paste0("Refined dataset size: ", nrow(m0_get_bib_data(m0_result) %||% data.frame())),
    paste0("Methodological mode: ", if (isTRUE(screening_summary$ok)) "full-review" else "counts-only"),
    paste0("Readiness band: ", readiness$readiness_band %||% "unknown"),
    paste0("Readiness score: ", round(100 * (readiness$readiness_score %||% 0), 1), "%"),
    paste0("Bibliometric approach: ", bibliometric_map$approach %||% "NA"),
    paste0("Clusters identified: ", bibliometric_map$metrics$n_clusters %||% 0),
    paste0("Largest cluster theme: ", top_cluster),
    ""
  )

  if (inherits(m1_result, "biblio_module_result")) {
    lines <- c(lines, paste0("M1 status: ", m1_result$status))
  }
  if (inherits(m2_result, "biblio_module_result")) {
    lines <- c(lines, paste0("M2 status: ", m2_result$status))
    if (!is.null(m2_result$data$regression$best_model$name)) {
      lines <- c(lines, paste0("  M2 headline model: ", m2_result$data$regression$best_model$name))
    }
  }
  if (inherits(m3_result, "biblio_module_result")) {
    lines <- c(lines, paste0("M3 status: ", m3_result$status))
  }

  if (is.data.frame(bibliometric_map$sample_selected) && nrow(bibliometric_map$sample_selected) > 0) {
    lines <- c(lines, paste0("Representative documents prepared for SLR coding: ", nrow(bibliometric_map$sample_selected)))
  }

  if (is.data.frame(checkpoints$summary) && nrow(checkpoints$summary) > 0) {
    lines <- c(lines, "", "Checkpoint summary")
    for (i in seq_len(nrow(checkpoints$summary))) {
      row <- checkpoints$summary[i, , drop = FALSE]
      lines <- c(lines, paste0("  - ", row$checkpoint, ": ", row$state))
    }
  }

  if (!isTRUE(screening_summary$ok)) {
    lines <- c(
      lines,
      "",
      "Journal-grade actions pending",
      "  - Provide a completed screening ledger with reviewer-level decisions.",
      "  - Re-run the workflow to promote PRISMA and B-SLR reporting from counts-only to full-review mode."
    )
  }

  list(lines = lines, tex = c("\\section*{B-SLR Executive Summary}", m0_escape_latex(lines[-c(1:4)])))
}

#' Render core B-SLR plots
#' @keywords internal
bslr_render_plots <- function(bibliometric_map, checkpoints, config = biblio_config()) {
  plots <- list()

  cluster_summary <- bibliometric_map$cluster_summary %||% data.frame()
  sample_ranking <- bibliometric_map$sample_ranking %||% data.frame()

  if (is.data.frame(cluster_summary) && nrow(cluster_summary) > 0) {
    cluster_plot_df <- cluster_summary
    cluster_plot_df$cluster_tag <- paste0("C", cluster_plot_df$cluster_id)
    cluster_plot_df$cluster_label_short <- vapply(cluster_plot_df$cluster_label, bslr_compact_cluster_label, character(1))

    plots$cluster_sizes <- ggplot2::ggplot(cluster_plot_df, ggplot2::aes(x = reorder(cluster_label_short, article_count), y = article_count)) +
      ggplot2::geom_col(fill = "#1f4e79", width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Cluster Sizes",
        subtitle = "Documents retained in each bibliometric cluster",
        x = NULL,
        y = "Documents"
      ) +
      ieee_theme()
    plots$cluster_sizes <- ieee_mark_plot_layout(plots$cluster_sizes, "full")

    plots$cluster_impact <- ggplot2::ggplot(cluster_plot_df, ggplot2::aes(
      x = article_count,
      y = mean_citations,
      size = total_citations,
      label = cluster_tag
    )) +
      ggplot2::geom_point(color = "#0b6e4f", alpha = 0.8) +
      ggplot2::labs(
        title = "Cluster Size vs Impact",
        subtitle = "Average citation performance by bibliometric cluster (use the cluster summary table for full labels)",
        x = "Documents per cluster",
        y = "Mean citations",
        caption = paste(cluster_plot_df$cluster_tag, cluster_plot_df$cluster_label_short, sep = " = ", collapse = " | ")
      ) +
      ieee_theme()
    plots$cluster_impact <- ieee_mark_plot_layout(plots$cluster_impact, "full")
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      plots$cluster_impact <- plots$cluster_impact + ggrepel::geom_label_repel(size = 3, max.overlaps = 12, label.padding = grid::unit(0.1, "lines"))
    } else {
      plots$cluster_impact <- plots$cluster_impact + ggplot2::geom_label(vjust = -0.6, size = 3, label.padding = grid::unit(0.1, "lines"))
    }
  }

  if (is.data.frame(sample_ranking) && nrow(sample_ranking) > 0) {
    plots$sample_ranking <- ggplot2::ggplot(sample_ranking, ggplot2::aes(
      x = weighted_degree,
      y = age_normalized_citations,
      color = factor(cluster_id)
    )) +
      ggplot2::geom_point(alpha = 0.8, size = 2.2) +
      ggplot2::labs(
        title = "Document Representativity by Cluster",
        subtitle = "Centrality versus age-normalized citations",
        x = "Within-network weighted degree",
        y = "Age-normalized citations",
        color = "Cluster"
      ) +
      ieee_theme()
    plots$sample_ranking <- ieee_mark_plot_layout(plots$sample_ranking, "full")
  }

  if (is.data.frame(checkpoints$summary) && nrow(checkpoints$summary) > 0) {
    plots$checkpoints <- ggplot2::ggplot(checkpoints$summary, ggplot2::aes(x = checkpoint, y = share, fill = state)) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      ggplot2::labs(
        title = "B-SLR Checkpoint Completion",
        subtitle = "Coverage of required methodological milestones",
        x = NULL,
        y = "Completion",
        fill = "State"
      ) +
      ieee_theme()
    plots$checkpoints <- ieee_mark_plot_layout(plots$checkpoints, "single")
  }

  list(plots = plots)
}

#' Build B-SLR tables
#' @keywords internal
bslr_build_tables <- function(protocol_validation, checkpoints, bibliometric_map, scaffolds, journal_assessment = NULL) {
  journal_assessment <- journal_assessment %||% list(summary = list(), gates = data.frame(), pending_actions = character())
  list(
    protocol_validation = data.frame(
      completeness = protocol_validation$completeness %||% 0,
      ok = protocol_validation$ok %||% FALSE,
      missing_required = paste(protocol_validation$missing_required %||% character(), collapse = "; "),
      missing_human_review = paste(protocol_validation$missing_human_review %||% character(), collapse = "; "),
      missing_journal_grade = paste(protocol_validation$missing_journal_grade %||% character(), collapse = "; "),
      maturity_band = protocol_validation$maturity_band %||% "unknown",
      journal_ready = protocol_validation$journal_ready %||% FALSE,
      stringsAsFactors = FALSE
    ),
    checkpoint_items = checkpoints$items %||% data.frame(),
    checkpoint_summary = checkpoints$summary %||% data.frame(),
    cluster_summary = bibliometric_map$cluster_summary %||% data.frame(),
    sample_ranking = bibliometric_map$sample_ranking %||% data.frame(),
    sample_selected = bibliometric_map$sample_selected %||% data.frame(),
    slr_holistic = scaffolds$holistic %||% data.frame(),
    slr_cluster = scaffolds$cluster %||% data.frame(),
    theorising = scaffolds$theorising %||% data.frame(),
    extraction_template = scaffolds$extraction_template %||% data.frame(),
    evidence_matrix = scaffolds$evidence_matrix %||% data.frame(),
    manuscript_outline = scaffolds$manuscript_outline %||% data.frame(),
    reporting_checklist = bslr_build_reporting_checklist(protocol_validation, checkpoints, bibliometric_map, journal_assessment),
    journal_grade_gates = journal_assessment$gates %||% data.frame(),
    human_action_plan = data.frame(
      action = journal_assessment$pending_actions %||% character(),
      stringsAsFactors = FALSE
    )
  )
}

bslr_build_extraction_template <- function(sample_selected, extraction_fields) {
  if (!is.data.frame(sample_selected) || nrow(sample_selected) == 0) {
    return(data.frame())
  }

  base <- sample_selected[, intersect(
    c("doc_key", "cluster_id", "cluster_label", "title", "year", "doi"),
    names(sample_selected)
  ), drop = FALSE]
  if (!"title" %in% names(base)) {
    base$title <- paste("Document", seq_len(nrow(base)))
  }

  for (field in extraction_fields) {
    base[[field]] <- NA_character_
  }
  base$extraction_status <- "pending"
  base$coder <- NA_character_
  base$notes <- NA_character_
  base
}

bslr_build_evidence_matrix <- function(cluster_summary, sample_selected) {
  if (!is.data.frame(cluster_summary) || nrow(cluster_summary) == 0) {
    return(data.frame())
  }

  rows <- lapply(seq_len(nrow(cluster_summary)), function(i) {
    row <- cluster_summary[i, , drop = FALSE]
    sample_docs <- if (is.data.frame(sample_selected)) {
      sample_selected$title[sample_selected$cluster_id == row$cluster_id]
    } else {
      character()
    }
    data.frame(
      cluster_id = row$cluster_id,
      cluster_label = row$cluster_label,
      article_count = row$article_count,
      mean_citations = row$mean_citations,
      representative_documents = paste(utils::head(sample_docs, 5), collapse = " | "),
      synthesis_claim = "To be completed during the human-led thematic synthesis.",
      support_type = "cluster_evidence",
      confidence = "pending",
      gap_to_verify = "Specify the unresolved issue or contradiction during coding.",
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

bslr_build_manuscript_outline <- function(protocol, cluster_summary) {
  protocol <- bslr_read_protocol(protocol)
  themes <- if (is.data.frame(cluster_summary) && nrow(cluster_summary) > 0) {
    paste(cluster_summary$cluster_label, collapse = " | ")
  } else {
    "Clusters pending"
  }

  data.frame(
    section = c(
      "Methods",
      "Bibliometric Results",
      "Cluster-level SLR",
      "Discussion",
      "Theoretical Contribution"
    ),
    purpose = c(
      "Describe the B-SLR protocol, search strategy, screening, and data consolidation.",
      "Present descriptive and network-level findings grounded in M1-M3 and bibliometric mapping.",
      "Synthesize each cluster with evidence from the selected sample.",
      "Integrate convergences, tensions, and boundary conditions across clusters.",
      "Translate the synthesis into a research agenda, taxonomy, or conceptual framework."
    ),
    anchor = c(
      protocol$research_question %||% "NA",
      protocol$bibliometric$approach %||% "NA",
      themes,
      protocol$theorising$perimeter %||% "NA",
      paste(protocol$theorising$modes %||% character(), collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
}

bslr_compact_cluster_label <- function(label, width = 34) {
  label <- trimws(as.character(label %||% ""))
  if (!nzchar(label)) {
    return("Unlabeled cluster")
  }
  if (nchar(label) <= width) {
    return(label)
  }
  paste0(substr(label, 1, max(1, width - 1)), "\u2026")
}

bslr_build_reporting_checklist <- function(protocol_validation, checkpoints, bibliometric_map, journal_assessment = NULL) {
  journal_assessment <- journal_assessment %||% list(summary = list(), gates = data.frame())
  gates <- journal_assessment$gates %||% data.frame()
  gate_ok <- function(gate_id) {
    isTRUE(any(gates$gate == gate_id & gates$status))
  }
  data.frame(
    item = c(
      "Research question",
      "Inclusion/exclusion criteria",
      "Search string",
      "Primary database",
      "Cross-check database",
      "Time span",
      "Raw dataset size",
      "Refined dataset size",
      "Screening ledger",
      "Reviewer agreement",
      "Bibliometric approach",
      "Cluster count",
      "Sample ranking rule",
      "Cluster labeling process",
      "Theorising rationale"
    ),
    status = c(
      !"research_question" %in% (protocol_validation$missing_required %||% character()),
      !any(c("inclusion_criteria", "exclusion_criteria") %in% (protocol_validation$missing_required %||% character())),
      !"primary_query" %in% (protocol_validation$missing_required %||% character()),
      !"primary_database" %in% (protocol_validation$missing_required %||% character()),
      !"secondary_databases" %in% (protocol_validation$missing_journal_grade %||% character()),
      !any(c("time_span_start", "time_span_end") %in% (protocol_validation$missing_required %||% character())),
      TRUE,
      TRUE,
      gate_ok("screening_ledger_available"),
      gate_ok("reviewer_agreement_statistics"),
      !"bibliometric_approach" %in% (protocol_validation$missing_required %||% character()),
      (bibliometric_map$metrics$n_clusters %||% 0) > 0,
      TRUE,
      !"cluster_labeling_process" %in% (protocol_validation$missing_journal_grade %||% character()),
      !"theorising_rationale" %in% (protocol_validation$missing_journal_grade %||% character())
    ),
    notes = c(
      "Explicit guiding question is required for journal-grade reporting.",
      "Conceptual perimeter must be declared before screening.",
      "Full string should be pasted verbatim in the Methods section.",
      "Declare the main extraction source.",
      "Declare the secondary source used for cross-checking.",
      "Report start and end coverage years.",
      "Report number of records before manual screening.",
      "Report number of retained records after screening.",
      "A reviewer-level decision ledger is needed for full-review mode.",
      "Agreement coefficients should be reported when screening is performed independently.",
      "State the internal engine used instead of external mapping software.",
      "Report the number of clusters retained for thematic reading.",
      "Explain how documents were ranked within cluster.",
      "Explain how thematic cluster labels were assigned and validated.",
      "State how the theoretical contribution will be justified."
    ),
    stringsAsFactors = FALSE
  )
}

bslr_build_manuscript_scaffold_report <- function(protocol, bibliometric_map, scaffolds, module_results = list(), journal_assessment = NULL) {
  protocol <- bslr_read_protocol(protocol)
  outline <- scaffolds$manuscript_outline %||% data.frame()
  extraction_template <- scaffolds$extraction_template %||% data.frame()
  journal_assessment <- journal_assessment %||% list(summary = list(), pending_actions = character())
  readiness <- journal_assessment$summary %||% list()
  m1_result <- module_results$m1 %||% list()
  m2_result <- module_results$m2 %||% list()
  m3_result <- module_results$m3 %||% list()

  lines <- c(
    "============================================================",
    "B-SLR Manuscript Scaffold",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    paste0("Target topic: ", protocol$review_topic %||% "NA"),
    paste0("Research question: ", protocol$research_question %||% "NA"),
    paste0("Bibliometric approach: ", bibliometric_map$approach %||% "NA"),
    paste0("Clusters retained: ", bibliometric_map$metrics$n_clusters %||% 0),
    paste0("Representative documents queued for coding: ", nrow(extraction_template)),
    paste0("Readiness band: ", readiness$readiness_band %||% "unknown"),
    ""
  )

  if (is.data.frame(outline) && nrow(outline) > 0) {
    lines <- c(lines, "Suggested manuscript blocks")
    for (i in seq_len(nrow(outline))) {
      row <- outline[i, , drop = FALSE]
      lines <- c(
        lines,
        paste0("  - ", row$section, ": ", row$purpose),
        paste0("    Anchor: ", row$anchor)
      )
    }
  }

  lines <- c(
    lines,
    "",
    "Automatic results blocks",
    paste0("  - M1 descriptive layer status: ", m1_result$status %||% "not_run"),
    paste0("  - M2 temporal layer status: ", m2_result$status %||% "not_run"),
    paste0("  - M3 comparative geography layer status: ", m3_result$status %||% "not_run"),
    "  - Use M1 for field structure, terms, sources, authors, and citations.",
    "  - Use M2 for growth regimes, breakpoints, saturation, and forecasting.",
    "  - Use M3 for countries, collaboration, inequality, quadrants, and emergence.",
    "",
    "Human completion items",
    "  - Complete the extraction template field-by-field for the selected documents.",
    "  - Validate cluster labels against the full text before writing the thematic synthesis.",
    "  - Turn the theorising scaffold into explicit propositions, categories, or framework relations.",
    if (length(journal_assessment$pending_actions %||% character()) > 0) paste0("  - Pending action: ", journal_assessment$pending_actions) else NULL
  )

  tex <- c(
    "\\section*{B-SLR Manuscript Scaffold}",
    paste0("Review topic: ", m0_escape_latex(protocol$review_topic %||% "NA"), "\\\\"),
    paste0("Research question: ", m0_escape_latex(protocol$research_question %||% "NA"), "\\\\"),
    paste0("Bibliometric approach: ", m0_escape_latex(bibliometric_map$approach %||% "NA"), "\\\\"),
    paste0("Representative documents queued for coding: ", nrow(extraction_template), "\\\\")
  )

  list(lines = lines, tex = tex)
}

bslr_build_journal_grade_report <- function(protocol,
                                            module_results,
                                            bibliometric_map,
                                            journal_assessment,
                                            scaffolds = list()) {
  protocol <- bslr_read_protocol(protocol)
  readiness <- journal_assessment$summary %||% list()
  gates <- journal_assessment$gates %||% data.frame()
  pending_actions <- journal_assessment$pending_actions %||% character()
  extraction_template <- scaffolds$extraction_template %||% data.frame()
  evidence_matrix <- scaffolds$evidence_matrix %||% data.frame()
  screening_summary <- module_results$m0$data$screening_summary %||% list()

  lines <- c(
    "============================================================",
    "B-SLR Journal-Grade Readiness",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    paste0("Title: ", protocol$title %||% "NA"),
    paste0("Readiness band: ", readiness$readiness_band %||% "unknown"),
    paste0("Readiness score: ", round(100 * (readiness$readiness_score %||% 0), 1), "%"),
    paste0("Methodological mode: ", readiness$methodological_mode %||% "unknown"),
    paste0("Clusters available: ", bibliometric_map$metrics$n_clusters %||% 0),
    paste0("Selected documents for coding: ", nrow(extraction_template)),
    paste0("Evidence-matrix rows: ", nrow(evidence_matrix)),
    paste0("Screening ledger status: ", if (isTRUE(screening_summary$ok)) "available" else "missing"),
    ""
  )

  if (is.data.frame(gates) && nrow(gates) > 0) {
    lines <- c(lines, "Gate assessment")
    for (i in seq_len(nrow(gates))) {
      row <- gates[i, , drop = FALSE]
      lines <- c(
        lines,
        paste0("  - ", row$gate, ": ", if (isTRUE(row$status)) "PASS" else "PENDING"),
        paste0("    Evidence: ", row$evidence),
        if (!isTRUE(row$status)) paste0("    Action: ", row$action) else "    Action: No action required."
      )
    }
  }

  if (length(pending_actions) > 0) {
    lines <- c(lines, "", "Priority human actions", paste0("  - ", pending_actions))
  }

  lines <- c(
    lines,
    "",
    "Interpretation",
    if (identical(readiness$readiness_band %||% "", "journal_grade_ready")) {
      "  The workflow has enough methodological structure to support a journal-grade B-SLR draft, subject to substantive human interpretation and writing."
    } else {
      "  The workflow can already generate strong bibliometric evidence and synthesis scaffolds, but the pending actions above must be closed before claiming a journal-grade B-SLR."
    }
  )

  tex <- c(
    "\\section*{B-SLR Journal-Grade Readiness}",
    paste0("Readiness band: ", m0_escape_latex(readiness$readiness_band %||% "unknown"), "\\\\"),
    paste0("Readiness score: ", round(100 * (readiness$readiness_score %||% 0), 1), "\\%\\\\"),
    paste0("Methodological mode: ", m0_escape_latex(readiness$methodological_mode %||% "unknown"), "\\\\")
  )

  list(lines = lines, tex = tex)
}

#' Export a B-SLR workflow result
#' @export
export_bslr <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots <- character()
  exported_tables <- character()
  exported_reports <- character()
  exported_files <- character()

  if (config$export_plots) {
    for (nm in names(result$artifacts$plots)) {
      plot_obj <- result$artifacts$plots[[nm]]
      if (is.null(plot_obj)) next
      plot_obj <- ieee_prepare_plot_for_export(plot_obj, module_id = "bslr", section_id = "workflow", plot_id = nm, config = config)
      spec <- ieee_get_plot_export_spec(plot_obj, config = config, section_id = "workflow", plot_id = nm)
      path <- build_artifact_path("bslr", "plots", paste0("bslr_", nm), "png", config)
      tryCatch({
        exported_paths <- export_plot_artifact(plot_obj, tools::file_path_sans_ext(path), width = spec$width, height = spec$height, dpi = spec$dpi)
        exported_plots <- c(exported_plots, unname(exported_paths[!is.na(exported_paths)]))
      }, error = function(e) cli::cli_warn("B-SLR plot export failed [{nm}]: {e$message}"))
    }
  }

  if (length(result$artifacts$tables) > 0) {
    for (nm in names(result$artifacts$tables)) {
      tab <- result$artifacts$tables[[nm]]
      if (!is.data.frame(tab)) next
      path <- build_artifact_path("bslr", "tables", paste0("bslr_", nm), "csv", config)
      utils::write.csv(tab, path, row.names = FALSE, na = "")
      exported_tables <- c(exported_tables, path)
    }
  }

  if (config$export_json) {
    json_payloads <- list(
      protocol = result$data$protocol,
      checkpoints = result$data$checkpoints,
      bibliometric_map = result$data$bibliometric_map,
      slr_scaffolds = result$data$slr_scaffolds,
      claim_ledger = result$data$claim_ledger,
      quality_gate = result$data$quality_gate,
      bibliometrix_parity = result$data$bibliometrix_parity,
      reproducibility = result$data$reproducibility
    )
    for (nm in names(json_payloads)) {
      path <- build_artifact_path("bslr", "json", paste0("bslr_", nm), "json", config)
      write_json_artifact(json_payloads[[nm]], path)
      exported_files <- c(exported_files, path)
    }
  }

  if (config$export_reports) {
    for (nm in names(result$artifacts$reports)) {
      report <- result$artifacts$reports[[nm]]
      if (!is.null(report$lines) && length(report$lines) > 0) {
        txt <- build_artifact_path("bslr", "reports", paste0("bslr_", nm), "txt", config)
        write_text_report(report$lines, txt)
        exported_reports <- c(exported_reports, txt)
      }
      if (!is.null(report$tex) && length(report$tex) > 0) {
        tex <- build_artifact_path("bslr", "reports", paste0("bslr_", nm), "tex", config)
        writeLines(report$tex, tex)
        exported_reports <- c(exported_reports, tex)
      }
    }
  }

  paper_bundle <- tryCatch(
    bslr_assemble_paper(
      bslr_result = result,
      config = config,
      render = identical(config$report_format, "quarto_html") ||
        identical(config$report_format, "quarto_pdf") ||
        length(config$paper_render_targets %||% character()) > 0
    ),
    error = function(e) {
      cli::cli_warn("B-SLR paper assembly failed: {e$message}")
      NULL
    }
  )
  if (is.list(paper_bundle) && length(paper_bundle$files %||% character()) > 0) {
    exported_reports <- c(exported_reports, paper_bundle$files)
  }
  if (is.list(paper_bundle) && length(paper_bundle$manifest_json %||% character()) > 0) {
    exported_files <- c(exported_files, paper_bundle$manifest_json)
  }

  list(
    plots = exported_plots,
    tables = exported_tables,
    reports = exported_reports,
    files = exported_files
  )
}

#' Build a B-SLR artifact manifest
#' @keywords internal
bslr_build_manifest <- function(result, exported, config = biblio_config()) {
  new_artifact_manifest(
    module_id = "bslr",
    generated_at = Sys.time(),
    files = exported$files,
    plots = exported$plots,
    tables = exported$tables,
    reports = exported$reports,
    status = result$status
  )
}
