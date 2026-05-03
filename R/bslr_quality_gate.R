# ============================================================================
# bslr_quality_gate.R - Journal-readiness checklist for B-SLR outputs
# ============================================================================

#' Build a PRISMA/B-SLR quality gate
#'
#' @param protocol_validation Output from \code{validate_bslr_protocol()}.
#' @param m0_result M0 result.
#' @param module_results Named module result list.
#' @param journal_assessment Existing B-SLR journal assessment.
#' @param claim_ledger Optional claim ledger.
#' @param parity Optional bibliometrix parity bundle.
#' @param reproducibility Optional reproducibility capsule.
#' @return List with summary, checklist, blocking issues, and recommendation.
#' @export
bslr_build_quality_gate <- function(protocol_validation,
                                    m0_result,
                                    module_results = list(),
                                    journal_assessment = list(),
                                    claim_ledger = NULL,
                                    parity = NULL,
                                    reproducibility = NULL) {
  screening <- m0_result$data$screening_summary %||% list()
  prisma_validation <- m0_result$data$prisma_validation %||% list()
  protocol_validation <- protocol_validation %||% list()
  journal_assessment <- journal_assessment %||% list()
  claim_ledger <- claim_ledger %||% biblio_empty_claim_ledger()
  parity <- parity %||% list(status = "not_run")
  reproducibility <- reproducibility %||% list(status = "not_run")

  module_status <- vapply(module_results, function(x) x$status %||% "not_run", character(1))
  module_ok <- function(id) id %in% names(module_status) && module_status[[id]] %in% c("success", "warning")
  module_status_for <- function(id) if (id %in% names(module_status)) module_status[[id]] else "not_run"
  reliability <- screening$reliability %||% list()
  alpha <- suppressWarnings(as.numeric(reliability$krippendorffs_alpha %||% NA_real_))
  has_claims <- is.data.frame(claim_ledger) && nrow(claim_ledger) > 0
  strong_claims <- if (has_claims && "strength" %in% names(claim_ledger)) {
    sum(claim_ledger$strength %in% c("strong", "moderate"), na.rm = TRUE)
  } else {
    0L
  }

  rows <- list(
    bslr_quality_gate_row("protocol_core", "Protocol declares topic, question, query, databases, criteria, and time span.", isTRUE(protocol_validation$ok), "protocol_validation", paste(protocol_validation$missing_required %||% character(), collapse = "; "), "Complete missing core protocol fields."),
    bslr_quality_gate_row("protocol_human_review", "Protocol declares human reviewers, screening rule, and conflict-resolution plan.", length(protocol_validation$missing_human_review %||% character()) == 0, "protocol_validation", paste(protocol_validation$missing_human_review %||% character(), collapse = "; "), "Add reviewer roles, screening stages, and conflict adjudication rule."),
    bslr_quality_gate_row("journal_protocol_fields", "Protocol includes journal-grade B-SLR extras: cross-check databases, cluster labeling, sample ordering, and theorising rationale.", length(protocol_validation$missing_journal_grade %||% character()) == 0, "protocol_validation", paste(protocol_validation$missing_journal_grade %||% character(), collapse = "; "), "Fill the B-SLR journal-grade metadata fields."),
    bslr_quality_gate_row("screening_ledger", "Reviewer-level screening ledger exists with document, stage, reviewer, decision, reason, and dates.", isTRUE(screening$ok), "m0$data$screening_summary", paste(screening$pending_actions %||% character(), collapse = "; "), "Provide a real screening ledger, not only PRISMA counts."),
    bslr_quality_gate_row("conflict_resolution", "Screening conflicts are resolved or adjudicated.", isTRUE(screening$ok) && (screening$unresolved_conflicts %||% 1L) == 0, "m0$data$screening_summary$consensus", paste0("Unresolved conflicts: ", screening$unresolved_conflicts %||% NA), "Resolve conflicts and mark final decisions."),
    bslr_quality_gate_row("reviewer_agreement", "Inter-reviewer agreement is available and interpretable.", isTRUE(reliability$status == "success") && is.finite(alpha), "m0$data$screening_summary$reliability", paste0("Krippendorff alpha: ", ifelse(is.finite(alpha), round(alpha, 3), "NA")), "Record overlapping decisions from two or more reviewers."),
    bslr_quality_gate_row("prisma_full_review", "PRISMA is generated from screening ledger and search metadata when full-review claims are made.", identical(screening$methodological_mode %||% "", "full-review") && isTRUE(prisma_validation$ok %||% FALSE), "m0$data$prisma_validation", paste(prisma_validation$warnings %||% character(), collapse = "; "), "Use full-review mode with screening ledger and search metadata."),
    bslr_quality_gate_row("m0_success", "M0 produced a canonical refined data frame and organized downstream tables.", module_ok("m0"), "module_results$m0", module_status_for("m0"), "Fix M0 ingestion, merge, deduplication, or canonical columns."),
    bslr_quality_gate_row("m1_success", "M1 descriptive and semantic layers executed.", module_ok("m1"), "module_results$m1", module_status_for("m1"), "Run or repair M1."),
    bslr_quality_gate_row("m2_success", "M2 temporal, model-selection, forecasting, and changepoint layers executed.", module_ok("m2"), "module_results$m2", module_status_for("m2"), "Run or repair M2."),
    bslr_quality_gate_row("m3_success", "M3 geographic, collaboration, concentration, and trajectory layers executed.", module_ok("m3"), "module_results$m3", module_status_for("m3"), "Run or repair M3."),
    bslr_quality_gate_row("claim_ledger", "Automatic claims are traceable to evidence, tests, limitations, and strength.", has_claims, "claim_ledger", paste0("Claims: ", if (has_claims) nrow(claim_ledger) else 0), "Build claim ledger before treating the manuscript as auditable."),
    bslr_quality_gate_row("supported_claims", "At least one moderate/strong claim supports the analytical storyline.", strong_claims > 0, "claim_ledger$strength", paste0("Moderate/strong claims: ", strong_claims), "Do not overclaim; either add data/tests or state results as exploratory."),
    bslr_quality_gate_row("bibliometrix_parity", "Key outputs were compared with bibliometrix or an explicit skip reason was recorded.", parity$status %in% c("success", "warning", "skipped"), "bibliometrix_parity", parity$status %||% "not_run", "Run parity checks or document why bibliometrix is unavailable."),
    bslr_quality_gate_row("reproducibility_capsule", "Session info, config, data hashes, timestamps, and provenance were captured.", reproducibility$status %in% c("success", "warning"), "reproducibility_capsule", reproducibility$status %||% "not_run", "Build reproducibility capsule.")
  )

  checklist <- dplyr::bind_rows(rows)
  checklist$blocking <- !checklist$pass & checklist$gate_id %in% c(
    "protocol_core", "screening_ledger", "conflict_resolution", "m0_success",
    "claim_ledger", "reproducibility_capsule"
  )
  blocking_issues <- checklist$action[checklist$blocking]
  score <- mean(checklist$pass, na.rm = TRUE)
  blocking_count <- sum(checklist$blocking, na.rm = TRUE)
  recommendation <- dplyr::case_when(
    blocking_count > 0 ~ "not_journal_ready",
    score >= 0.90 ~ "journal_ready_with_human_synthesis",
    score >= 0.75 ~ "near_ready_close_methodological_gaps",
    TRUE ~ "research_prototype_not_submission_ready"
  )

  list(
    status = if (blocking_count == 0) "success" else "warning",
    summary = data.frame(
      readiness_score = score,
      gates_total = nrow(checklist),
      gates_passed = sum(checklist$pass, na.rm = TRUE),
      blocking_issues = blocking_count,
      recommendation = recommendation,
      stringsAsFactors = FALSE
    ),
    checklist = checklist,
    blocking_issues = blocking_issues,
    recommendation = recommendation
  )
}

bslr_quality_gate_row <- function(gate_id, requirement, pass, evidence_source, evidence, action) {
  data.frame(
    gate_id = gate_id,
    requirement = requirement,
    pass = isTRUE(pass),
    evidence_source = evidence_source,
    evidence = ifelse(is.na(evidence) | !nzchar(as.character(evidence)), "available", as.character(evidence)),
    action = if (isTRUE(pass)) "No action required." else action,
    stringsAsFactors = FALSE
  )
}

bslr_build_quality_gate_report <- function(quality_gate) {
  summary <- quality_gate$summary %||% data.frame()
  checklist <- quality_gate$checklist %||% data.frame()
  recommendation <- quality_gate$recommendation %||% "unknown"
  score <- if (is.data.frame(summary) && nrow(summary) > 0) round(100 * summary$readiness_score[1], 1) else NA_real_

  lines <- c(
    "============================================================",
    "B-SLR / PRISMA Quality Gate",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    paste0("Recommendation: ", recommendation),
    paste0("Readiness score: ", score, "%"),
    ""
  )

  if (is.data.frame(checklist) && nrow(checklist) > 0) {
    lines <- c(lines, "Checklist")
    for (i in seq_len(nrow(checklist))) {
      row <- checklist[i, , drop = FALSE]
      lines <- c(
        lines,
        paste0("  - ", row$gate_id, ": ", if (isTRUE(row$pass)) "PASS" else "PENDING"),
        paste0("    Evidence: ", row$evidence),
        paste0("    Action: ", row$action)
      )
    }
  }

  tex <- c(
    "\\section*{B-SLR / PRISMA Quality Gate}",
    paste0("Recommendation: ", m0_escape_latex(recommendation), "\\\\"),
    paste0("Readiness score: ", score, "\\%\\\\")
  )
  list(lines = lines, tex = tex)
}
