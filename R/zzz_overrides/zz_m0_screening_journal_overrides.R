# ============================================================================
# zz_m0_screening_journal_overrides.R - Late-binding screening/full-review helpers
# ============================================================================

#' @export
m0_screening_ledger_template <- function(merged = NULL,
                                         reviewers = c("reviewer_1", "reviewer_2"),
                                         stages = c("screening", "eligibility")) {
  reviewers <- as.character(reviewers %||% character())
  reviewers <- reviewers[nzchar(trimws(reviewers))]
  if (length(reviewers) == 0) reviewers <- c("reviewer_1", "reviewer_2")

  stages <- as.character(stages %||% character())
  stages <- stages[nzchar(trimws(stages))]
  if (length(stages) == 0) stages <- c("screening", "eligibility")

  cols <- c(
    "M0_DOC_ID", "title", "doi", "year", "source",
    "stage", "reviewer", "decision", "reason",
    "notes", "decision_date", "is_final", "status"
  )

  if (is.null(merged) || !is.data.frame(merged) || nrow(merged) == 0) {
    out <- as.data.frame(matrix(ncol = length(cols), nrow = 0), stringsAsFactors = FALSE)
    names(out) <- cols
    return(out)
  }

  merged <- m0_org_prepare_input(merged)
  base <- data.frame(
    M0_DOC_ID = merged$M0_DOC_ID %||% seq_len(nrow(merged)),
    title = as.character(m0_org_get_column(merged, "TI")),
    doi = as.character(m0_org_get_column(merged, "DI")),
    year = suppressWarnings(as.integer(m0_org_get_column(merged, "PY"))),
    source = as.character(m0_org_get_column(merged, "SO")),
    stringsAsFactors = FALSE
  )

  template <- merge(
    base,
    expand.grid(
      stage = stages,
      reviewer = reviewers,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    by = NULL
  )
  template$decision <- NA_character_
  template$reason <- NA_character_
  template$notes <- NA_character_
  template$decision_date <- as.character(Sys.Date())
  template$is_final <- FALSE
  template$status <- "pending"

  template[, cols, drop = FALSE]
}

m0_build_screening_decision_matrix <- function(ledger) {
  if (!is.data.frame(ledger) || nrow(ledger) == 0) {
    return(data.frame())
  }

  compact <- ledger[, c("M0_DOC_ID", "stage", "reviewer", "decision"), drop = FALSE]
  compact$reviewer_stage <- paste(compact$stage, compact$reviewer, sep = "__")
  wide <- reshape(
    compact,
    idvar = "M0_DOC_ID",
    timevar = "reviewer_stage",
    direction = "wide"
  )
  names(wide) <- sub("^decision\\.", "", names(wide))
  wide[order(wide$M0_DOC_ID), , drop = FALSE]
}

m0_screening_pending_actions <- function(consensus, reliability) {
  actions <- character()
  if (!is.data.frame(consensus) || nrow(consensus) == 0) {
    return(c(
      "Provide explicit screening decisions before claiming full-review mode.",
      "Ensure at least two reviewers overlap on decisions so agreement can be quantified."
    ))
  }

  if (any(consensus$unresolved %||% FALSE, na.rm = TRUE)) {
    actions <- c(actions, "Resolve unresolved screening conflicts and mark the adjudicated decisions as final.")
  }
  if (!identical(reliability$status %||% "", "success")) {
    actions <- c(actions, "Capture overlapping reviewer decisions so inter-rater agreement coefficients can be computed.")
  }
  if (!any(consensus$stage == "eligibility")) {
    actions <- c(actions, "Add eligibility-stage decisions if full-text review has already been performed.")
  }

  unique(actions)
}

m0_build_screening_summary <- function(ledger, merged) {
  if (!is.data.frame(ledger) || nrow(ledger) == 0) {
    return(list(
      ok = FALSE,
      n_decisions = 0L,
      n_documents = 0L,
      stage_counts = data.frame(),
      exclusion_reasons = data.frame(),
      consensus = data.frame(),
      decision_matrix = data.frame(),
      reliability = list(status = "not_available"),
      methodological_mode = "counts-only",
      full_review_ready = FALSE,
      pending_actions = c(
        "Provide a reviewer-level screening ledger with stage, decision, reason, and decision date.",
        "Record overlapping decisions from at least two reviewers to compute agreement statistics."
      ),
      prisma = NULL
    ))
  }

  consensus <- m0_resolve_screening_consensus(ledger)
  decision_matrix <- m0_build_screening_decision_matrix(ledger)
  stage_counts <- consensus %>%
    dplyr::group_by(stage, consensus_decision) %>%
    dplyr::summarise(n_documents = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(stage, desc(n_documents))

  exclusion_reasons <- consensus %>%
    dplyr::filter(consensus_decision == "exclude", !is.na(reason), nzchar(reason)) %>%
    dplyr::group_by(stage, reason) %>%
    dplyr::summarise(n_documents = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(stage, desc(n_documents))

  reliability <- m0_build_screening_reliability(ledger)
  prisma <- m0_prisma_from_screening_consensus(consensus, merged, exclusion_reasons)
  unresolved_conflicts <- sum(consensus$unresolved %||% FALSE, na.rm = TRUE)
  full_review_ready <- identical(reliability$status %||% "", "success") && unresolved_conflicts == 0
  pending_actions <- m0_screening_pending_actions(consensus, reliability)

  list(
    ok = TRUE,
    n_decisions = nrow(ledger),
    n_documents = dplyr::n_distinct(ledger$M0_DOC_ID),
    stage_counts = stage_counts,
    exclusion_reasons = exclusion_reasons,
    consensus = consensus,
    decision_matrix = decision_matrix,
    reliability = reliability,
    unresolved_conflicts = unresolved_conflicts,
    methodological_mode = "full-review",
    full_review_ready = full_review_ready,
    pending_actions = pending_actions,
    prisma = prisma
  )
}

m0_build_screening_report <- function(screening_summary) {
  if (is.null(screening_summary) || !isTRUE(screening_summary$ok)) {
    return(list(lines = c(
      "No screening ledger available.",
      "Methodological mode: counts-only.",
      "Human action required: provide a reviewer-level screening ledger to activate full-review mode."
    )))
  }

  lines <- c(
    "============================================================",
    "Screening Ledger Summary",
    paste("Generated:", Sys.time()),
    "============================================================",
    paste("Methodological mode:", screening_summary$methodological_mode %||% "full-review"),
    paste("Full-review ready:", if (isTRUE(screening_summary$full_review_ready)) "yes" else "no"),
    paste("Total decisions:", screening_summary$n_decisions),
    paste("Documents with decisions:", screening_summary$n_documents),
    paste("Unresolved conflicts:", screening_summary$unresolved_conflicts %||% 0),
    ""
  )

  if (is.data.frame(screening_summary$stage_counts) && nrow(screening_summary$stage_counts) > 0) {
    lines <- c(lines, "Stage Counts")
    for (i in seq_len(nrow(screening_summary$stage_counts))) {
      row <- screening_summary$stage_counts[i, ]
      lines <- c(lines, paste0("  - ", row$stage, " / ", row$consensus_decision, ": ", row$n_documents))
    }
    lines <- c(lines, "")
  }

  if (is.data.frame(screening_summary$exclusion_reasons) && nrow(screening_summary$exclusion_reasons) > 0) {
    lines <- c(lines, "Exclusion Reasons")
    for (i in seq_len(nrow(screening_summary$exclusion_reasons))) {
      row <- screening_summary$exclusion_reasons[i, ]
      lines <- c(lines, paste0("  - ", row$stage, " / ", row$reason, ": ", row$n_documents))
    }
    lines <- c(lines, "")
  }

  reliability <- screening_summary$reliability %||% list()
  if (identical(reliability$status %||% "", "success")) {
    lines <- c(
      lines,
      "Reviewer Reliability",
      paste0("  Mean Cohen's kappa: ", round(reliability$cohens_kappa_mean %||% NA_real_, 3)),
      paste0("  Fleiss' kappa: ", round(reliability$fleiss_kappa %||% NA_real_, 3)),
      paste0("  Krippendorff's alpha: ", round(reliability$krippendorffs_alpha %||% NA_real_, 3)),
      paste0("  Percent agreement: ", round(100 * (reliability$percent_agreement %||% NA_real_), 1), "%"),
      paste0("  Interpretation: ", reliability$interpretation %||% "NA"),
      ""
    )
  } else {
    lines <- c(
      lines,
      "Reviewer Reliability",
      "  Inter-rater agreement is not available yet.",
      ""
    )
  }

  pending_actions <- screening_summary$pending_actions %||% character()
  if (length(pending_actions) > 0) {
    lines <- c(lines, "Pending Human Actions", paste0("  - ", pending_actions), "")
  }

  list(lines = lines)
}
