# ============================================================================
# m0_screening.R - Screening ledger and PRISMA decision support
# ============================================================================

#' Read and standardize a screening ledger
#'
#' @param screening_ledger A data frame, CSV/XLSX path, or list with decisions.
#' @param merged Merged M0 bibliographic data frame.
#' @return Standardized screening ledger.
#' @export
m0_read_screening_ledger <- function(screening_ledger, merged) {
  if (is.null(screening_ledger)) {
    return(tibble::tibble())
  }

  ledger <- screening_ledger
  if (is.character(screening_ledger) && length(screening_ledger) == 1 && file.exists(screening_ledger)) {
    ext <- tolower(tools::file_ext(screening_ledger))
    if (ext == "csv") {
      ledger <- withCallingHandlers(
        utils::read.csv(screening_ledger, stringsAsFactors = FALSE, encoding = "UTF-8"),
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("EOF within quoted string|number of items read is not a multiple|items le.dos no es m.ltiplo", msg, ignore.case = TRUE)) {
            invokeRestart("muffleWarning")
          }
        }
      )
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli::cli_abort("Package 'readxl' is required to read screening ledgers from Excel files.")
      }
      ledger <- as.data.frame(readxl::read_excel(screening_ledger), stringsAsFactors = FALSE)
    } else if (ext == "json") {
      ledger <- jsonlite::fromJSON(screening_ledger, simplifyDataFrame = TRUE)
    } else {
      cli::cli_abort("Unsupported screening ledger format: {ext}")
    }
  }

  if (is.list(ledger) && !is.data.frame(ledger)) {
    ledger <- dplyr::bind_rows(ledger)
  }
  if (!is.data.frame(ledger)) {
    cli::cli_abort("screening_ledger must resolve to a data frame.")
  }

  m0_standardize_screening_ledger(ledger, merged)
}

#' Standardize screening ledger columns and map to M0 documents
#' @keywords internal
m0_standardize_screening_ledger <- function(ledger, merged) {
  ledger <- data.frame(ledger, stringsAsFactors = FALSE)
  names(ledger) <- gsub("\\s+", "_", tolower(names(ledger)))

  alias_map <- c(
    record_id = "M0_DOC_ID",
    doc_id = "M0_DOC_ID",
    m0_doc_id = "M0_DOC_ID",
    doi = "doi",
    title = "title",
    stage = "stage",
    decision = "decision",
    final_decision = "decision",
    reviewer = "reviewer",
    reviewer_id = "reviewer",
    reason = "reason",
    exclusion_reason = "reason",
    notes = "notes",
    comment = "notes",
    decision_date = "decision_date",
    date = "decision_date",
    is_final = "is_final",
    final = "is_final",
    status = "status"
  )

  for (alias in names(alias_map)) {
    if (alias %in% names(ledger) && !alias_map[[alias]] %in% names(ledger)) {
      names(ledger)[names(ledger) == alias] <- alias_map[[alias]]
    }
  }

  required_any <- c("M0_DOC_ID", "doi", "title")
  if (!any(required_any %in% names(ledger))) {
    cli::cli_abort("screening_ledger must include one identifier column: M0_DOC_ID, doi, or title.")
  }

  if (!"stage" %in% names(ledger)) ledger$stage <- "screening"
  if (!"decision" %in% names(ledger)) ledger$decision <- "include"
  if (!"reviewer" %in% names(ledger)) ledger$reviewer <- "reviewer_1"
  if (!"reason" %in% names(ledger)) ledger$reason <- NA_character_
  if (!"notes" %in% names(ledger)) ledger$notes <- NA_character_
  if (!"decision_date" %in% names(ledger)) ledger$decision_date <- Sys.Date()
  if (!"is_final" %in% names(ledger)) ledger$is_final <- FALSE
  if (!"status" %in% names(ledger)) ledger$status <- "submitted"

  ledger$stage <- m0_normalize_stage(ledger$stage)
  ledger$decision <- m0_normalize_decision(ledger$decision)
  ledger$is_final <- as.logical(ledger$is_final)
  ledger$reviewer <- as.character(ledger$reviewer)
  ledger$reason <- as.character(ledger$reason)
  ledger$notes <- as.character(ledger$notes)
  ledger$status <- as.character(ledger$status)

  merged_lookup <- m0_prepare_screening_lookup(merged)
  ledger <- m0_map_screening_records(ledger, merged_lookup)
  ledger <- ledger[!is.na(ledger$M0_DOC_ID), , drop = FALSE]
  ledger$ledger_id <- seq_len(nrow(ledger))

  ledger %>%
    dplyr::arrange(M0_DOC_ID, stage, reviewer, decision_date, ledger_id)
}

#' Build screening ledger summary
#' @param ledger Standardized screening ledger.
#' @param merged M0 merged bibliographic data frame.
#' @return Summary list.
#' @export
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
    actions <- c(actions, "Add eligibility-stage decisions if full-text review has been performed.")
  }

  unique(actions)
}

#' Resolve per-document screening consensus
#' @keywords internal
m0_resolve_screening_consensus <- function(ledger) {
  if (!is.data.frame(ledger) || nrow(ledger) == 0) return(tibble::tibble())

  split_groups <- split(ledger, interaction(ledger$M0_DOC_ID, ledger$stage, drop = TRUE))
  rows <- lapply(split_groups, function(group) {
    final_rows <- group[group$is_final %in% TRUE | tolower(group$status) %in% c("final", "adjudicated"), , drop = FALSE]
    chosen_pool <- if (nrow(final_rows) > 0) final_rows else group

    decision_table <- sort(table(chosen_pool$decision), decreasing = TRUE)
    top_decision <- names(decision_table)[1]
    if (length(decision_table) > 1 && decision_table[1] == decision_table[2]) {
      top_decision <- "conflict"
    }

    chosen_row <- chosen_pool[1, , drop = FALSE]
    if (top_decision != "conflict") {
      matched <- chosen_pool[chosen_pool$decision == top_decision, , drop = FALSE]
      if (nrow(matched) > 0) {
        chosen_row <- matched[which.max(as.numeric(as.Date(matched$decision_date))) %||% 1, , drop = FALSE]
      }
    }

    data.frame(
      M0_DOC_ID = chosen_row$M0_DOC_ID[1],
      stage = chosen_row$stage[1],
      consensus_decision = top_decision,
      reason = m0_pick_consensus_reason(chosen_pool, top_decision),
      reviewers = paste(unique(chosen_pool$reviewer), collapse = ";"),
      n_reviewers = dplyr::n_distinct(chosen_pool$reviewer),
      n_votes = nrow(chosen_pool),
      unresolved = identical(top_decision, "conflict"),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows) %>%
    dplyr::arrange(M0_DOC_ID, stage)
}

#' Derive PRISMA counts from screening consensus
#' @keywords internal
m0_prisma_from_screening_consensus <- function(consensus, merged, exclusion_reasons = data.frame()) {
  if (!is.data.frame(consensus) || nrow(consensus) == 0) {
    return(NULL)
  }

  merged <- m0_org_prepare_input(merged)
  screening_rows <- consensus[consensus$stage == "screening" & consensus$consensus_decision != "conflict", , drop = FALSE]
  eligibility_rows <- consensus[consensus$stage == "eligibility" & consensus$consensus_decision != "conflict", , drop = FALSE]
  included_rows <- consensus[consensus$stage == "included" & consensus$consensus_decision != "conflict", , drop = FALSE]

  screened_docs <- unique(screening_rows$M0_DOC_ID)
  fulltext_docs <- unique(eligibility_rows$M0_DOC_ID)
  included_docs <- unique(c(
    included_rows$M0_DOC_ID[included_rows$consensus_decision == "include"],
    eligibility_rows$M0_DOC_ID[eligibility_rows$consensus_decision == "include"]
  ))

  eligibility_reasons <- exclusion_reasons
  eligibility_reasons <- eligibility_reasons[eligibility_reasons$stage == "eligibility", , drop = FALSE]
  reasons_list <- if (nrow(eligibility_reasons) > 0) {
    stats::setNames(as.list(eligibility_reasons$n_documents), eligibility_reasons$reason)
  } else {
    list()
  }

  included_types <- list()
  if (length(included_docs) > 0 && "DT" %in% names(merged)) {
    included_df <- merged[merged$M0_DOC_ID %in% included_docs, , drop = FALSE]
    counts <- sort(table(trimws(as.character(stats::na.omit(included_df$DT)))), decreasing = TRUE)
    included_types <- stats::setNames(as.list(as.integer(counts)), names(counts))
  }

  list(
    screening = list(
      records_screened = length(screened_docs),
      excluded_screening = sum(screening_rows$consensus_decision == "exclude", na.rm = TRUE)
    ),
    eligibility = list(
      fulltext_assessed = length(fulltext_docs),
      excluded_fulltext = sum(eligibility_rows$consensus_decision == "exclude", na.rm = TRUE),
      excluded_reasons = reasons_list
    ),
    included = list(
      studies_included = length(unique(included_docs)),
      by_type = included_types
    ),
    methods = list(
      screening_derived = TRUE,
      unresolved_screening_conflicts = sum(consensus$unresolved, na.rm = TRUE)
    )
  )
}

#' Build inter-rater reliability summary from screening ledger
#' @keywords internal
m0_build_screening_reliability <- function(ledger) {
  screening_only <- ledger[ledger$stage %in% c("screening", "eligibility"), , drop = FALSE]
  if (nrow(screening_only) == 0 || dplyr::n_distinct(screening_only$reviewer) < 2) {
    return(list(status = "not_available"))
  }

  wide <- reshape(
    screening_only[, c("M0_DOC_ID", "stage", "reviewer", "decision")],
    idvar = c("M0_DOC_ID", "stage"),
    timevar = "reviewer",
    direction = "wide"
  )
  decision_cols <- grep("^decision\\.", names(wide), value = TRUE)
  if (length(decision_cols) < 2) {
    return(list(status = "not_available"))
  }

  ratings <- as.matrix(wide[, decision_cols, drop = FALSE])
  colnames(ratings) <- sub("^decision\\.", "", decision_cols)
  m0_inter_rater_reliability(ratings)
}

#' Merge screening-derived counts into PRISMA data
#' @keywords internal
m0_apply_screening_to_prisma <- function(prisma_data, screening_summary) {
  if (is.null(screening_summary) || !isTRUE(screening_summary$ok) || is.null(screening_summary$prisma)) {
    return(prisma_data)
  }

  prisma_patch <- screening_summary$prisma
  prisma_data$screening <- m0_recursive_fill(prisma_data$screening %||% list(), prisma_patch$screening %||% list())
  prisma_data$eligibility <- m0_recursive_fill(prisma_data$eligibility %||% list(), prisma_patch$eligibility %||% list())
  prisma_data$included <- m0_recursive_fill(prisma_data$included %||% list(), prisma_patch$included %||% list())
  prisma_data$methods <- m0_recursive_fill(prisma_data$methods %||% list(), prisma_patch$methods %||% list())
  prisma_data
}

#' Build a text report from screening summary
#' @keywords internal
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
      lines <- c(lines, paste0(
        "  - ", row$stage, " / ", row$consensus_decision, ": ", row$n_documents
      ))
    }
    lines <- c(lines, "")
  }

  if (is.data.frame(screening_summary$exclusion_reasons) && nrow(screening_summary$exclusion_reasons) > 0) {
    lines <- c(lines, "Exclusion Reasons")
    for (i in seq_len(nrow(screening_summary$exclusion_reasons))) {
      row <- screening_summary$exclusion_reasons[i, ]
      lines <- c(lines, paste0(
        "  - ", row$stage, " / ", row$reason, ": ", row$n_documents
      ))
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
    lines <- c(
      lines,
      "Pending Human Actions",
      paste0("  - ", pending_actions),
      ""
    )
  }

  list(lines = lines)
}

m0_prepare_screening_lookup <- function(merged) {
  merged <- m0_org_prepare_input(merged)
  data.frame(
    M0_DOC_ID = merged$M0_DOC_ID,
    doi = tolower(trimws(as.character(m0_org_get_column(merged, "DI")))),
    title = m0_normalize_title(m0_org_get_column(merged, "TI")),
    stringsAsFactors = FALSE
  )
}

m0_map_screening_records <- function(ledger, lookup) {
  if ("M0_DOC_ID" %in% names(ledger)) {
    ledger$M0_DOC_ID <- suppressWarnings(as.integer(ledger$M0_DOC_ID))
  } else {
    ledger$M0_DOC_ID <- NA_integer_
  }

  if ("doi" %in% names(ledger)) {
    ledger$doi <- tolower(trimws(as.character(ledger$doi)))
  } else {
    ledger$doi <- NA_character_
  }
  if ("title" %in% names(ledger)) {
    ledger$title <- m0_normalize_title(ledger$title)
  } else {
    ledger$title <- NA_character_
  }

  unresolved <- is.na(ledger$M0_DOC_ID)
  if (any(unresolved) && any(!is.na(ledger$doi) & nzchar(ledger$doi))) {
    doi_map <- stats::setNames(lookup$M0_DOC_ID, lookup$doi)
    hit <- doi_map[ledger$doi[unresolved]]
    ledger$M0_DOC_ID[unresolved] <- suppressWarnings(as.integer(hit))
    unresolved <- is.na(ledger$M0_DOC_ID)
  }

  if (any(unresolved) && any(!is.na(ledger$title) & nzchar(ledger$title))) {
    title_map <- stats::setNames(lookup$M0_DOC_ID, lookup$title)
    hit <- title_map[ledger$title[unresolved]]
    ledger$M0_DOC_ID[unresolved] <- suppressWarnings(as.integer(hit))
  }

  ledger
}

m0_normalize_stage <- function(stage) {
  stage <- tolower(trimws(as.character(stage)))
  stage[stage %in% c("title_abstract", "title", "abstract")] <- "screening"
  stage[stage %in% c("fulltext", "full_text", "eligibility_review")] <- "eligibility"
  stage[stage %in% c("include", "final_include", "synthesis")] <- "included"
  stage[stage %in% c("risk", "quality_assessment")] <- "quality"
  stage[is.na(stage) | !nzchar(stage)] <- "screening"
  stage
}

m0_normalize_decision <- function(decision) {
  decision <- tolower(trimws(as.character(decision)))
  decision[decision %in% c("yes", "keep", "accept", "eligible")] <- "include"
  decision[decision %in% c("no", "remove", "reject", "ineligible")] <- "exclude"
  decision[decision %in% c("maybe", "uncertain")] <- "pending"
  decision[is.na(decision) | !nzchar(decision)] <- "pending"
  decision
}

m0_pick_consensus_reason <- function(group, decision) {
  if (!identical(decision, "exclude")) {
    return(NA_character_)
  }
  reasons <- trimws(as.character(group$reason))
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
  if (length(reasons) == 0) return(NA_character_)
  names(sort(table(reasons), decreasing = TRUE))[1]
}
