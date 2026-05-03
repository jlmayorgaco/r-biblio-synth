# ============================================================================
# claim_ledger.R - Evidence ledger for automated claims
# ============================================================================

#' Build a claim ledger from module hypotheses and B-SLR gates
#'
#' @param module_results Named list of module results, or a B-SLR result.
#' @param journal_assessment Optional B-SLR journal assessment list.
#' @param config Configuration list.
#' @return Data frame with one row per auditable automated claim.
#' @export
build_claim_ledger <- function(module_results,
                               journal_assessment = NULL,
                               config = biblio_config()) {
  config <- merge_biblio_config(config)

  if (inherits(module_results, "biblio_module_result") &&
      identical(module_results$module_id %||% "", "bslr")) {
    journal_assessment <- journal_assessment %||% module_results$data$journal_assessment %||% list()
    module_results <- module_results$data$modules %||% list()
  }

  if (!is.list(module_results)) {
    return(biblio_empty_claim_ledger())
  }

  rows <- list()
  for (module_id in names(module_results)) {
    module_result <- module_results[[module_id]]
    tables <- biblio_collect_hypothesis_tables(module_result$data %||% list())
    if (length(tables) == 0) {
      next
    }
    for (tbl_info in tables) {
      tbl <- tbl_info$table
      if (!is.data.frame(tbl) || nrow(tbl) == 0) {
        next
      }
      rows[[length(rows) + 1L]] <- biblio_hypothesis_table_to_claims(
        tbl,
        module_id = module_id,
        table_source = tbl_info$path
      )
    }
  }

  gate_rows <- biblio_journal_gates_to_claims(journal_assessment)
  if (is.data.frame(gate_rows) && nrow(gate_rows) > 0) {
    rows[[length(rows) + 1L]] <- gate_rows
  }

  ledger <- if (length(rows) > 0) dplyr::bind_rows(rows) else biblio_empty_claim_ledger()
  if (!is.data.frame(ledger) || nrow(ledger) == 0) {
    return(biblio_empty_claim_ledger())
  }

  ledger <- ledger |>
    dplyr::mutate(
      claim_id = ifelse(
        is.na(.data$claim_id) | !nzchar(.data$claim_id),
        paste0("CLAIM_", sprintf("%03d", dplyr::row_number())),
        .data$claim_id
      ),
      strength = biblio_claim_strength(
        decision = .data$decision,
        p_value = .data$p_value,
        p_adjusted = .data$p_adjusted,
        effect_size = .data$effect_size,
        confidence_interval = .data$confidence_interval
      ),
      status = ifelse(.data$strength == "inconclusive", "needs_human_review", "auditable")
    ) |>
    dplyr::select(
      "claim_id", "module_id", "claim", "evidence_summary", "table_source",
      "plot_source", "test", "effect_size", "confidence_interval", "p_value",
      "p_adjusted", "decision", "strength", "limitation", "status"
    )

  data.frame(ledger, stringsAsFactors = FALSE)
}

biblio_empty_claim_ledger <- function() {
  data.frame(
    claim_id = character(),
    module_id = character(),
    claim = character(),
    evidence_summary = character(),
    table_source = character(),
    plot_source = character(),
    test = character(),
    effect_size = numeric(),
    confidence_interval = character(),
    p_value = numeric(),
    p_adjusted = numeric(),
    decision = character(),
    strength = character(),
    limitation = character(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

biblio_collect_hypothesis_tables <- function(x, path = "data") {
  out <- list()
  if (is.data.frame(x)) {
    required <- c("hypothesis_id", "question", "test", "decision")
    if (all(required %in% names(x))) {
      return(list(list(path = path, table = x)))
    }
    return(out)
  }
  if (!is.list(x) || length(x) == 0) {
    return(out)
  }

  nms <- names(x)
  if (is.null(nms)) nms <- paste0("item", seq_along(x))
  for (i in seq_along(x)) {
    child <- biblio_collect_hypothesis_tables(x[[i]], paste(path, nms[i], sep = "$"))
    if (length(child) > 0) out <- c(out, child)
  }
  out
}

biblio_hypothesis_table_to_claims <- function(tbl, module_id, table_source) {
  tbl <- data.frame(tbl, stringsAsFactors = FALSE)
  get_col <- function(name, default = NA) {
    if (name %in% names(tbl)) tbl[[name]] else rep(default, nrow(tbl))
  }

  claim_ids <- as.character(get_col("hypothesis_id", NA_character_))
  questions <- as.character(get_col("question", NA_character_))
  interpretations <- as.character(get_col("plain_language_interpretation", NA_character_))
  decisions <- as.character(get_col("decision", "inconclusive"))
  limitations <- ifelse(
    decisions %in% c("supported"),
    "Automated statistical support; final manuscript claim still requires domain interpretation.",
    ifelse(nzchar(interpretations), interpretations, "Evidence is incomplete or statistically inconclusive.")
  )

  data.frame(
    claim_id = claim_ids,
    module_id = module_id,
    claim = questions,
    evidence_summary = interpretations,
    table_source = table_source,
    plot_source = biblio_default_plot_source(module_id, claim_ids),
    test = as.character(get_col("test", NA_character_)),
    effect_size = suppressWarnings(as.numeric(get_col("effect_size", NA_real_))),
    confidence_interval = as.character(get_col("confidence_interval", NA_character_)),
    p_value = suppressWarnings(as.numeric(get_col("p_value", NA_real_))),
    p_adjusted = suppressWarnings(as.numeric(get_col("p_adjusted", NA_real_))),
    decision = decisions,
    strength = "inconclusive",
    limitation = limitations,
    status = "pending",
    stringsAsFactors = FALSE
  )
}

biblio_journal_gates_to_claims <- function(journal_assessment) {
  gates <- journal_assessment$gates %||% data.frame()
  if (!is.data.frame(gates) || nrow(gates) == 0 || !"gate" %in% names(gates)) {
    return(biblio_empty_claim_ledger())
  }

  status <- if ("status" %in% names(gates)) gates$status else rep(FALSE, nrow(gates))
  status <- status %in% TRUE
  evidence <- if ("evidence" %in% names(gates)) as.character(gates$evidence) else rep(NA_character_, nrow(gates))
  action <- if ("action" %in% names(gates)) as.character(gates$action) else rep(NA_character_, nrow(gates))

  data.frame(
    claim_id = paste0("BSLR_GATE_", toupper(gsub("[^A-Za-z0-9]+", "_", gates$gate))),
    module_id = "bslr",
    claim = paste0("Methodological gate `", gates$gate, "` is satisfied."),
    evidence_summary = evidence,
    table_source = "journal_assessment$gates",
    plot_source = "bslr_workflow/checkpoints",
    test = "methodological_gate",
    effect_size = NA_real_,
    confidence_interval = NA_character_,
    p_value = NA_real_,
    p_adjusted = NA_real_,
    decision = ifelse(status, "supported", "inconclusive"),
    strength = ifelse(status, "moderate", "inconclusive"),
    limitation = ifelse(status, "Gate is automatically auditable from supplied protocol artifacts.", action),
    status = ifelse(status, "auditable", "needs_human_review"),
    stringsAsFactors = FALSE
  )
}

biblio_claim_strength <- function(decision,
                                  p_value = NA_real_,
                                  p_adjusted = NA_real_,
                                  effect_size = NA_real_,
                                  confidence_interval = NA_character_) {
  decision <- tolower(as.character(decision))
  p <- suppressWarnings(as.numeric(p_adjusted))
  p_raw <- suppressWarnings(as.numeric(p_value))
  p[!is.finite(p) & is.finite(p_raw)] <- p_raw[!is.finite(p) & is.finite(p_raw)]
  effect_size <- suppressWarnings(abs(as.numeric(effect_size)))

  out <- rep("inconclusive", length(decision))
  supported <- decision %in% c("supported", "pass", "true")
  not_supported <- decision %in% c("not_supported", "not supported", "fail", "false")
  weak_supported <- supported & (!is.finite(p) | p >= 0.05)
  moderate_supported <- supported & is.finite(p) & p < 0.05
  strong_supported <- supported & is.finite(p) & p < 0.01

  out[weak_supported] <- "weak"
  out[moderate_supported] <- "moderate"
  out[strong_supported] <- "strong"
  out[supported & is.finite(effect_size) & effect_size >= 0.8 & (!is.finite(p) | p < 0.05)] <- "strong"
  out[not_supported] <- "weak"
  out[decision %in% c("inconclusive", "not_estimable", "pending", "unknown")] <- "inconclusive"
  out
}

biblio_default_plot_source <- function(module_id, claim_ids) {
  module_id <- tolower(as.character(module_id %||% ""))
  claim_ids <- as.character(claim_ids)
  if (identical(module_id, "m2")) {
    return(dplyr::case_when(
      claim_ids == "M2_H09" ~ "m2/advanced_journal/model_selection",
      claim_ids == "M2_H10" ~ "m2/advanced_journal/growth_regime",
      claim_ids == "M2_H11" ~ "m2/advanced_journal/changepoint_consensus_timeline",
      claim_ids == "M2_H12" ~ "m2/advanced_journal/forecast_backtesting_heatmap",
      claim_ids == "M2_H13" ~ "m2/advanced_journal/prediction_interval_calibration",
      claim_ids == "M2_H14" ~ "m2/advanced_journal/ensemble_forecast_intervals",
      claim_ids == "M2_H15" ~ "m2/advanced_journal/segmented_regression_screen",
      claim_ids == "M2_H16" ~ "m2/advanced_journal/model_uncertainty_forest",
      claim_ids == "M2_H17" ~ "m2/advanced_journal/early_warning_acceleration",
      claim_ids == "M2_H18" ~ "m2/advanced_journal/model_confidence_set",
      TRUE ~ "m2/hypotheses"
    ))
  }
  if (identical(module_id, "m3")) {
    return(dplyr::case_when(
      claim_ids == "M3_H09" ~ "m3/advanced_journal/collaboration_premium_forest",
      claim_ids == "M3_H10" ~ "m3/advanced_journal/concentration_dashboard",
        claim_ids == "M3_H11" ~ "m3/advanced_journal/rank_mobility_bump",
        claim_ids == "M3_H12" ~ "m3/advanced_journal/emerging_declining_bubbles",
        claim_ids == "M3_H13" ~ "m3/advanced_journal/regional_decomposition_area",
        claim_ids == "M3_H19" ~ "m3/advanced_journal/spatial_autocorrelation_lisa",
        claim_ids == "M3_H20" ~ "m3/advanced_journal/collaboration_backbone",
        claim_ids == "M3_H21" ~ "m3/advanced_journal/gravity_model_table",
        claim_ids == "M3_H22" ~ "m3/advanced_journal/country_role_taxonomy",
        TRUE ~ "m3/hypotheses"
    ))
  }
  rep(paste0(module_id, "/hypotheses"), length(claim_ids))
}

bslr_build_claim_ledger_report <- function(claim_ledger) {
  if (!is.data.frame(claim_ledger) || nrow(claim_ledger) == 0) {
    lines <- c(
      "============================================================",
      "Automated Claim Ledger",
      paste("Generated:", Sys.time()),
      "============================================================",
      "",
      "No auditable automated claims were available. Run M1-M3 with hypotheses enabled before using the manuscript as evidence-led output."
    )
    return(list(lines = lines, tex = c("\\section*{Automated Claim Ledger}", "No auditable automated claims were available.")))
  }

  summary <- claim_ledger |>
    dplyr::count(.data$module_id, .data$strength, name = "n") |>
    dplyr::arrange(.data$module_id, .data$strength)

  lines <- c(
    "============================================================",
    "Automated Claim Ledger",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    paste0("Claims captured: ", nrow(claim_ledger)),
    ""
  )

  lines <- c(lines, "Claim strength summary")
  for (i in seq_len(nrow(summary))) {
    row <- summary[i, , drop = FALSE]
    lines <- c(lines, paste0("  - ", row$module_id, " / ", row$strength, ": ", row$n))
  }

  lines <- c(lines, "", "Claims")
  for (i in seq_len(nrow(claim_ledger))) {
    row <- claim_ledger[i, , drop = FALSE]
    lines <- c(
      lines,
      paste0("  - ", row$claim_id, " [", row$strength, "] ", row$claim),
      paste0("    Evidence: ", row$evidence_summary),
      paste0("    Source: ", row$table_source, " | Plot: ", row$plot_source),
      paste0("    Test: ", row$test, " | Decision: ", row$decision),
      paste0("    Limitation: ", row$limitation)
    )
  }

  tex <- c(
    "\\section*{Automated Claim Ledger}",
    paste0("Claims captured: ", nrow(claim_ledger), "\\\\")
  )
  list(lines = lines, tex = tex)
}
