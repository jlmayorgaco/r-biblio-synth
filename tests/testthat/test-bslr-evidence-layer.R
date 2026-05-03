# ============================================================================
# test-bslr-evidence-layer.R
# ============================================================================

test_that("claim ledger captures automatic hypotheses with strength and limitations", {
  module_results <- list(
    m2 = list(
      module_id = "m2",
      status = "success",
      data = list(
        advanced_journal = list(
          hypotheses = list(
            table = data.frame(
              hypothesis_id = "M2_H12",
              question = "Does the forecast outperform the naive baseline?",
              test = "rolling_origin_mase",
              effect_size = 0.82,
              confidence_interval = NA_character_,
              p_value = 0.01,
              p_adjusted = 0.02,
              decision = "supported",
              plain_language_interpretation = "Forecast validation beats the naive baseline.",
              stringsAsFactors = FALSE
            )
          )
        )
      )
    )
  )

  ledger <- build_claim_ledger(module_results)
  expect_equal(nrow(ledger), 1)
  expect_equal(ledger$claim_id, "M2_H12")
  expect_true(ledger$strength %in% c("moderate", "strong"))
  expect_true(all(c("evidence_summary", "table_source", "plot_source", "limitation") %in% names(ledger)))
})

test_that("B-SLR quality gate records blocking methodological issues", {
  m0_result <- list(
    status = "success",
    data = list(
      screening_summary = list(
        ok = FALSE,
        pending_actions = "Provide a reviewer-level screening ledger.",
        reliability = list(status = "not_available")
      ),
      prisma_validation = list(ok = FALSE, warnings = "counts-only")
    )
  )
  protocol_validation <- list(
    ok = TRUE,
    missing_required = character(),
    missing_human_review = "reviewers",
    missing_journal_grade = "cluster_labeling_process"
  )
  claim_ledger <- data.frame(
    claim_id = "M2_H12",
    module_id = "m2",
    claim = "Forecast beats naive baseline.",
    evidence_summary = "Example",
    table_source = "x",
    plot_source = "y",
    test = "rolling_origin_mase",
    effect_size = 0.8,
    confidence_interval = NA_character_,
    p_value = 0.01,
    p_adjusted = 0.02,
    decision = "supported",
    strength = "moderate",
    limitation = "Example limitation",
    status = "auditable",
    stringsAsFactors = FALSE
  )
  gate <- bslr_build_quality_gate(
    protocol_validation = protocol_validation,
    m0_result = m0_result,
    module_results = list(m0 = m0_result),
    claim_ledger = claim_ledger,
    parity = list(status = "skipped", summary = data.frame()),
    reproducibility = list(status = "success", summary = data.frame())
  )

  expect_equal(gate$status, "warning")
  expect_true(nrow(gate$checklist) >= 10)
  expect_true(any(gate$checklist$blocking))
  expect_equal(gate$recommendation, "not_journal_ready")
})

test_that("reproducibility capsule summarizes module status", {
  result <- list(
    module_id = "bslr",
    data = list(
      modules = list(
        m0 = list(module_id = "m0", status = "success", inputs = list(n_total = 3), data = list())
      )
    ),
    artifacts = list()
  )
  capsule <- build_reproducibility_capsule(result, biblio_config(export = FALSE))
  expect_equal(capsule$status, "success")
  expect_true(nrow(capsule$module_status) == 1)
  expect_true(length(capsule$session_info) > 0)
})
