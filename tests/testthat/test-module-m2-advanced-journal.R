# ============================================================================
# test-module-m2-advanced-journal.R
# ============================================================================

make_m2_advanced_journal_fixture <- function() {
  set.seed(42)
  years <- 2000:2024
  t <- seq_along(years)
  cumulative <- 120 / (1 + exp(-0.35 * (t - 12)))
  articles <- pmax(1, round(c(cumulative[1], diff(cumulative)) + rnorm(length(t), 0, 1)))
  data.frame(Year = years, Articles = articles, stringsAsFactors = FALSE)
}

test_that("M2 advanced journal layer returns uncertainty, regimes, validation, consensus, and hypotheses", {
  cfg <- biblio_config(
    export = FALSE,
    advanced_analytics = TRUE,
    bootstrap_n = 25,
    min_years_for_advanced_ts = 10,
    validate_strict = FALSE
  )

  result <- run_m2(make_m2_advanced_journal_fixture(), cfg, export = FALSE)
  advanced <- result$data$advanced_journal

  expect_equal(advanced$status, "success")
  expect_true(nrow(advanced$model_uncertainty$table) > 0)
  expect_true(nrow(advanced$growth_regimes$summary) == 1)
  expect_true(nrow(advanced$forecast_validation$leaderboard) > 0)
  expect_true("Naive" %in% advanced$forecast_validation$leaderboard$model)
  expect_true(nrow(advanced$changepoint_consensus$table) >= 0)
  expect_true(all(c(
    "M2_H09", "M2_H10", "M2_H11", "M2_H12", "M2_H13",
    "M2_H14", "M2_H15", "M2_H16", "M2_H17", "M2_H18"
  ) %in% advanced$hypotheses$table$hypothesis_id))
  expect_true(all(c(
    "hypothesis_id", "question", "test", "effect_size", "confidence_interval",
    "p_value", "p_adjusted", "decision", "plain_language_interpretation"
  ) %in% names(advanced$hypotheses$table)))
})

test_that("M2 advanced journal layer is fail-soft on short series", {
  cfg <- biblio_config(
    export = FALSE,
    advanced_analytics = TRUE,
    min_years_for_advanced_ts = 12,
    validate_strict = FALSE
  )
  short <- data.frame(Year = 2020:2024, Articles = c(1, 2, 3, 4, 5))
  result <- compute_m2_advanced_journal(short, data = list(), config = cfg)

  expect_equal(result$status, "insufficient_data")
  expect_true(nrow(result$hypotheses$table) >= 10)
  expect_true(all(result$hypotheses$table$decision == "inconclusive"))
})

test_that("M2 advanced journal plots and tables are integrated into module artifacts", {
  cfg <- biblio_config(
    export = FALSE,
    advanced_analytics = TRUE,
    bootstrap_n = 25,
    min_years_for_advanced_ts = 10,
    validate_strict = FALSE
  )
  result <- run_m2(make_m2_advanced_journal_fixture(), cfg, export = FALSE)

  expect_true("advanced_journal" %in% names(result$artifacts$plots))
  expect_true("advanced_journal" %in% names(result$artifacts$tables))
  expect_true(length(result$artifacts$plots$advanced_journal$plots) > 0)
  expect_true(nrow(result$artifacts$tables$advanced_journal$table) >= 5)
  expect_true(any(grepl("Advanced Journal Analytics", result$artifacts$reports[[1]]$lines)))
})
