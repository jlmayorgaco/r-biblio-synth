# ============================================================================
# test-module-m3-advanced-journal.R
# ============================================================================

make_m3_advanced_journal_fixture <- function(n = 160) {
  set.seed(7)
  countries <- c(
    "UNITED STATES", "CANADA", "CHINA", "GERMANY", "FRANCE",
    "BRAZIL", "INDIA", "AUSTRALIA", "SPAIN", "ITALY"
  )
  make_co <- function(i) {
    k <- sample(1:3, 1)
    paste(sample(countries, k, replace = FALSE), collapse = ";")
  }
  data.frame(
    PY = sample(2005:2024, n, replace = TRUE),
    TC = rpois(n, lambda = sample(1:40, n, replace = TRUE)),
    AU_CO = vapply(seq_len(n), make_co, character(1)),
    stringsAsFactors = FALSE
  )
}

test_that("M3 advanced journal layer returns collaboration, mobility, trajectories, concentration, regions, and hypotheses", {
  cfg <- biblio_config(
    export = FALSE,
    advanced_analytics = TRUE,
    bootstrap_n = 25,
    min_countries_for_advanced_geo = 5,
    validate_strict = FALSE
  )
  result <- run_m3(make_m3_advanced_journal_fixture(), cfg, export = FALSE)
  advanced <- result$data$advanced_journal

  expect_equal(advanced$status, "success")
  expect_true(nrow(advanced$collaboration_premium$table) == 1)
  expect_true(nrow(advanced$mobility$rank_windows) >= 5)
  expect_true(nrow(advanced$trajectories$table) >= 5)
  expect_true(nrow(advanced$geo_concentration$table) == 2)
  expect_true(all(c("gini", "theil", "hhi", "palma_ratio", "top5_share", "top10_share") %in% names(advanced$geo_concentration$table)))
  expect_true(nrow(advanced$regional_decomposition$table) > 0)
  expect_true(nrow(advanced$country_metrics$country_table) >= 5)
  expect_true(all(c("full_articles", "fractional_articles", "citations_per_year", "age_normalized_impact", "mcp_ratio", "share_acceleration") %in% names(advanced$country_metrics$country_table)))
  expect_true(nrow(advanced$collaboration_network$edges) > 0)
  expect_true(nrow(advanced$scp_mcp_trends$annual) > 0)
  expect_true(nrow(advanced$uncertainty$intervals) > 0)
  expect_true(nrow(advanced$robustness$rank_sensitivity) >= 2)
  expect_true(nrow(advanced$inequality_decomposition$window_metrics) > 0)
  expect_true(nrow(advanced$trend_models$table) > 0)
  expect_true(nrow(advanced$outliers$table) > 0)
  expect_true(all(sprintf("M3_H%02d", 9:22) %in% advanced$hypotheses$table$hypothesis_id))
})

test_that("M3 advanced journal layer is fail-soft on sparse country coverage", {
  cfg <- biblio_config(
    export = FALSE,
    advanced_analytics = TRUE,
    min_countries_for_advanced_geo = 8,
    validate_strict = FALSE
  )
  sparse <- data.frame(
    PY = 2020:2024,
    TC = c(1, 2, 3, 4, 5),
    AU_CO = c("CANADA", "CANADA", "UNITED STATES", "CANADA", "UNITED STATES"),
    stringsAsFactors = FALSE
  )
  prepared <- prepare_m3_country_data(sparse, cfg)
  advanced <- m3_compute_advanced_journal(prepared, data = list(), config = cfg)

  expect_equal(advanced$status, "insufficient_data")
  expect_true(nrow(advanced$hypotheses$table) >= 14)
  expect_true(all(advanced$hypotheses$table$decision == "inconclusive"))
})

test_that("M3 advanced journal plots and tables are integrated into module artifacts", {
  cfg <- biblio_config(
    export = FALSE,
    advanced_analytics = TRUE,
    bootstrap_n = 25,
    min_countries_for_advanced_geo = 5,
    validate_strict = FALSE
  )
  result <- run_m3(make_m3_advanced_journal_fixture(), cfg, export = FALSE)

  expect_true("advanced_journal" %in% names(result$artifacts$plots))
  expect_true("advanced_journal" %in% names(result$artifacts$tables))
  expect_true(length(result$artifacts$plots$advanced_journal$plots) > 0)
  expect_true(all(c("country_contribution_decomposition", "temporal_country_heatmap", "scp_mcp_trend", "impact_collaboration_quadrant", "normalized_impact", "robustness_dashboard") %in% names(result$artifacts$plots$advanced_journal$plots)))
  expect_true(nrow(result$artifacts$tables$advanced_journal$table) >= 25)
  expect_true(any(grepl("Advanced Journal Analytics", result$artifacts$reports[[1]]$lines)))
})
