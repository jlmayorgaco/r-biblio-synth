# ============================================================================
# m4_compute_hypotheses.R - Formal source / venue hypothesis tests
# ============================================================================

m4_compute_hypotheses <- function(data, config = biblio_config()) {
  hypotheses <- list(
    H04_01 = m4_test_bradford_concentration(data, config),
    H04_02 = m4_test_bradford_zone_impact(data, config),
    H04_03 = m4_test_core_source_growth(data, config),
    H04_04 = m4_test_specialist_generalist_impact(data, config),
    H04_05 = m4_test_source_cluster_separability(data, config),
    H04_06 = m4_test_similarity_impact_alignment(data, config),
    H04_07 = m4_test_lifecycle_high_impact_probability(data, config),
    H04_08 = m4_test_lifecycle_trajectory_differences(data, config),
    H04_09 = m4_test_specialization_predicts_efficiency(data, config),
    H04_10 = m4_test_classifier_above_chance(data, config),
    H04_11 = m4_test_anomaly_high_impact_association(data, config),
    H04_12 = m4_test_lifecycle_forecast_direction(data, config)
  )
  summary <- m4_summarize_hypotheses(hypotheses)
  list(
    status = "success",
    hypotheses = hypotheses,
    hyphypotheses = hypotheses,
    n_hypotheses = length(hypotheses),
    n_rejected = summary$n_rejected,
    n_not_rejected = summary$n_failed_to_reject,
    rejection_rate = summary$rejection_rate,
    summary = summary
  )
}

m4_test_bradford_concentration <- function(data, config) {
  impact <- data$impact$impact %||% tibble::tibble()
  if (!is.data.frame(impact) || nrow(impact) < 3 || sum(impact$tp, na.rm = TRUE) <= 0) {
    return(m4_inconclusive_hypothesis("Source productivity is uniformly distributed", "Insufficient source productivity data"))
  }
  observed <- suppressWarnings(as.numeric(impact$tp))
  expected <- rep(sum(observed, na.rm = TRUE) / length(observed), length(observed))
  test <- tryCatch(stats::chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE), error = function(e) NULL)
  if (is.null(test)) {
    return(m4_inconclusive_hypothesis("Source productivity is uniformly distributed", "Chi-square test failed"))
  }
  shares <- observed / sum(observed, na.rm = TRUE)
  hhi <- sum(shares^2, na.rm = TRUE)
  m4_stat_hypothesis(
    "Source productivity is uniformly distributed",
    "Documents are uniformly distributed across sources",
    "Chi-square goodness-of-fit test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    hhi,
    sprintf("Source productivity chi-square = %.3f (p = %.4f), HHI = %.3f.", as.numeric(test$statistic), test$p.value, hhi)
  )
}

m4_test_bradford_zone_impact <- function(data, config) {
  df <- m4_features_with_context(data)
  if (!is.data.frame(df) || nrow(df) < 6 || length(unique(stats::na.omit(df$bradford_zone))) < 2) {
    return(m4_inconclusive_hypothesis("Bradford zones have equal citation impact", "At least two Bradford zones are required"))
  }
  test <- tryCatch(stats::kruskal.test(cpp ~ bradford_zone, data = df), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Bradford zones have equal citation impact", "Kruskal-Wallis failed"))
  m4_stat_hypothesis(
    "Bradford zones have equal citation impact",
    "CPP distributions are equal across Bradford zones",
    "Kruskal-Wallis zone impact test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m4_epsilon_squared(test$statistic, nrow(df), length(unique(df$bradford_zone))),
    sprintf("Bradford-zone CPP chi-square = %.3f (p = %.4f).", as.numeric(test$statistic), test$p.value)
  )
}

m4_test_core_source_growth <- function(data, config) {
  df <- m4_features_with_context(data)
  if (!is.data.frame(df) || nrow(df) < 6 || length(unique(stats::na.omit(df$bradford_zone))) < 2) {
    return(m4_inconclusive_hypothesis("Bradford zones have equal source growth", "At least two Bradford zones are required"))
  }
  test <- tryCatch(stats::kruskal.test(tp_slope ~ bradford_zone, data = df), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Bradford zones have equal source growth", "Kruskal-Wallis failed"))
  m4_stat_hypothesis(
    "Bradford zones have equal source growth",
    "TP slope distributions are equal across Bradford zones",
    "Kruskal-Wallis source-growth test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m4_epsilon_squared(test$statistic, nrow(df), length(unique(df$bradford_zone))),
    sprintf("Bradford-zone TP slope chi-square = %.3f (p = %.4f).", as.numeric(test$statistic), test$p.value)
  )
}

m4_test_specialist_generalist_impact <- function(data, config) {
  spec <- data$specialization$specialization %||% tibble::tibble()
  if (!is.data.frame(spec) || nrow(spec) < 6 || length(unique(stats::na.omit(spec$venue_scope))) < 2) {
    return(m4_inconclusive_hypothesis("Specialist, mixed, and generalist venues have equal impact", "Insufficient specialization groups"))
  }
  test <- tryCatch(stats::kruskal.test(cpp ~ venue_scope, data = spec), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Specialist, mixed, and generalist venues have equal impact", "Kruskal-Wallis failed"))
  m4_stat_hypothesis(
    "Specialist, mixed, and generalist venues have equal impact",
    "CPP distributions are equal across venue-scope groups",
    "Kruskal-Wallis specialization impact test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m4_epsilon_squared(test$statistic, nrow(spec), length(unique(spec$venue_scope))),
    sprintf("Venue-scope CPP chi-square = %.3f (p = %.4f).", as.numeric(test$statistic), test$p.value)
  )
}

m4_test_source_cluster_separability <- function(data, config) {
  clusters <- data$clusters$clusters %||% tibble::tibble()
  if (!is.data.frame(clusters) || nrow(clusters) < 6 || length(unique(stats::na.omit(clusters$cluster))) < 2) {
    return(m4_inconclusive_hypothesis("Source clusters do not differ in bibliometric profile", "Insufficient source clusters"))
  }
  metric <- if ("cpp" %in% names(clusters)) "cpp" else "tc"
  test <- tryCatch(stats::kruskal.test(clusters[[metric]] ~ clusters$cluster), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Source clusters do not differ in bibliometric profile", "Cluster comparison failed"))
  m4_stat_hypothesis(
    "Source clusters do not differ in bibliometric profile",
    "K-means clusters have equal citation/productivity profiles",
    "Kruskal-Wallis cluster separability test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m4_epsilon_squared(test$statistic, nrow(clusters), length(unique(clusters$cluster))),
    sprintf("Cluster %s chi-square = %.3f (p = %.4f).", metric, as.numeric(test$statistic), test$p.value)
  )
}

m4_test_similarity_impact_alignment <- function(data, config) {
  pairs <- data$similarity$pairwise %||% tibble::tibble()
  impact <- data$impact$impact %||% tibble::tibble()
  if (!is.data.frame(pairs) || !is.data.frame(impact) || nrow(pairs) < 8) {
    return(m4_inconclusive_hypothesis("Keyword similarity is unrelated to citation-impact similarity", "Insufficient pairwise similarity data"))
  }
  cpp <- stats::setNames(impact$cpp, impact$source)
  pairs$impact_gap <- abs(cpp[pairs$source_a] - cpp[pairs$source_b])
  pairs <- pairs[is.finite(pairs$similarity) & is.finite(pairs$impact_gap), , drop = FALSE]
  if (nrow(pairs) < 8) {
    return(m4_inconclusive_hypothesis("Keyword similarity is unrelated to citation-impact similarity", "Insufficient finite pairwise impact gaps"))
  }
  test <- tryCatch(stats::cor.test(pairs$similarity, -pairs$impact_gap, method = "spearman", exact = FALSE), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Keyword similarity is unrelated to citation-impact similarity", "Spearman test failed"))
  m4_stat_hypothesis(
    "Keyword similarity is unrelated to citation-impact similarity",
    "Cosine keyword similarity is not associated with smaller CPP gaps",
    "Spearman pairwise association test",
    test$p.value,
    suppressWarnings(as.numeric(test$estimate)),
    suppressWarnings(as.numeric(test$estimate)),
    sprintf("Similarity vs negative CPP gap Spearman rho = %.3f (p = %.4f).", as.numeric(test$estimate), test$p.value)
  )
}

m4_test_lifecycle_high_impact_probability <- function(data, config) {
  lifecycle <- data$lifecycle$lifecycle %||% tibble::tibble()
  preds <- data$advanced_analytics$svm$predictions %||% tibble::tibble()
  if (!is.data.frame(lifecycle) || !is.data.frame(preds) || nrow(preds) < 6) {
    return(m4_inconclusive_hypothesis("Lifecycle stage has no association with high-impact probability", "Insufficient lifecycle or classifier data"))
  }
  df <- dplyr::left_join(lifecycle, preds[, intersect(c("source", "high_impact_probability"), names(preds)), drop = FALSE], by = "source")
  df <- df[is.finite(df$high_impact_probability) & !is.na(df$lifecycle_stage), , drop = FALSE]
  if (nrow(df) < 6 || length(unique(df$lifecycle_stage)) < 2) {
    return(m4_inconclusive_hypothesis("Lifecycle stage has no association with high-impact probability", "At least two lifecycle stages are required"))
  }
  test <- tryCatch(stats::kruskal.test(high_impact_probability ~ lifecycle_stage, data = df), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Lifecycle stage has no association with high-impact probability", "Kruskal-Wallis failed"))
  m4_stat_hypothesis(
    "Lifecycle stage has no association with high-impact probability",
    "Predicted high-impact probability is equal across lifecycle stages",
    "Kruskal-Wallis lifecycle probability test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m4_epsilon_squared(test$statistic, nrow(df), length(unique(df$lifecycle_stage))),
    sprintf("Lifecycle probability chi-square = %.3f (p = %.4f).", as.numeric(test$statistic), test$p.value)
  )
}

m4_test_lifecycle_trajectory_differences <- function(data, config) {
  lifecycle <- data$lifecycle$lifecycle %||% tibble::tibble()
  if (!is.data.frame(lifecycle) || nrow(lifecycle) < 6 || length(unique(stats::na.omit(lifecycle$lifecycle_stage))) < 2) {
    return(m4_inconclusive_hypothesis("Lifecycle stages have equal TP/TC trajectories", "Insufficient lifecycle stages"))
  }
  test <- tryCatch(stats::kruskal.test(tp_slope ~ lifecycle_stage, data = lifecycle), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Lifecycle stages have equal TP/TC trajectories", "Kruskal-Wallis failed"))
  m4_stat_hypothesis(
    "Lifecycle stages have equal TP/TC trajectories",
    "TP slopes are equal across lifecycle stages",
    "Kruskal-Wallis lifecycle trajectory test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m4_epsilon_squared(test$statistic, nrow(lifecycle), length(unique(lifecycle$lifecycle_stage))),
    sprintf("Lifecycle TP-slope chi-square = %.3f (p = %.4f).", as.numeric(test$statistic), test$p.value)
  )
}

m4_test_specialization_predicts_efficiency <- function(data, config) {
  spec <- data$specialization$specialization %||% tibble::tibble()
  if (!is.data.frame(spec) || nrow(spec) < 8 || stats::sd(spec$specialization_score, na.rm = TRUE) <= 0) {
    return(m4_inconclusive_hypothesis("Specialization does not predict citation efficiency after controlling volume", "Insufficient specialization variation"))
  }
  fit <- tryCatch(stats::lm(log1p(cpp) ~ log1p(tp) + specialization_score, data = spec), error = function(e) NULL)
  if (is.null(fit)) return(m4_inconclusive_hypothesis("Specialization does not predict citation efficiency after controlling volume", "Regression failed"))
  coefs <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (is.null(coefs) || !"specialization_score" %in% rownames(coefs)) {
    return(m4_inconclusive_hypothesis("Specialization does not predict citation efficiency after controlling volume", "Specialization coefficient unavailable"))
  }
  p_value <- coefs["specialization_score", "Pr(>|t|)"]
  stat <- coefs["specialization_score", "t value"]
  beta <- coefs["specialization_score", "Estimate"]
  m4_stat_hypothesis(
    "Specialization does not predict citation efficiency after controlling volume",
    "Specialization coefficient equals zero in log CPP regression",
    "Linear model coefficient test",
    p_value,
    stat,
    beta,
    sprintf("Specialization beta = %.3f (t = %.3f, p = %.4f).", beta, stat, p_value)
  )
}

m4_test_classifier_above_chance <- function(data, config) {
  cv <- data$advanced_analytics$ml_cv %||% list()
  summary <- cv$summary %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0 || !is.finite(summary$balanced_accuracy[1])) {
    return(m4_inconclusive_hypothesis("High-impact classifier does not perform above chance", "Cross-validation summary unavailable"))
  }
  best <- summary[1, , drop = FALSE]
  folds <- cv$folds %||% tibble::tibble()
  n <- if (is.data.frame(folds) && nrow(folds) > 0) nrow(folds) else nrow(data$advanced_analytics$features %||% tibble::tibble())
  p_value <- tryCatch(stats::binom.test(round(best$accuracy * n), n, p = 0.5, alternative = "greater")$p.value, error = function(e) NA_real_)
  m4_stat_hypothesis(
    "High-impact classifier does not perform above chance",
    "Cross-validated classification accuracy is not greater than 0.5",
    "One-sided binomial classifier test",
    p_value,
    best$balanced_accuracy,
    best$balanced_accuracy - 0.5,
    sprintf("Best CV model %s balanced accuracy = %.3f (accuracy p = %.4f).", best$model, best$balanced_accuracy, p_value)
  )
}

m4_test_anomaly_high_impact_association <- function(data, config) {
  outliers <- data$advanced_analytics$outliers %||% tibble::tibble()
  features <- data$advanced_analytics$features %||% tibble::tibble()
  if (!is.data.frame(outliers) || !is.data.frame(features) || nrow(outliers) < 6) {
    return(m4_inconclusive_hypothesis("Venue anomaly scores are unrelated to high-impact status", "Insufficient anomaly data"))
  }
  df <- dplyr::left_join(outliers, features[, intersect(c("source", "high_impact"), names(features)), drop = FALSE], by = "source")
  if (length(unique(stats::na.omit(df$high_impact))) < 2) {
    return(m4_inconclusive_hypothesis("Venue anomaly scores are unrelated to high-impact status", "Two high-impact classes are required"))
  }
  test <- tryCatch(stats::wilcox.test(anomaly_score ~ high_impact, data = df, exact = FALSE), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Venue anomaly scores are unrelated to high-impact status", "Wilcoxon test failed"))
  delta <- median(df$anomaly_score[df$high_impact], na.rm = TRUE) - median(df$anomaly_score[!df$high_impact], na.rm = TRUE)
  m4_stat_hypothesis(
    "Venue anomaly scores are unrelated to high-impact status",
    "Anomaly-score distributions are equal for high- and lower-impact sources",
    "Wilcoxon anomaly association test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    delta,
    sprintf("High-impact minus lower-impact anomaly median = %.3f (p = %.4f).", delta, test$p.value)
  )
}

m4_test_lifecycle_forecast_direction <- function(data, config) {
  lifecycle <- data$lifecycle$lifecycle %||% tibble::tibble()
  if (!is.data.frame(lifecycle) || nrow(lifecycle) < 6 || !"tp_forecast" %in% names(lifecycle)) {
    return(m4_inconclusive_hypothesis("Lifecycle forecast vectors have no positive field migration", "Insufficient lifecycle forecasts"))
  }
  delta <- suppressWarnings(as.numeric(lifecycle$tp_forecast - lifecycle$tp))
  delta <- delta[is.finite(delta)]
  if (length(delta) < 6) {
    return(m4_inconclusive_hypothesis("Lifecycle forecast vectors have no positive field migration", "Insufficient finite forecast deltas"))
  }
  positive <- sum(delta > 0)
  test <- tryCatch(stats::binom.test(positive, length(delta), p = 0.5, alternative = "greater"), error = function(e) NULL)
  if (is.null(test)) return(m4_inconclusive_hypothesis("Lifecycle forecast vectors have no positive field migration", "Binomial test failed"))
  m4_stat_hypothesis(
    "Lifecycle forecast vectors have no positive field migration",
    "The share of positive source TP forecast vectors is not greater than 0.5",
    "One-sided binomial direction test",
    test$p.value,
    positive,
    positive / length(delta),
    sprintf("%d/%d source TP forecast vectors are positive (p = %.4f).", positive, length(delta), test$p.value)
  )
}

m4_features_with_context <- function(data) {
  impact <- data$impact$impact %||% tibble::tibble()
  growth <- data$growth$growth %||% tibble::tibble()
  bradford <- data$bradford$zones %||% tibble::tibble()
  if (!is.data.frame(impact) || nrow(impact) == 0) return(tibble::tibble())
  growth_ctx <- if (is.data.frame(growth) && nrow(growth) > 0 && "source" %in% names(growth)) {
    growth[, intersect(c("source", "tp_slope", "tc_slope", "cagr"), names(growth)), drop = FALSE]
  } else {
    tibble::tibble(source = character(), tp_slope = numeric(), tc_slope = numeric(), cagr = numeric())
  }
  bradford_ctx <- if (is.data.frame(bradford) && nrow(bradford) > 0 && "source" %in% names(bradford)) {
    bradford[, intersect(c("source", "bradford_zone"), names(bradford)), drop = FALSE]
  } else {
    tibble::tibble(source = character(), bradford_zone = character())
  }
  impact |>
    dplyr::left_join(growth_ctx, by = "source") |>
    dplyr::left_join(bradford_ctx, by = "source") |>
    dplyr::mutate(
      tp_slope = dplyr::coalesce(.data$tp_slope, 0),
      tc_slope = dplyr::coalesce(.data$tc_slope, 0)
    )
}

m4_inconclusive_hypothesis <- function(label, interpretation = "Insufficient data", null = NULL) {
  list(
    hypothesis = label,
    hyphypothesis = label,
    null = null %||% label,
    result = "inconclusive",
    evidence_class = "statistical",
    test = "not estimable",
    interpretation = interpretation
  )
}

m4_stat_hypothesis <- function(label, null, test, p_value, statistic, effect_size, interpretation) {
  p_value <- suppressWarnings(as.numeric(p_value)[1])
  list(
    hypothesis = label,
    hyphypothesis = label,
    null = null,
    test = test,
    evidence_class = "statistical",
    result = if (is.finite(p_value) && p_value <= 0.05) "reject" else if (is.finite(p_value)) "fail_to_reject" else "inconclusive",
    statistic = suppressWarnings(as.numeric(statistic)[1]),
    p_value = p_value,
    effect_size = suppressWarnings(as.numeric(effect_size)[1]),
    interpretation = interpretation
  )
}

m4_summarize_hypotheses <- function(hypotheses) {
  n_total <- length(hypotheses)
  n_rejected <- sum(vapply(hypotheses, function(h) identical(h$result, "reject"), logical(1)), na.rm = TRUE)
  n_failed <- sum(vapply(hypotheses, function(h) identical(h$result, "fail_to_reject"), logical(1)), na.rm = TRUE)
  list(
    n_total = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed,
    rejection_rate = if (n_total > 0) n_rejected / n_total else NA_real_
  )
}

m4_epsilon_squared <- function(statistic, n, k) {
  h <- suppressWarnings(as.numeric(statistic)[1])
  if (!is.finite(h) || !is.finite(n) || !is.finite(k) || n <= k) return(NA_real_)
  max(0, (h - k + 1) / (n - k))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
