# ============================================================================
# m4_compute_advanced_analytics.R - Advanced source analytics and ML
# ============================================================================

m4_compute_advanced_analytics <- function(data, config = biblio_config()) {
  feature_table <- m4_advanced_feature_table(data)
  if (!is.data.frame(feature_table) || nrow(feature_table) < 3) {
    return(list(status = "empty", reason = "At least three sources are required.", features = tibble::tibble()))
  }

  features <- m4_advanced_add_archetypes(feature_table)
  outliers <- m4_advanced_outliers(features)
  regression <- m4_advanced_regression(features)
  svm <- m4_advanced_svm_classifier(features, config)
  silhouette <- m4_advanced_cluster_separation(data$clusters$clusters %||% tibble::tibble())
  patterns <- m4_advanced_pattern_story(features, outliers, regression, svm, silhouette)

  list(
    status = "success",
    features = features,
    outliers = outliers,
    regression = regression,
    svm = svm,
    silhouette = silhouette,
    patterns = patterns
  )
}

m4_advanced_feature_table <- function(data) {
  impact <- data$impact$impact %||% tibble::tibble()
  growth <- data$growth$growth %||% tibble::tibble()
  clusters <- data$clusters$clusters %||% tibble::tibble()
  bradford <- data$bradford$zones %||% tibble::tibble()
  keywords <- data$keywords$source_keywords %||% tibble::tibble()
  if (!is.data.frame(impact) || nrow(impact) == 0) return(tibble::tibble())

  impact |>
    dplyr::select("source", "tp", "tc", "cpp", "h_index", "g_index", "active_years", "share") |>
    dplyr::left_join(growth |> dplyr::select("source", "tp_slope", "tc_slope", "cagr"), by = "source") |>
    dplyr::left_join(clusters |> dplyr::select("source", "cluster", "pc1", "pc2"), by = "source") |>
    dplyr::left_join(bradford |> dplyr::select("source", "bradford_zone"), by = "source") |>
    dplyr::left_join(keywords, by = "source") |>
    dplyr::mutate(
      tp_slope = dplyr::coalesce(.data$tp_slope, 0),
      tc_slope = dplyr::coalesce(.data$tc_slope, 0),
      cagr = dplyr::coalesce(.data$cagr, 0),
      citation_leverage = .data$tc / pmax(sum(.data$tc, na.rm = TRUE), .Machine$double.eps),
      productivity_leverage = .data$tp / pmax(sum(.data$tp, na.rm = TRUE), .Machine$double.eps),
      impact_efficiency = .data$cpp / pmax(stats::median(.data$cpp, na.rm = TRUE), .Machine$double.eps),
      high_impact = .data$tc >= stats::quantile(.data$tc, 0.75, na.rm = TRUE),
      high_growth = .data$tp_slope > stats::median(.data$tp_slope, na.rm = TRUE),
      source_label = substr(.data$source, 1, 42)
    )
}

m4_advanced_add_archetypes <- function(features) {
  tp_med <- stats::median(features$tp, na.rm = TRUE)
  tc_med <- stats::median(features$tc, na.rm = TRUE)
  growth_med <- stats::median(features$tp_slope, na.rm = TRUE)
  cpp_med <- stats::median(features$cpp, na.rm = TRUE)
  features |>
    dplyr::mutate(
      archetype = dplyr::case_when(
        .data$tp >= tp_med & .data$tc >= tc_med & .data$tp_slope >= growth_med ~ "Core growth engine",
        .data$tp >= tp_med & .data$tc < tc_med ~ "Volume-heavy low-impact venue",
        .data$tp < tp_med & .data$tc >= tc_med & .data$cpp >= cpp_med ~ "Selective high-impact venue",
        .data$tp_slope > growth_med & .data$cpp >= cpp_med ~ "Emerging high-impact venue",
        TRUE ~ "Peripheral or stable niche"
      )
    )
}

m4_advanced_outliers <- function(features) {
  if (!is.data.frame(features) || nrow(features) < 4) return(tibble::tibble())
  numeric_df <- features |>
    dplyr::transmute(
      source = .data$source,
      log_tp = log1p(.data$tp),
      log_tc = log1p(.data$tc),
      log_cpp = log1p(.data$cpp),
      log_h = log1p(.data$h_index),
      tp_slope = .data$tp_slope,
      tc_slope = .data$tc_slope
    )
  x <- as.data.frame(numeric_df[, setdiff(names(numeric_df), "source"), drop = FALSE])
  x[!is.finite(as.matrix(x))] <- 0
  center <- colMeans(x, na.rm = TRUE)
  scaled <- scale(x, center = center, scale = apply(x, 2, stats::sd, na.rm = TRUE))
  scaled[!is.finite(scaled)] <- 0
  score <- sqrt(rowSums(scaled^2))
  threshold <- stats::quantile(score, 0.9, na.rm = TRUE)
  tibble::tibble(
    source = numeric_df$source,
    anomaly_score = score,
    anomaly_flag = score >= threshold
  ) |>
    dplyr::arrange(dplyr::desc(.data$anomaly_score))
}

m4_advanced_regression <- function(features) {
  if (!is.data.frame(features) || nrow(features) < 4) {
    return(list(status = "empty", coefficients = tibble::tibble(), fitted = tibble::tibble(), r_squared = NA_real_))
  }
  df <- features |>
    dplyr::transmute(
      source = .data$source,
      y = log1p(.data$tc),
      log_tp = log1p(.data$tp),
      log_cpp = log1p(.data$cpp),
      h_index = log1p(.data$h_index),
      tp_slope = .data$tp_slope
    )
  if (nrow(df) < 4) return(list(status = "empty", coefficients = tibble::tibble(), fitted = tibble::tibble(), r_squared = NA_real_))
  fit <- tryCatch(stats::lm(y ~ log_tp + log_cpp + h_index + tp_slope, data = df), error = function(e) NULL)
  if (is.null(fit)) return(list(status = "error", coefficients = tibble::tibble(), fitted = tibble::tibble(), r_squared = NA_real_))
  coefs <- as.data.frame(summary(fit)$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  names(coefs)[1:4] <- c("estimate", "std_error", "t_value", "p_value")
  fitted <- tibble::tibble(
    source = df$source,
    observed_log_tc = df$y,
    predicted_log_tc = as.numeric(stats::predict(fit, newdata = df)),
    residual = stats::resid(fit)
  )
  list(
    status = "success",
    coefficients = tibble::as_tibble(coefs[, c("term", "estimate", "std_error", "t_value", "p_value")]),
    fitted = fitted,
    r_squared = summary(fit)$r.squared,
    adj_r_squared = summary(fit)$adj.r.squared
  )
}

m4_advanced_svm_classifier <- function(features, config = biblio_config()) {
  if (!is.data.frame(features) || nrow(features) < 6 || length(unique(features$high_impact)) < 2) {
    return(list(status = "insufficient_data", reason = "SVM requires at least six sources and two impact classes.", predictions = tibble::tibble()))
  }
  df <- features |>
    dplyr::transmute(
      source = .data$source,
      high_impact = factor(ifelse(.data$high_impact, "high_impact", "lower_impact")),
      log_tp = log1p(.data$tp),
      log_cpp = log1p(.data$cpp),
      h_index = log1p(.data$h_index),
      tp_slope = .data$tp_slope,
      tc_slope = .data$tc_slope
    )
  if (!requireNamespace("e1071", quietly = TRUE)) {
    return(m4_advanced_logistic_classifier(df))
  }
  predictor_cols <- c("log_tp", "log_cpp", "h_index", "tp_slope", "tc_slope")
  variable_cols <- predictor_cols[vapply(df[predictor_cols], function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1))]
  if (length(variable_cols) < 2) return(m4_advanced_logistic_classifier(df))
  formula <- stats::as.formula(paste("high_impact ~", paste(variable_cols, collapse = " + ")))
  svm_fit <- tryCatch(
    e1071::svm(formula, data = df, kernel = "radial", probability = TRUE, scale = TRUE),
    error = function(e) NULL
  )
  if (is.null(svm_fit)) return(m4_advanced_logistic_classifier(df))
  pred <- stats::predict(svm_fit, df, probability = TRUE)
  probs <- attr(pred, "probabilities")
  high_prob <- if (is.matrix(probs) && "high_impact" %in% colnames(probs)) probs[, "high_impact"] else as.numeric(pred == "high_impact")
  predictions <- tibble::tibble(
    source = df$source,
    observed = as.character(df$high_impact),
    predicted = as.character(pred),
    high_impact_probability = high_prob
  )
  list(
    status = "success",
    model = "svm_radial",
    predictions = predictions,
    accuracy = mean(predictions$observed == predictions$predicted, na.rm = TRUE),
    note = "In-sample diagnostic classifier; use as pattern evidence, not causal proof."
  )
}

m4_advanced_logistic_classifier <- function(df) {
  fit <- tryCatch(stats::glm(high_impact ~ log_tp + log_cpp + h_index + tp_slope + tc_slope, data = df, family = stats::binomial()), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(status = "error", model = "logistic_fallback", predictions = tibble::tibble(), accuracy = NA_real_))
  }
  prob <- suppressWarnings(as.numeric(stats::predict(fit, type = "response")))
  pred <- ifelse(prob >= 0.5, "high_impact", "lower_impact")
  predictions <- tibble::tibble(
    source = df$source,
    observed = as.character(df$high_impact),
    predicted = pred,
    high_impact_probability = prob
  )
  list(
    status = "fallback",
    model = "logistic_fallback",
    predictions = predictions,
    accuracy = mean(predictions$observed == predictions$predicted, na.rm = TRUE),
    note = "Package e1071 was unavailable; logistic classifier used as fallback."
  )
}

m4_advanced_cluster_separation <- function(clusters) {
  if (!is.data.frame(clusters) || nrow(clusters) < 4 || !"cluster" %in% names(clusters)) {
    return(list(status = "empty", mean_silhouette = NA_real_, table = tibble::tibble()))
  }
  x <- clusters[, intersect(c("pc1", "pc2"), names(clusters)), drop = FALSE]
  if (ncol(x) < 2 || length(unique(clusters$cluster)) < 2) {
    return(list(status = "empty", mean_silhouette = NA_real_, table = tibble::tibble()))
  }
  if (!requireNamespace("cluster", quietly = TRUE)) {
    return(list(status = "package_unavailable", mean_silhouette = NA_real_, table = tibble::tibble()))
  }
  sil <- tryCatch(cluster::silhouette(as.integer(factor(clusters$cluster)), stats::dist(x)), error = function(e) NULL)
  if (is.null(sil)) return(list(status = "error", mean_silhouette = NA_real_, table = tibble::tibble()))
  table <- tibble::tibble(
    source = clusters$source,
    cluster = clusters$cluster,
    silhouette_width = as.numeric(sil[, "sil_width"])
  )
  list(status = "success", mean_silhouette = mean(table$silhouette_width, na.rm = TRUE), table = table)
}

m4_advanced_pattern_story <- function(features, outliers, regression, svm, silhouette) {
  lines <- character()
  if (is.data.frame(features) && nrow(features) > 0) {
    archetype_counts <- as.data.frame(table(features$archetype), stringsAsFactors = FALSE)
    lines <- c(lines, paste0("Archetypes: ", paste(archetype_counts$Var1, archetype_counts$Freq, sep = "=", collapse = "; "), "."))
    top_engine <- features[order(-features$tp_slope, -features$tc), , drop = FALSE][1, , drop = FALSE]
    lines <- c(lines, sprintf("Strongest venue trajectory: %s (TP slope %.3f/year, TC=%s, CPP=%.2f).", top_engine$source, top_engine$tp_slope, top_engine$tc, top_engine$cpp))
  }
  if (is.data.frame(outliers) && nrow(outliers) > 0) {
    flagged <- outliers[outliers$anomaly_flag, , drop = FALSE]
    if (nrow(flagged) > 0) lines <- c(lines, sprintf("Anomalous source profile: %s had the highest anomaly score (%.2f).", flagged$source[1], flagged$anomaly_score[1]))
  }
  if (is.list(regression) && identical(regression$status, "success")) {
    lines <- c(lines, sprintf("Impact regression explained %.1f%% of log citation variance.", 100 * regression$r_squared))
  }
  if (is.list(svm) && svm$status %in% c("success", "fallback")) {
    lines <- c(lines, sprintf("%s classifier in-sample accuracy for high-impact source labels: %.1f%%.", svm$model, 100 * svm$accuracy))
  }
  if (is.list(silhouette) && identical(silhouette$status, "success")) {
    lines <- c(lines, sprintf("Mean cluster silhouette: %.3f.", silhouette$mean_silhouette))
  }
  lines
}
