# ============================================================================
# module_m2/compute/m2_compute_eda.R - EDA for annual production
# ============================================================================

#' Compute M2 EDA (Exploratory Data Analysis)
#' @export
compute_m2_eda <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) < 2) {
    return(list(status = "error", summary = list(), anomalies = data.frame(), moving_averages = list()))
  }

  years <- as.numeric(input$Year)
  articles <- as.numeric(input$Articles)

  # Basic stats
  summary_stats <- list(
    start_year = min(years, na.rm = TRUE),
    end_year = max(years, na.rm = TRUE),
    peak_year = years[which.max(articles)],
    peak_articles = max(articles, na.rm = TRUE),
    min_year = years[which.min(articles)],
    min_articles = min(articles, na.rm = TRUE),
    mean_articles = round(mean(articles, na.rm = TRUE), 2),
    median_articles = median(articles, na.rm = TRUE),
    sd_articles = round(sd(articles, na.rm = TRUE), 2),
    total_articles = sum(articles, na.rm = TRUE)
  )

  # Anomaly detection (z-score > 3)
  mean_val <- mean(articles, na.rm = TRUE)
  sd_val <- sd(articles, na.rm = TRUE)
  if (sd_val > 0) {
    z_scores <- (articles - mean_val) / sd_val
    anomaly_idx <- which(abs(z_scores) > 3)
    anomalies <- data.frame(
      year = years[anomaly_idx],
      articles = articles[anomaly_idx],
      z_score = round(z_scores[anomaly_idx], 3)
    )
  } else {
    anomalies <- data.frame(year = integer(), articles = integer(), z_score = numeric())
  }

  # Outliers via IQR
  q1 <- quantile(articles, 0.25, na.rm = TRUE)
  q3 <- quantile(articles, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  outliers <- data.frame(
    year = years[articles < (q1 - 1.5 * iqr) | articles > (q3 + 1.5 * iqr)],
    articles = articles[articles < (q1 - 1.5 * iqr) | articles > (q3 + 1.5 * iqr)]
  )

  # Moving averages (1, 3, 5, 10)
  moving_averages <- list()
  for (w in c(1, 3, 5, 10)) {
    if (w <= length(articles)) {
      ma <- zoo::rollmean(articles, k = w, fill = NA, align = "right")
      moving_averages[[paste0("ma_", w)]] <- data.frame(year = years, articles = ma)
    }
  }

  list(
    status = "success",
    summary = summary_stats,
    anomalies = anomalies,
    outliers = outliers,
    moving_averages = moving_averages
  )
}
