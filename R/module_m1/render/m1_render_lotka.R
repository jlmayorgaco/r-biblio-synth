# ============================================================================
# m1_render_lotka.R - Lotka's Law Visualization (IEEE Q1)
# ============================================================================

#' Render Lotka's Law plots
#'
#' @param result Output from compute_m1_lotka
#' @param config Configuration list
#' @return List with plots
#' @export
render_m1_lotka <- function(result, config = biblio_config()) {
  if (is.null(result) || result$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  lotka <- result$lotka
  plots <- list()
  
  if (!is.null(lotka$frequencies) && nrow(lotka$frequencies) > 0) {
    plots$lotka_plot <- create_lotka_scatter_plot(lotka, config)
    plots$lotka_loglog <- create_lotka_loglog_plot(lotka, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create Lotka scatter plot (observed vs expected)
#' @keywords internal
create_lotka_scatter_plot <- function(lotka, config) {
  freq <- lotka$frequencies
  
  alpha <- lotka$alpha
  C <- lotka$C
  
  n_max <- max(freq$n_articles)
  n_seq <- 1:n_max
  
  expected_prop <- n_seq^(-alpha) / sum(n_seq^(-alpha))
  expected_authors <- expected_prop * sum(freq$n_authors)
  
  plot_data <- data.frame(
    n_articles = freq$n_articles,
    observed = freq$n_authors,
    expected = expected_authors[1:nrow(freq)]
  )
  
  plot_data_long <- reshape2::melt(
    plot_data,
    id.vars = "n_articles",
    variable.name = "type",
    value.name = "n_authors"
  )
  
  plot_data_long$type <- factor(
    plot_data_long$type,
    levels = c("observed", "expected"),
    labels = c("Observed", "Expected (Lotka)")
  )
  
  colors <- c("Observed" = "#0072BD", "Expected (Lotka)" = "#D95319")
  
  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = n_articles, y = n_authors, color = type)) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::geom_line(linewidth = 0.5, alpha = 0.5) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::scale_x_continuous(name = "Number of Articles (n)", trans = "log1p") +
    ggplot2::scale_y_continuous(name = "Number of Authors", trans = "log1p") +
    ieee_theme_scatter() +
    ggplot2::labs(
      title = "Lotka's Law - Author Productivity Distribution",
      subtitle = sprintf("Power-law exponent: alpha = %.2f (classical = 2.0)", alpha)
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  if (!is.na(lotka$gof_pvalue)) {
    annotation_text <- sprintf("K-S test p-value = %.4f", lotka$gof_pvalue)
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
      label = annotation_text, size = 2.5, fontface = "italic"
    )
  }
  
  p
}

#' Create Lotka log-log plot
#' @keywords internal
create_lotka_loglog_plot <- function(lotka, config) {
  freq <- lotka$frequencies
  
  alpha <- lotka$alpha
  
  freq_filtered <- freq[freq$n_authors > 0 & freq$n_articles > 0, ]
  
  if (nrow(freq_filtered) < 2) return(NULL)
  
  freq_filtered$log_n <- log10(freq_filtered$n_articles)
  freq_filtered$log_authors <- log10(freq_filtered$n_authors)
  
  model <- lm(log_authors ~ log_n, data = freq_filtered)
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  fit_alpha <- -slope
  
  x_range <- range(freq_filtered$log_n)
  x_pred <- seq(x_range[1], x_range[2], length.out = 100)
  y_pred <- intercept + slope * x_pred
  
  pred_data <- data.frame(x = x_pred, y = y_pred)
  
  p <- ggplot2::ggplot(freq_filtered, ggplot2::aes(x = log_n, y = log_authors)) +
    ggplot2::geom_point(size = 3, color = "#0072BD", alpha = 0.8) +
    ggplot2::geom_line(data = pred_data, ggplot2::aes(x = x, y = y),
                       color = "#D95319", linewidth = 0.8, linetype = "dashed") +
    ggplot2::scale_x_continuous(name = expression(log[10]~"(Number of Articles)")) +
    ggplot2::scale_y_continuous(name = expression(log[10]~"(Number of Authors)")) +
    ieee_theme_scatter() +
    ggplot2::labs(
      title = "Lotka's Law - Log-Log Fit",
      subtitle = sprintf("Fitted alpha = %.2f (slope = %.2f)", fit_alpha, slope)
    )
  
  equation_text <- sprintf("log(N) = %.2f + (%.2f) * log(n)", intercept, slope)
  p <- p + ggplot2::annotate(
    "text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
    label = equation_text, size = 2.5, fontface = "italic"
  )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b