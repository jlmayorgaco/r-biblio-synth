# ============================================================================
# m3_render_experiments.R - Quadrant and exploratory plots for M3
# ============================================================================

#' Render exploratory experiment plots
#'
#' @param experiments_data Output from \code{m3_compute_experiments}
#' @param config A configuration list
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_experiments <- function(experiments_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())
  if (!is.list(experiments_data)) return(stub)

  plots <- list()

  quad <- experiments_data$quadrant
  if (is.data.frame(quad) && nrow(quad) >= 2) {
    plots$quadrant <- create_m3_quadrant_plot(quad, config)
  }

  conc <- experiments_data$concentration
  if (is.data.frame(conc) && nrow(conc) >= 2) {
    plots$concentration_sensitivity <- create_m3_concentration_sensitivity_plot(conc, config)
  }

  mom <- experiments_data$momentum
  if (is.data.frame(mom) && nrow(mom) >= 2) {
    plots$momentum_bar <- create_m3_momentum_plot(mom, config)
  }

  if (length(plots) == 0) return(stub)
  list(status = "success", plots = plots)
}

#' Productivity-impact quadrant plot
#' @keywords internal
create_m3_quadrant_plot <- function(quad, config) {
  quad <- quad[is.finite(quad$article_count) & is.finite(quad$avg_citations), , drop = FALSE]
  if (nrow(quad) < 2) {
    return(NULL)
  }

  quad$country <- m3_title_case_country(quad$country)
  quad$quadrant <- factor(
    quad$quadrant,
    levels = c(
      "High-Output / High-Impact",
      "High-Output / Low-Impact",
      "Low-Output / High-Impact",
      "Low-Output / Low-Impact"
    )
  )

  x_med <- unique(stats::na.omit(quad$output_median))
  y_med <- unique(stats::na.omit(quad$impact_median))
  x_med <- if (length(x_med) > 0) x_med[1] else stats::median(quad$article_count, na.rm = TRUE)
  y_med <- if (length(y_med) > 0) y_med[1] else stats::median(quad$avg_citations, na.rm = TRUE)

  top_labels <- quad[order(-quad$total_citations, -quad$article_count), , drop = FALSE]
  top_labels <- head(top_labels, min(12, nrow(top_labels)))

  x_left <- stats::quantile(quad$article_count, 0.18, na.rm = TRUE, names = FALSE)
  x_right <- stats::quantile(quad$article_count, 0.82, na.rm = TRUE, names = FALSE)
  y_low <- stats::quantile(quad$avg_citations, 0.18, na.rm = TRUE, names = FALSE)
  y_high <- stats::quantile(quad$avg_citations, 0.82, na.rm = TRUE, names = FALSE)
  quadrant_labels <- data.frame(
    x = c(x_right, x_right, x_left, x_left),
    y = c(y_high, y_low, y_high, y_low),
    label = c("Leaders", "Scale, lower impact", "Niche high impact", "Emerging"),
    hjust = c(1, 1, 0, 0),
    stringsAsFactors = FALSE
  )

  palette <- c(
    "High-Output / High-Impact" = "#1F77B4",
    "High-Output / Low-Impact" = "#E15759",
    "Low-Output / High-Impact" = "#59A14F",
    "Low-Output / Low-Impact" = "#9C9C9C"
  )

  p <- ggplot2::ggplot(
    quad,
    ggplot2::aes(
      x = article_count,
      y = avg_citations,
      color = quadrant,
      size = total_citations
    )
  ) +
    ggplot2::geom_point(alpha = 0.9) +
    ggplot2::geom_vline(xintercept = x_med, linetype = "22", color = "#666666", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = y_med, linetype = "22", color = "#666666", linewidth = 0.5) +
    ggplot2::scale_color_manual(values = palette, name = NULL) +
    ggplot2::scale_size_continuous(name = "Total citations", range = c(2.2, 8)) +
    ggplot2::scale_x_continuous(
      name = "Publications",
      trans = scales::pseudo_log_trans(base = 10),
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.18, 0.08))
    ) +
    ggplot2::scale_y_continuous(
      name = "Average citations per article",
      labels = scales::label_number(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.05, 0.1))
    ) +
    ggplot2::labs(
      title = "Country productivity-impact quadrant",
      subtitle = "Vertical and horizontal guides mark the median output and median citation impact",
      caption = "Bubble size encodes total citations."
    ) +
    ieee_theme_wide(base_size = 8.4) +
    ggplot2::theme(legend.position = "bottom")

  p <- tryCatch(
    p + ggrepel::geom_text_repel(
      data = top_labels,
      ggplot2::aes(label = country),
      size = 2.6,
      min.segment.length = 0,
      box.padding = 0.18,
      point.padding = 0.12,
      max.overlaps = 20,
      show.legend = FALSE
    ),
    error = function(e) {
      p + ggplot2::geom_text(
        data = top_labels,
        ggplot2::aes(label = country),
        size = 2.4,
        vjust = -0.7,
        show.legend = FALSE
      )
    }
  )

  p <- p +
    ggplot2::geom_label(
      data = quadrant_labels,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust),
      inherit.aes = FALSE,
      size = 2.5,
      color = "#666666",
      fontface = "bold",
      label.size = 0.15,
      fill = grDevices::adjustcolor("white", alpha.f = 0.82)
    )

  ieee_mark_plot_layout(p, "full")
}

#' Concentration sensitivity plot
#' @keywords internal
create_m3_concentration_sensitivity_plot <- function(conc, config) {
  p <- ggplot2::ggplot(conc, ggplot2::aes(x = top_k, y = gini)) +
    ggplot2::geom_line(color = "black", linewidth = 0.7) +
    ggplot2::geom_point(color = "black", fill = "white", shape = 21, size = 2, stroke = 0.4) +
    ggplot2::scale_x_continuous(name = "Number of top countries included", breaks = conc$top_k) +
    ggplot2::scale_y_continuous(name = "Gini coefficient", limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Concentration sensitivity to top-k truncation",
      subtitle = "Tracks how inequality changes as the country set is truncated"
    ) +
    ieee_theme(base_size = 8.2)

  ieee_mark_plot_layout(p, "single")
}

#' Momentum score plot
#' @keywords internal
create_m3_momentum_plot <- function(mom, config) {
  mom_plot <- mom %>%
    dplyr::filter(!is.na(momentum_score)) %>%
    dplyr::arrange(dplyr::desc(momentum_score))

  if (nrow(mom_plot) < 2) {
    return(NULL)
  }

  mom_plot <- utils::head(mom_plot, min(15L, nrow(mom_plot))) %>%
    dplyr::mutate(
      country = m3_title_case_country(country),
      label_clean = substr(trimws(country), 1, 28),
      phase = dplyr::case_when(
        momentum_score >= 1.15 ~ "accelerating",
        momentum_score <= 0.85 ~ "cooling",
        TRUE ~ "stable"
      )
    )

  colors <- c("accelerating" = "#59A14F", "stable" = "#9C9C9C", "cooling" = "#E15759")

  p <- ggplot2::ggplot(
    mom_plot,
    ggplot2::aes(x = reorder(label_clean, momentum_score), y = momentum_score, fill = phase)
  ) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 1, linetype = "22", color = "#666666", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    ggplot2::scale_y_continuous(name = "Momentum score", expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = "Country momentum score",
      subtitle = "Recent 3-year average divided by the prior period average"
    ) +
    ieee_theme(base_size = 8.2) +
    ggplot2::theme(legend.position = "bottom")

  ieee_mark_plot_layout(p, "single")
}

#' Title-case country labels
#' @keywords internal
m3_title_case_country <- function(x) {
  x <- trimws(as.character(x))
  x[nchar(x) == 0] <- NA_character_
  if (exists("m3_normalize_country_names", mode = "function")) {
    x <- m3_normalize_country_names(x)
  }
  out <- gsub("\\s+", " ", x)
  tools::toTitleCase(tolower(out))
}
