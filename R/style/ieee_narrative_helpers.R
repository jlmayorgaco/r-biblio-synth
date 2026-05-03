# ============================================================================
# ieee_narrative_helpers.R - Journal-ready narrative evidence plots
# ============================================================================

ieee_safe_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  value <- suppressWarnings(as.numeric(x[[1]]))
  if (length(value) == 0 || !is.finite(value)) default else value
}

ieee_clamp01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  pmax(0, pmin(1, x))
}

ieee_score_label <- function(score) {
  score <- ieee_safe_num(score)
  if (!is.finite(score)) return("not estimable")
  if (score >= 0.75) "high"
  else if (score >= 0.50) "moderate-high"
  else if (score >= 0.25) "moderate"
  else "low"
}

ieee_format_value <- function(value, suffix = "", digits = 2) {
  value <- ieee_safe_num(value)
  if (!is.finite(value)) return("NA")
  if (identical(suffix, "%")) {
    return(sprintf(paste0("%.", digits, "f%%"), value))
  }
  paste0(format(round(value, digits), trim = TRUE, nsmall = digits), suffix)
}

ieee_pick_num <- function(x, paths, default = NA_real_) {
  for (path in paths) {
    current <- x
    ok <- TRUE
    for (nm in path) {
      if (!is.list(current) || is.null(current[[nm]])) {
        ok <- FALSE
        break
      }
      current <- current[[nm]]
    }
    if (ok) {
      value <- ieee_safe_num(current, default = NA_real_)
      if (is.finite(value)) return(value)
    }
  }
  default
}

ieee_metric_row <- function(module, dimension, metric, value, score = NULL,
                            units = "", digits = 2, interpretation = NULL) {
  value <- ieee_safe_num(value)
  score <- if (is.null(score)) value else ieee_safe_num(score)
  data.frame(
    module = module,
    dimension = dimension,
    metric = metric,
    value = value,
    score = ieee_clamp01(score),
    display = ieee_format_value(value, units, digits),
    signal = ieee_score_label(score),
    interpretation = as.character(interpretation %||% ieee_score_label(score)),
    stringsAsFactors = FALSE
  )
}

ieee_bind_metric_rows <- function(...) {
  rows <- list(...)
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, rows)
  if (length(rows) == 0) {
    return(data.frame(
      module = character(), dimension = character(), metric = character(),
      value = numeric(), score = numeric(), display = character(),
      signal = character(), interpretation = character(),
      stringsAsFactors = FALSE
    ))
  }
  dplyr::bind_rows(rows)
}

ieee_metric_dashboard <- function(metrics,
                                  title,
                                  subtitle = NULL,
                                  caption = NULL,
                                  layout = "full") {
  if (!is.data.frame(metrics) || nrow(metrics) == 0) {
    return(ieee_no_data_plot(
      title = title,
      message = "Narrative metrics were not estimable from the available module outputs.",
      layout = layout
    ))
  }

  plot_df <- metrics
  plot_df$score <- ieee_clamp01(plot_df$score)
  plot_df <- plot_df[is.finite(plot_df$score), , drop = FALSE]
  if (nrow(plot_df) == 0) {
    return(ieee_no_data_plot(
      title = title,
      message = "Narrative metrics were available, but none had a finite normalized score.",
      layout = layout
    ))
  }

  plot_df$metric_label <- paste0(plot_df$metric, "  (", plot_df$display, ")")
  plot_df$metric_label <- factor(plot_df$metric_label, levels = rev(unique(plot_df$metric_label)))
  plot_df$dimension <- factor(plot_df$dimension, levels = unique(plot_df$dimension))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = score, y = metric_label, fill = dimension)) +
    ggplot2::geom_col(width = 0.62, color = "#222222", linewidth = 0.18) +
    ggplot2::geom_vline(xintercept = c(0.25, 0.5, 0.75), color = "#BDBDBD", linewidth = 0.18, linetype = "dotted") +
    ggplot2::geom_text(
      ggplot2::aes(label = signal),
      hjust = -0.05,
      size = 2.35,
      color = "#222222"
    ) +
    ggplot2::facet_grid(dimension ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.08),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", ".25", ".50", ".75", "1.0"),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(plot_df$dimension)), "primary")) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Normalized evidence intensity",
      y = NULL,
      caption = caption
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(
      legend.position = "none",
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
      axis.text.y = ggplot2::element_text(size = 7.1),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  ieee_mark_plot_layout(p, layout)
}

ieee_metric_signal_plot <- function(metrics,
                                    title,
                                    subtitle = NULL,
                                    caption = NULL,
                                    layout = "full") {
  if (!is.data.frame(metrics) || nrow(metrics) == 0) {
    return(ieee_no_data_plot(
      title = title,
      message = "Narrative signal map could not be estimated from the available metrics.",
      layout = layout
    ))
  }

  plot_df <- metrics
  plot_df$score <- ieee_clamp01(plot_df$score)
  plot_df <- plot_df[is.finite(plot_df$score), , drop = FALSE]
  if (nrow(plot_df) == 0) {
    return(ieee_no_data_plot(
      title = title,
      message = "No finite evidence scores were available for signal mapping.",
      layout = layout
    ))
  }

  plot_df$metric <- factor(plot_df$metric, levels = rev(unique(plot_df$metric)))
  plot_df$dimension <- factor(plot_df$dimension, levels = unique(plot_df$dimension))

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = dimension, y = metric, size = score, color = score)
  ) +
    ggplot2::geom_point(alpha = 0.92) +
    ggplot2::geom_text(
      ggplot2::aes(label = display),
      nudge_x = 0.16,
      hjust = 0,
      size = 2.25,
      color = "#222222",
      show.legend = FALSE
    ) +
    ggplot2::scale_color_gradientn(
      colors = c("#6C757D", "#4C78A8", "#F58518", "#B22222"),
      limits = c(0, 1),
      breaks = c(0.25, 0.5, 0.75),
      labels = c("moderate", "moderate-high", "high")
    ) +
    ggplot2::scale_size_continuous(range = c(2.2, 6.8), limits = c(0, 1), guide = "none") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      color = "Evidence",
      caption = caption
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
      legend.position = "bottom"
    ) +
    ggplot2::coord_cartesian(clip = "off")

  ieee_mark_plot_layout(p, layout)
}

ieee_narrative_lines <- function(metrics, max_lines = 8) {
  if (!is.data.frame(metrics) || nrow(metrics) == 0) return(character())
  metrics <- metrics[order(-metrics$score), , drop = FALSE]
  metrics <- utils::head(metrics, max_lines)
  sprintf(
    "%s / %s: %s (%s). %s",
    metrics$dimension,
    metrics$metric,
    metrics$display,
    metrics$signal,
    metrics$interpretation
  )
}
