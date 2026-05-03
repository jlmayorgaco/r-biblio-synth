# ============================================================================
# m3_render_temporal_dynamics.R - Temporal trend plots for M3
# ============================================================================

#' Render temporal dynamics plots
#'
#' @param data Output from \code{m3_compute_temporal_dynamics}
#' @param config Configuration list
#' @return A list with \code{status} and \code{plots}
#' @export
render_m3_temporal_dynamics <- function(data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())
  if (!is.list(data) || !identical(data$status, "success")) {
    return(stub)
  }

  plots <- list()

  share_plot <- create_m3_share_trends_plot(data$share_evolution, config)
  if (!is.null(share_plot)) {
    plots$share_trends <- share_plot
  }

  mobility_plot <- create_m3_rank_mobility_plot(data$rank_mobility, config)
  if (!is.null(mobility_plot)) {
    plots$rank_mobility <- mobility_plot
  }

  emergence_plot <- create_m3_emergence_plot(data$emergence, config)
  if (!is.null(emergence_plot)) {
    plots$emergence <- emergence_plot
  }

  if (length(plots) == 0) {
    return(stub)
  }

  list(status = "success", plots = plots)
}

#' Share trend plot
#' @keywords internal
create_m3_share_trends_plot <- function(share_evolution, config) {
  if (is.null(share_evolution) || !is.data.frame(share_evolution$share_trends) || nrow(share_evolution$share_trends) == 0) {
    return(NULL)
  }

  df <- share_evolution$share_trends
  df <- df[is.finite(df$change), , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }

  pos_df <- df[df$change > 0, , drop = FALSE]
  neg_df <- df[df$change < 0, , drop = FALSE]
  plot_df <- dplyr::bind_rows(
    utils::head(pos_df[order(-pos_df$change), , drop = FALSE], 6),
    utils::head(neg_df[order(neg_df$change), , drop = FALSE], 6)
  )
  if (nrow(plot_df) < min(8, nrow(df))) {
    remaining <- df[!df$country %in% plot_df$country, , drop = FALSE]
    remaining <- utils::head(remaining[order(abs(remaining$change), decreasing = TRUE), , drop = FALSE], 12 - nrow(plot_df))
    plot_df <- dplyr::bind_rows(plot_df, remaining)
  }

  if (nrow(plot_df) == 0) {
    plot_df <- utils::head(df[order(abs(df$change), decreasing = TRUE), , drop = FALSE], 12)
  }

  df <- plot_df
  df$country <- m3_title_case_country(df$country)
  df$trend <- factor(df$trend, levels = c("gaining", "stable", "losing"))
  df <- df[order(df$change, decreasing = TRUE), , drop = FALSE]

  first_label <- if (!is.null(share_evolution$first_window) && length(share_evolution$first_window) > 0) {
    paste0(min(share_evolution$first_window), "-", max(share_evolution$first_window))
  } else {
    "initial window"
  }
  last_label <- if (!is.null(share_evolution$last_window) && length(share_evolution$last_window) > 0) {
    paste0(min(share_evolution$last_window), "-", max(share_evolution$last_window))
  } else {
    "final window"
  }

  colors <- c("gaining" = "#59A14F", "stable" = "#9C9C9C", "losing" = "#E15759")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(country, change), y = change, fill = trend)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    ggplot2::scale_y_continuous(name = "Share change (percentage points)", labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Country share change between initial and final observation windows",
      subtitle = sprintf("Window comparison: %s vs %s; positive values indicate gain in global production share", first_label, last_label)
    ) +
    ieee_theme_wide(base_size = 8.3) +
    ggplot2::theme(legend.position = "bottom")

  ieee_mark_plot_layout(p, "full")
}

#' Rank mobility plot
#' @keywords internal
create_m3_rank_mobility_plot <- function(rank_mobility, config) {
  if (is.null(rank_mobility) || !is.list(rank_mobility$rank_changes) || !is.data.frame(rank_mobility$rank_changes$changes)) {
    return(NULL)
  }

  df <- rank_mobility$rank_changes$changes
  if (nrow(df) == 0) {
    return(NULL)
  }

  df$country <- m3_title_case_country(df$country)
  persistent_df <- df[df$first_window_output > 0 & df$last_window_output > 0, , drop = FALSE]
  if (nrow(persistent_df) >= 3) {
    df <- persistent_df
  } else {
    df <- df[df$rank_first <= 15 | df$rank_last <= 15, , drop = FALSE]
  }
  if (nrow(df) == 0) {
    return(NULL)
  }
  df <- df[order(abs(df$rank_change), decreasing = TRUE), , drop = FALSE]
  df <- head(df, min(12, nrow(df)))
  df$country <- factor(df$country, levels = rev(df$country))
  df$direction <- ifelse(df$rank_change > 0, "improved", ifelse(df$rank_change < 0, "declined", "stable"))

  first_label <- if (!is.null(rank_mobility$first_window) && length(rank_mobility$first_window) > 0) {
    paste0(min(rank_mobility$first_window), "-", max(rank_mobility$first_window))
  } else {
    "initial window"
  }
  last_label <- if (!is.null(rank_mobility$last_window) && length(rank_mobility$last_window) > 0) {
    paste0(min(rank_mobility$last_window), "-", max(rank_mobility$last_window))
  } else {
    "final window"
  }

  colors <- c("improved" = "#59A14F", "stable" = "#9C9C9C", "declined" = "#E15759")

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_segment(
      ggplot2::aes(x = rank_first, xend = rank_last, y = country, yend = country, color = direction),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = rank_first, y = country),
      color = "black",
      fill = "white",
      shape = 21,
      size = 2.3,
      stroke = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = rank_last, y = country, color = direction),
      size = 2.6
    ) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::scale_x_reverse(name = "Rank (1 = leading country)", breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::labs(
      title = "Rank mobility between initial and final observation windows",
      subtitle = sprintf("Open marker = %s average rank; filled marker = %s average rank", first_label, last_label)
    ) +
    ieee_theme_wide(base_size = 8.2) +
    ggplot2::theme(legend.position = "bottom")

  ieee_mark_plot_layout(p, "full")
}

#' Emergence plot
#' @keywords internal
create_m3_emergence_plot <- function(emergence, config) {
  if (is.null(emergence) || is.null(emergence$emergence_counts) || length(emergence$emergence_counts) == 0) {
    return(NULL)
  }

  df <- data.frame(
    year = as.numeric(names(emergence$emergence_counts)),
    new_countries = as.numeric(emergence$emergence_counts),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$year) & is.finite(df$new_countries), , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = new_countries)) +
    ggplot2::geom_col(fill = "#4E79A7", color = "black", linewidth = 0.2, width = 0.8) +
    ggplot2::geom_line(group = 1, color = "black", linewidth = 0.45) +
    ggplot2::scale_x_continuous(name = "Year", breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::scale_y_continuous(name = "New active countries", expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = "Entry of new countries into the field",
      subtitle = "Counts the first observed active year per country"
    ) +
    ieee_theme(base_size = 8.2)

  ieee_mark_plot_layout(p, "single")
}
