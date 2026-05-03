# ============================================================================
# m1_render_author_career.R - Author Career Visualization
# ============================================================================

#' Render author career results
#'
#' @param data Output from compute_m1_author_career
#' @param config Configuration list
#' @return List with author career plots
#' @export
render_m1_author_career <- function(data, config = biblio_config()) {
  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(list(plots = list(), status = data$status %||% "error"))
  }

  plots <- list()

  if (!is.null(data$career_df) && nrow(data$career_df) > 0) {
    plots$career_length <- create_career_length_plot(data, config)
    plots$productivity <- create_productivity_distribution_plot(data, config)
    plots$h_index <- create_h_index_distribution_plot(data, config)
    plots$career_stage <- create_career_stage_plot(data, config)
  }

  if (!is.null(data$trajectories) && nrow(data$trajectories) > 0) {
    plots$trajectory <- create_career_trajectory_plot(data, config)
  }

  list(
    plots = plots,
    status = "success"
  )
}

#' Create career length distribution plot
#' @keywords internal
create_career_length_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  metrics <- data$career_df
  if (is.null(metrics) || nrow(metrics) == 0) return(NULL)

  ggplot2::ggplot(metrics, ggplot2::aes(x = career_length)) +
    ggplot2::geom_histogram(bins = 30, fill = "#2166AC", alpha = 0.7) +
    ggplot2::labs(
      title = "Distribution of Author Career Lengths",
      x = "Career Length (Years)",
      y = "Number of Authors"
    ) +
    ieee_theme()
}

#' Create productivity distribution plot
#' @keywords internal
create_productivity_distribution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  metrics <- data$career_df
  if (is.null(metrics) || nrow(metrics) == 0) return(NULL)

  positive <- metrics[!is.na(metrics$total_papers) & metrics$total_papers > 0, , drop = FALSE]
  if (nrow(positive) == 0) return(NULL)

  ggplot2::ggplot(positive, ggplot2::aes(x = total_papers)) +
    ggplot2::geom_histogram(bins = 40, fill = "#67A9CF", alpha = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Author Productivity Distribution",
      x = "Total Papers (log10 scale)",
      y = "Number of Authors"
    ) +
    ieee_theme()
}

#' Create H-index distribution plot
#' @keywords internal
create_h_index_distribution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  metrics <- data$career_df
  if (is.null(metrics) || nrow(metrics) == 0) return(NULL)

  top_n <- config$top_n_authors %||% 20
  top_authors <- metrics[order(-metrics$h_index, -metrics$total_citations), , drop = FALSE]
  top_authors <- top_authors[seq_len(min(top_n, nrow(top_authors))), , drop = FALSE]
  label_col <- if ("display_author" %in% names(top_authors)) "display_author" else "author"
  top_authors$author_label <- top_authors[[label_col]]
  top_authors$author_label <- factor(top_authors$author_label, levels = rev(top_authors$author_label))

  ggplot2::ggplot(top_authors, ggplot2::aes(x = author_label, y = h_index)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", nrow(top_authors), "Authors by H-index"),
      x = "Author",
      y = "H-index"
    ) +
    ieee_theme()
}

#' Create career stage plot
#' @keywords internal
create_career_stage_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  metrics <- data$career_df
  if (is.null(metrics) || !"career_stage" %in% names(metrics)) return(NULL)

  stage_table <- table(metrics$career_stage, useNA = "no")
  plot_data <- data.frame(
    career_stage = names(stage_table),
    n_authors = as.numeric(stage_table),
    stringsAsFactors = FALSE
  )
  plot_data <- plot_data[plot_data$career_stage != "", , drop = FALSE]
  if (nrow(plot_data) == 0) return(NULL)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = career_stage, y = n_authors, fill = career_stage)) +
    ggplot2::geom_col(show.legend = FALSE, alpha = 0.8) +
    ggplot2::labs(
      title = "Career Stage Distribution",
      x = "Career Stage",
      y = "Authors"
    ) +
    ieee_theme()
}

#' Create career trajectory plot
#' @keywords internal
create_career_trajectory_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  trajectories <- data$trajectories
  if (is.null(trajectories) || nrow(trajectories) == 0) return(NULL)

  trajectories$author_label <- vapply(trajectories$author, m1_author_display_label, character(1))

  ggplot2::ggplot(trajectories, ggplot2::aes(x = year, y = cumulative_citations, color = author_label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::labs(
      title = "Career Trajectories of Leading Authors",
      x = "Year",
      y = "Cumulative Citations",
      color = "Author"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ieee_theme()
}

#' Build author career table
#' @export
build_m1_author_career_table <- function(data, config = biblio_config()) {
  empty <- list(
    status = "stub",
    table = tibble::tibble(),
    summary = list()
  )

  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(empty)
  }

  metrics <- data$career_df
  if (is.null(metrics) || nrow(metrics) == 0) {
    return(empty)
  }

  top_n <- config$top_n_authors %||% 100
  metrics <- metrics[order(-metrics$h_index, -metrics$total_citations), , drop = FALSE]
  metrics <- metrics[seq_len(min(top_n, nrow(metrics))), , drop = FALSE]

  if ("display_author" %in% names(metrics)) {
    metrics$author <- metrics$display_author
  }

  keep <- c("author", "total_papers", "total_citations", "h_index", "g_index", "career_length", "career_stage", "m_index")
  keep <- keep[keep %in% names(metrics)]

  list(
    status = "success",
    table = tibble::as_tibble(metrics[, keep, drop = FALSE]),
    summary = list(
      n_authors = data$n_authors %||% nrow(data$career_df),
      rising_stars = nrow(data$rising_stars %||% data.frame()),
      established_stars = nrow(data$established_stars %||% data.frame())
    )
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
