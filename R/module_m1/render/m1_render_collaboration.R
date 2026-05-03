# ============================================================================
# m1_render_collaboration.R - Collaboration Index Visualization (IEEE Q1)
# ============================================================================

#' Render collaboration index plots
#'
#' @param result Output from compute_m1_collaboration
#' @param config Configuration list
#' @return List with plots
#' @export
render_m1_collaboration <- function(result, config = biblio_config()) {
  if (is.null(result) || result$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  if (!is.null(result$by_year) && nrow(result$by_year) > 0) {
    plots$collaboration_trend <- create_collaboration_trend_plot(result, config)
  }
  
  if (!is.null(result$summary)) {
    plots$collaboration_types <- create_collaboration_types_plot(result, config)
  }
  
  if (!is.null(result$authors_per_paper_dist)) {
    plots$authors_per_paper <- create_authors_per_paper_plot(result, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create collaboration trend over time plot
#' @keywords internal
create_collaboration_trend_plot <- function(result, config) {
  df <- result$by_year
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year)) +
    ggplot2::geom_line(ggplot2::aes(y = collaboration_rate), color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = collaboration_rate), color = "#0072BD", size = 2) +
    ggplot2::geom_smooth(ggplot2::aes(y = collaboration_rate), method = "loess",
                        se = FALSE, color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(
      name = "Multi-authored Papers (%)",
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Collaboration Trend Over Time",
      subtitle = sprintf("Overall Collaboration Index: %.2f", 
                         result$collaboration_index %||% NA_real_)
    )
  
  if (!is.null(result$collaboration_index) && !is.na(result$collaboration_index)) {
    ci_text <- sprintf("Collaboration Index: %.2f", result$collaboration_index)
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
      label = ci_text, size = 2.5, fontface = "italic"
    )
  }
  
  p
}

#' Create collaboration types pie/bar chart
#' @keywords internal
create_collaboration_types_plot <- function(result, config) {
  # Get single vs multi-author proportions from summary
  if (is.null(result$summary)) return(NULL)
  
  single_pct <- result$summary$single_author_pct / 100
  collab_dist <- c(scp = single_pct, mcp = 1 - single_pct)
  
  type_df <- data.frame(
    type = names(collab_dist),
    proportion = as.numeric(collab_dist),
    stringsAsFactors = FALSE
  )
  
  type_labels <- c(
    scp = "Single Author (SCP)",
    mcp = "Multiple Authors (MCP)"
  )
  type_df$type_label <- type_labels[type_df$type]
  type_df$type_label <- factor(type_df$type_label, 
                               levels = c("Single Author (SCP)", "Multiple Authors (MCP)"))
  
  colors <- c("Single Author (SCP)" = "#A2142F", "Multiple Authors (MCP)" = "#0072BD")
  
  p <- ggplot2::ggplot(type_df, ggplot2::aes(x = "", y = proportion, fill = type_label)) +
    ggplot2::geom_col(width = 0.7, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, name = "Type") +
    ggplot2::scale_y_continuous(
      name = "Proportion",
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Document Collaboration Types",
      subtitle = sprintf("SCP: %.1f%%, MCP: %.1f%%",
                        collab_dist["scp"] * 100, collab_dist["mcp"] * 100),
      x = NULL
    )
  
  p
}

#' Create authors per paper distribution
#' @keywords internal
create_authors_per_paper_plot <- function(result, config) {
  dist <- result$authors_per_paper_dist
  
  df <- data.frame(
    n_authors = as.integer(names(dist)),
    count = as.integer(dist),
    stringsAsFactors = FALSE
  )
  
  df <- df[df$n_authors <= 20, ]
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(n_authors), y = count)) +
    ggplot2::geom_col(fill = "#77AC30", color = "black", linewidth = 0.2, width = 0.8) +
    ggplot2::scale_x_discrete(name = "Number of Authors") +
    ggplot2::scale_y_continuous(
      name = "Number of Papers",
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Authors per Paper Distribution",
      subtitle = sprintf("Mean: %.2f, Median: %.0f",
                        result$mean_authors_per_paper %||% NA_real_,
                        result$median_authors_per_paper %||% NA_real_)
    )
  
  p
}