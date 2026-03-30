# ============================================================================
# m1_render_author_indices.R - Author Impact Indices Visualization (IEEE Q1)
# ============================================================================

#' Render author impact indices plots
#'
#' @param result Output from compute_m1_author_indices
#' @param config Configuration list
#' @return List with plots
#' @export
render_m1_author_indices <- function(result, config = biblio_config()) {
  if (is.null(result) || result$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  if (!is.null(result$author_indices) && nrow(result$author_indices) > 0) {
    plots$h_index_plot <- create_h_index_plot(result, config)
    plots$g_index_plot <- create_g_index_plot(result, config)
    plots$h_vs_g <- create_h_vs_g_comparison_plot(result, config)
  }
  
  if (!is.null(result$h_index_distribution)) {
    plots$h_distribution <- create_h_index_distribution_plot(result, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create h-index bar chart for top authors
#' @keywords internal
create_h_index_plot <- function(result, config) {
  df <- result$author_indices
  
  top_n <- min(15, nrow(df))
  df_top <- head(df[order(-df$h_index), ], top_n)
  
  df_top$author <- factor(df_top$author, levels = rev(df_top$author))
  
  p <- ggplot2::ggplot(df_top, ggplot2::aes(x = author, y = h_index)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = h_index), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "h-index",
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Authors by h-index",
      subtitle = sprintf("Showing top %d authors", top_n),
      x = NULL
    )
  
  p
}

#' Create g-index bar chart for top authors
#' @keywords internal
create_g_index_plot <- function(result, config) {
  df <- result$author_indices
  
  if (!"g_index" %in% names(df)) return(NULL)
  
  top_n <- min(15, nrow(df))
  df_top <- head(df[order(-df$g_index), ], top_n)
  
  df_top$author <- factor(df_top$author, levels = rev(df_top$author))
  
  p <- ggplot2::ggplot(df_top, ggplot2::aes(x = author, y = g_index)) +
    ggplot2::geom_col(fill = "#D95319", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = g_index), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "g-index",
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Authors by g-index",
      subtitle = sprintf("Showing top %d authors", top_n),
      x = NULL
    )
  
  p
}

#' Create h-index vs g-index comparison plot
#' @keywords internal
create_h_vs_g_comparison_plot <- function(result, config) {
  df <- result$author_indices
  
  if (!"g_index" %in% names(df)) return(NULL)
  
  top_n <- min(20, nrow(df))
  df_top <- head(df[order(-df$h_index), ], top_n)
  
  df_long <- reshape2::melt(
    df_top[, c("author", "h_index", "g_index")],
    id.vars = "author",
    variable.name = "index_type",
    value.name = "value"
  )
  
  df_long$index_label <- ifelse(df_long$index_type == "h_index", "h-index", "g-index")
  df_long$index_label <- factor(df_long$index_label, levels = c("h-index", "g-index"))
  
  df_long$author <- factor(df_long$author, levels = rev(df_top$author))
  
  colors <- c("h-index" = "#0072BD", "g-index" = "#D95319")
  
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = author, y = value, fill = index_label)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7,
                     color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, name = "Index") +
    ggplot2::scale_y_continuous(name = "Index Value", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "h-index vs g-index Comparison",
      subtitle = sprintf("Top %d authors", top_n),
      x = NULL
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create h-index distribution plot
#' @keywords internal
create_h_index_distribution_plot <- function(result, config) {
  dist <- result$h_index_distribution
  
  df <- data.frame(
    h = as.integer(names(dist)),
    count = as.integer(dist),
    stringsAsFactors = FALSE
  )
  
  df <- df[df$h > 0, ]
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = h, y = count)) +
    ggplot2::geom_col(fill = "#77AC30", color = "black", linewidth = 0.2, width = 0.8) +
    ggplot2::scale_x_continuous(name = "h-index") +
    ggplot2::scale_y_continuous(
      name = "Number of Authors",
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Distribution of h-index Values",
      subtitle = sprintf("Total authors: %d", sum(df$count))
    )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b