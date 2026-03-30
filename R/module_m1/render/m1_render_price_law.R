# ============================================================================
# m1_render_price_law.R - Price's Law Visualization (IEEE Q1)
# ============================================================================
# Price's Law: Square root of authors produce half of the literature
# Price Index: Proportion of papers by core authors

#' Render Price's Law plots
#'
#' @param result Output from compute_m1_price_law
#' @param config Configuration list
#' @return List with plots
#' @export
render_m1_price_law <- function(result, config = biblio_config()) {
  if (is.null(result) || result$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # Access author_distribution from price_law sub-object
  author_dist <- result$price_law$author_distribution
  if (!is.null(author_dist) && nrow(author_dist) > 0) {
    plots$price_law <- create_price_law_plot(result, config)
  }
  
  if (!is.null(result$author_concentration)) {
    plots$core_peripheral <- create_core_peripheral_plot(result, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create Price's Law cumulative contribution plot
#' @keywords internal
create_price_law_plot <- function(result, config) {
  author_dist <- result$price_law$author_distribution
  
  if (is.null(author_dist) || nrow(author_dist) == 0) return(NULL)
  
  # Calculate cumulative percentages
  author_dist <- author_dist[order(-author_dist$n_papers), ]
  author_dist$author_rank <- seq_len(nrow(author_dist))
  author_dist$cumulative_articles <- cumsum(author_dist$n_papers)
  total_papers <- sum(author_dist$n_papers)
  author_dist$cumulative_articles_pct <- author_dist$cumulative_articles / total_papers
  
  n_authors <- nrow(author_dist)
  sqrt_n <- sqrt(n_authors)
  half_articles <- total_papers / 2
  
  p <- ggplot2::ggplot(author_dist, ggplot2::aes(x = author_rank, y = cumulative_articles_pct)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = 0, ymax = cumulative_articles_pct),
      fill = "#0072BD", alpha = 0.1
    ) +
    ggplot2::geom_hline(yintercept = 0.5, color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = sqrt_n, color = "#77AC30", linetype = "dotted", linewidth = 0.5) +
    ggplot2::annotate(
      "text", x = sqrt_n, y = 0.05, hjust = -0.1, vjust = 0,
      label = sprintf("sqrt(n) = %.0f", sqrt_n), size = 2.5, color = "#77AC30"
    ) +
    ggplot2::scale_x_continuous(
      name = "Author Rank",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      name = "Cumulative Articles (%)",
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Price's Law - Author Contribution Concentration",
      subtitle = sprintf("Top sqrt(n)=%.0f authors: %.1f%% of papers", 
                        sqrt_n, result$price_law$actual_proportion * 100)
    )
  
  p
}

#' Create core vs peripheral authors bar plot
#' @keywords internal
create_core_peripheral_plot <- function(result, config) {
  conc <- result$author_concentration
  
  if (is.null(conc) || is.null(conc$top_10_pct_share)) return(NULL)
  
  df <- data.frame(
    category = c("Top 10%", "Bottom 90%"),
    proportion = c(conc$top_10_pct_share, 1 - conc$top_10_pct_share),
    stringsAsFactors = FALSE
  )
  
  df$category <- factor(df$category, levels = c("Bottom 90%", "Top 10%"))
  
  colors <- c("Top 10%" = "#0072BD", "Bottom 90%" = "#A2142F")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = proportion, fill = category)) +
    ggplot2::geom_col(width = 0.7, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, name = "Category") +
    ggplot2::scale_y_continuous(
      name = "Proportion of Articles",
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Author Concentration",
      subtitle = sprintf("Gini: %.3f | %s", conc$gini %||% NA_real_, conc$concentration_type %||% ""),
      x = NULL
    )
  
  p
}

#' Create contribution pie chart
#' @keywords internal
create_contribution_pie <- function(result, config) {
  author_dist <- result$price_law$author_distribution
  
  if (is.null(author_dist) || nrow(author_dist) == 0) return(NULL)
  
  top_n <- min(10, nrow(author_dist))
  top_contrib <- head(author_dist[order(-author_dist$n_papers), ], top_n)
  other <- sum(author_dist$n_papers) - sum(top_contrib$n_papers)
  
  df <- data.frame(
    author = c(top_contrib$author, "Other"),
    articles = c(top_contrib$n_papers, other),
    stringsAsFactors = FALSE
  )
  
  df$author <- factor(df$author, levels = df$author)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = articles, fill = author)) +
    ggplot2::geom_col(position = "stack", color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_discrete(name = "Author") +
    ieee_theme() +
    ggplot2::labs(
      title = "Top Authors by Contribution",
      subtitle = sprintf("Top %d authors + Others", top_n),
      y = "Articles"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b