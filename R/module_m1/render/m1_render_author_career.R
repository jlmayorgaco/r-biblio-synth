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
  if (is.null(data) || data$status == "error") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$career_df) && nrow(data$career_df) > 0) {
    plots$career_length <- create_career_length_plot(data, config)
    plots$productivity <- create_productivity_distribution_plot(data, config)
    plots$h_index <- create_h_index_distribution_plot(data, config)
    plots$trajectory <- create_career_trajectory_plot(data, config)
  }
  
  if (!is.null(data$collaboration_network)) {
    plots$network <- create_author_network_plot(data, config)
  }
  
  if (!is.null(data$centrality)) {
    plots$centrality <- create_centrality_plot(data, config)
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
  
  ggplot2::ggplot(metrics, ggplot2::aes(x = total_papers)) +
    ggplot2::geom_histogram(bins = 50, fill = "#67A9CF", alpha = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Author Productivity Distribution (Log Scale)",
      x = "Total Papers (log10)",
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
  
  top_n <- config$top_n_authors %||% 50
  top_authors <- metrics[order(-metrics$h_index), ][1:min(top_n, nrow(metrics)), ]
  
  top_authors$author <- factor(top_authors$author, levels = top_authors$author[order(top_authors$h_index)])
  
  ggplot2::ggplot(top_authors, ggplot2::aes(x = author, y = h_index)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", top_n, "Authors by H-index"),
      x = "Author",
      y = "H-index"
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
  
  top_n <- config$trajectory_top_n %||% 10
  top_authors <- unique(trajectories$author)[1:min(top_n, length(unique(trajectories$author)))]
  
  traj_subset <- trajectories[trajectories$author %in% top_authors, ]
  
  ggplot2::ggplot(traj_subset, ggplot2::aes(x = year, y = cumulative_citations, color = author)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Career Trajectories of Top Authors",
      x = "Year",
      y = "Cumulative Citations"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ieee_theme()
}

#' Create author collaboration network plot
#' @keywords internal
create_author_network_plot <- function(data, config) {
  if (!requireNamespace("igraph", quietly = TRUE) || 
      !requireNamespace("ggraph", quietly = TRUE)) {
    return(NULL)
  }
  
  network <- data$collaboration_network
  if (is.null(network)) return(NULL)
  
  g <- network$graph
  if (is.null(g)) return(NULL)
  
  ggraph::ggraph(g, layout = "fr") +
    ggraph::geom_edge_link(alpha = 0.3) +
    ggraph::geom_node_point(ggplot2::aes(size = degree, color = betweenness)) +
    ggraph::scale_color_viridis_c() +
    ggplot2::labs(title = "Author Collaboration Network") +
    ggraph::theme_graph()
}

#' Create centrality plot
#' @keywords internal
create_centrality_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  centrality <- data$centrality
  if (is.null(centrality) || nrow(centrality) == 0) return(NULL)
  
  top_n <- config$top_n_authors %||% 20
  top_centrality <- centrality[order(-centrality$betweenness), ][1:min(top_n, nrow(centrality)), ]
  
  top_centrality$author <- factor(top_centrality$author, levels = top_centrality$author[order(top_centrality$betweenness)])
  
  ggplot2::ggplot(top_centrality, ggplot2::aes(x = author, y = betweenness)) +
    ggplot2::geom_col(fill = "#B2182B", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Authors by Betweenness Centrality",
      x = "Author",
      y = "Betweenness Centrality"
    ) +
    ieee_theme()
}

#' Build author career table
#' @export
build_m1_author_career_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status == "error") {
    return(data.frame(
      author = character(),
      total_papers = integer(),
      total_citations = integer(),
      h_index = integer(),
      g_index = integer(),
      career_length = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  metrics <- data$career_metrics
  if (is.null(metrics) || nrow(metrics) == 0) {
    return(data.frame(
      author = character(),
      total_papers = integer(),
      total_citations = integer(),
      h_index = integer(),
      g_index = integer(),
      career_length = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  top_n <- config$top_n_authors %||% 100
  metrics[order(-metrics$h_index), ][1:min(top_n, nrow(metrics)), 
    c("author", "total_papers", "total_citations", "h_index", "g_index", "career_length")]
}

`%||%` <- function(a, b) if (!is.null(a)) a else b