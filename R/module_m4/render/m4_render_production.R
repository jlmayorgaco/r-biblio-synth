# ============================================================================
# m4_render_production.R - Institutional Production Visualizations
# ============================================================================

#' Render institutional production plots
#'
#' @param data Output from m4_compute_institutional_production
#' @param config Configuration list
#' @return List with plots
#' @export
render_m4_production <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error"))
  }
  
  plots <- list()
  
  # Top institutions bar chart
  if (!is.null(data$top_institutions)) {
    plots$top_institutions <- create_top_institutions_plot(data$top_institutions, config)
  }
  
  # Sector distribution pie chart
  if (!is.null(data$sector_summary)) {
    plots$sector_distribution <- create_sector_distribution_plot(data$sector_summary, config)
  }
  
  # Production inequality (Lorenz-like)
  if (!is.null(data$institutions)) {
    plots$production_inequality <- create_production_inequality_plot(data$institutions, config)
  }
  
  list(plots = plots, status = "success")
}

#' Create top institutions bar chart
#' @keywords internal
create_top_institutions_plot <- function(top_insts, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  n_show <- min(20, nrow(top_insts))
  plot_data <- top_insts[1:n_show, ]
  plot_data$institution_short <- substr(plot_data$institution_canonical, 1, 40)
  plot_data$institution_short <- factor(plot_data$institution_short, 
                                       levels = rev(plot_data$institution_short))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = institution_short, y = n_papers, fill = sector)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = sprintf("Top %d Most Productive Institutions", n_show),
      x = "Institution",
      y = "Number of Papers",
      fill = "Sector"
    ) +
    ieee_theme()
}

#' Create sector distribution pie chart
#' @keywords internal
create_sector_distribution_plot <- function(sector_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  ggplot2::ggplot(sector_data, ggplot2::aes(x = "", y = total_papers, fill = sector)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::labs(
      title = "Publication Distribution by Sector",
      fill = "Sector"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "right")
}

#' Create production inequality plot
#' @keywords internal
create_production_inequality_plot <- function(institutions, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  # Sort by production
  sorted <- institutions %>%
    dplyr::arrange(dplyr::desc(n_papers)) %>%
    dplyr::mutate(
      cumsum_papers = cumsum(n_papers),
      cumsum_pct = cumsum_papers / sum(n_papers) * 100,
      inst_pct = dplyr::row_number() / nrow(institutions) * 100
    )
  
  ggplot2::ggplot(sorted) +
    ggplot2::geom_line(ggplot2::aes(x = inst_pct, y = cumsum_pct), 
                      color = "#2166AC", size = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = "Institutional Production Concentration",
      x = "Cumulative % of Institutions",
      y = "Cumulative % of Papers"
    ) +
    ieee_theme()
}

#' Render institutional collaboration plots
#' @export
render_m4_collaboration <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error"))
  }
  
  plots <- list()
  
  # Collaboration heatmap
  if (!is.null(data$collaboration_matrix) && nrow(data$collaboration_matrix) > 1) {
    plots$collaboration_heatmap <- create_collaboration_heatmap(data$collaboration_matrix, config)
  }
  
  # Top collaboration pairs
  if (!is.null(data$collaboration_pairs) && nrow(data$collaboration_pairs) > 0) {
    plots$top_collaborations <- create_top_collaborations_plot(data$collaboration_pairs, config)
  }
  
  list(plots = plots, status = "success")
}

#' Create collaboration heatmap
#' @keywords internal
create_collaboration_heatmap <- function(collab_matrix, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!requireNamespace("reshape2", quietly = TRUE)) return(NULL)
  
  # Limit to top institutions for readability
  n_show <- min(30, nrow(collab_matrix))
  if (n_show < nrow(collab_matrix)) {
    top_idx <- order(rowSums(collab_matrix), decreasing = TRUE)[1:n_show]
    collab_matrix <- collab_matrix[top_idx, top_idx]
  }
  
  # Melt to long format
  melted <- reshape2::melt(collab_matrix)
  colnames(melted) <- c("Institution1", "Institution2", "Collaborations")
  
  ggplot2::ggplot(melted, ggplot2::aes(x = Institution1, y = Institution2, fill = Collaborations)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "#2166AC") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Institutional Collaboration Heatmap")
}

#' Create top collaborations plot
#' @keywords internal
create_top_collaborations_plot <- function(collab_pairs, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  n_show <- min(15, nrow(collab_pairs))
  plot_data <- collab_pairs[1:n_show, ]
  plot_data$pair <- paste(substr(plot_data$inst1, 1, 20), "-", 
                         substr(plot_data$inst2, 1, 20))
  plot_data$pair <- factor(plot_data$pair, levels = rev(plot_data$pair))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = pair, y = n_collaborations)) +
    ggplot2::geom_col(fill = "#67A9CF") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Top %d Institutional Collaborations", n_show),
      x = "Institution Pair",
      y = "Number of Collaborative Papers"
    ) +
    ieee_theme()
}

#' Render institutional network plots
#' @export
render_m4_networks <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error"))
  }
  
  plots <- list()
  
  # Network visualization if igraph available
  if (!is.null(data$network_metrics) && requireNamespace("igraph", quietly = TRUE)) {
    plots$network_graph <- create_network_graph_plot(data$collaboration_matrix, config)
  }
  
  list(plots = plots, status = "success")
}

#' Create network graph visualization
#' @keywords internal
create_network_graph_plot <- function(collab_matrix, config) {
  if (!requireNamespace("igraph", quietly = TRUE)) return(NULL)
  if (!requireNamespace("ggraph", quietly = TRUE)) return(NULL)
  
  # Limit size
  n_insts <- min(50, nrow(collab_matrix))
  top_idx <- order(rowSums(collab_matrix), decreasing = TRUE)[1:n_insts]
  sub_matrix <- collab_matrix[top_idx, top_idx]
  
  g <- igraph::graph_from_adjacency_matrix(sub_matrix, mode = "undirected", weighted = TRUE)
  
  ggraph::ggraph(g, layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(alpha = weight), color = "gray70") +
    ggraph::geom_node_point(ggplot2::aes(size = igraph::degree(g)), color = "#2166AC") +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Institutional Collaboration Network")
}

#' Render institutional geography plots
#' @export
render_m4_geography <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error"))
  }
  
  plots <- list()
  
  # World map if data available
  if (!is.null(data$by_country) && requireNamespace("rnaturalearth", quietly = TRUE)) {
    plots$institutions_map <- create_institutions_world_map(data$by_country, config)
  }
  
  # Country bar chart
  if (!is.null(data$by_country)) {
    plots$countries_bar <- create_countries_institutions_plot(data$by_country, config)
  }
  
  list(plots = plots, status = "success")
}

#' Create world map of institutions
#' @keywords internal
create_institutions_world_map <- function(country_data, config) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) return(NULL)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Merge with data
  world$data <- country_data$n_papers[match(world$name, country_data$country)]
  
  ggplot2::ggplot(world) +
    ggplot2::geom_sf(ggplot2::aes(fill = data)) +
    ggplot2::scale_fill_gradient(low = "white", high = "#2166AC", na.value = "gray90") +
    ggplot2::labs(title = "Global Distribution of Institutional Production", fill = "Papers")
}

#' Create countries institutions bar chart
#' @keywords internal
create_countries_institutions_plot <- function(country_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  n_show <- min(20, nrow(country_data))
  plot_data <- country_data[1:n_show, ]
  plot_data$country <- factor(plot_data$country, levels = rev(plot_data$country))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = country, y = n_institutions)) +
    ggplot2::geom_col(fill = "#67A9CF") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Top %d Countries by Number of Institutions", n_show),
      x = "Country",
      y = "Number of Institutions"
    ) +
    ieee_theme()
}