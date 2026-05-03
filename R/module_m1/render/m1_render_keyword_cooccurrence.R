# ============================================================================
# m1_render_keyword_cooccurrence.R - Keyword Network Visualization (IEEE Q1)
# ============================================================================
# Creates network visualizations for keyword co-occurrence analysis
# ============================================================================

#' Render keyword co-occurrence network plots
#'
#' @param data Output from compute_m1_keyword_cooccurrence
#' @param config Configuration list
#' @return List with plots
#' @export
render_m1_keyword_cooccurrence <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # 1. Network visualization
  plots$network <- create_network_plot(data, config)
  
  # 2. Keyword frequency bar chart
  plots$frequency <- create_keyword_frequency_plot(data, config)
  
  # 3. Co-occurrence heatmap (top keywords)
  plots$heatmap <- create_cooccurrence_heatmap(data, config)
  
  # 4. Centrality comparison
  plots$centrality <- create_centrality_plot(data, config)
  
  # 5. Community structure
  if (!is.null(data$communities) && length(data$communities$membership) > 0) {
    plots$communities <- create_community_plot(data, config)
  }
  
  # 6. Top co-occurrence pairs
  plots$top_pairs <- create_top_pairs_plot(data, config)
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create network visualization
#' @keywords internal
create_network_plot <- function(data, config) {
  if (is.null(data$edgelist) || nrow(data$edgelist) == 0) return(NULL)
  
  edgelist <- data$edgelist
  top_n <- (config$top_n_keywords %||% 50)
  
  # Filter to top keywords if needed
  if (!is.null(data$adjacency_top)) {
    nodes <- rownames(data$adjacency_top)
    edgelist <- edgelist[edgelist$from %in% nodes & edgelist$to %in% nodes, ]
  }
  
  if (nrow(edgelist) == 0) return(NULL)
  
  # Create node data frame
  nodes <- unique(c(edgelist$from, edgelist$to))
  node_freq <- data$keyword_freq[nodes]
  
  nodes_df <- data.frame(
    keyword = nodes,
    freq = as.numeric(node_freq),
    stringsAsFactors = FALSE
  )
  
  # Add community membership if available
  if (!is.null(data$communities) && !is.null(data$communities$membership)) {
    nodes_df$community <- data$communities$membership[nodes]
  } else {
    nodes_df$community <- 1
  }
  
  # Calculate degree for node size
  nodes_df$degree <- sapply(nodes_df$keyword, function(kw) {
    sum(edgelist$from == kw | edgelist$to == kw)
  })
  
  # Sort by frequency
  nodes_df <- nodes_df[order(-nodes_df$freq), ]
  nodes_df$keyword <- factor(nodes_df$keyword, levels = nodes_df$keyword)
  
  # Create plot using ggraph-like approach with ggplot2
  # Simple circular layout
  n <- nrow(nodes_df)
  angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
  nodes_df$x <- cos(angles) * nodes_df$degree / max(nodes_df$degree)
  nodes_df$y <- sin(angles) * nodes_df$degree / max(nodes_df$degree)
  
  # Create edge data frame
  edges_df <- edgelist
  edges_df$x1 <- nodes_df$x[match(edges_df$from, nodes_df$keyword)]
  edges_df$y1 <- nodes_df$y[match(edges_df$from, nodes_df$keyword)]
  edges_df$x2 <- nodes_df$x[match(edges_df$to, nodes_df$keyword)]
  edges_df$y2 <- nodes_df$y[match(edges_df$to, nodes_df$keyword)]
  
  colors <- get_ieee_palette(max(nodes_df$community))
  
  p <- ggplot2::ggplot() +
    # Edges
    ggplot2::geom_segment(
      data = edges_df,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = weight),
      alpha = 0.3,
      color = "#666666"
    ) +
    # Nodes
    ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = x, y = y, size = freq, color = factor(community)),
      alpha = 0.8
    ) +
    # Labels
    ggrepel::geom_text_repel(
      data = nodes_df[1:min(20, nrow(nodes_df)), ],
      ggplot2::aes(x = x, y = y, label = keyword),
      size = 2.5,
      max.overlaps = 30
    ) +
    ggplot2::scale_size_continuous(
      name = "Frequency",
      range = c(3, 10)
    ) +
    ggplot2::scale_linewidth_continuous(
      name = "Co-occurrence",
      range = c(0.2, 1.2)
    ) +
    ggplot2::scale_color_manual(values = colors, name = "Community") +
    ieee_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Keyword Co-occurrence Network",
      subtitle = "Node size = frequency, edge width = co-occurrence strength"
    )
  
  p
}

#' Create keyword frequency bar chart
#' @keywords internal
create_keyword_frequency_plot <- function(data, config) {
  if (is.null(data$keyword_freq) || length(data$keyword_freq) == 0) return(NULL)
  
  top_n <- config$top_n_keywords %||% 30
  freq <- data$keyword_freq[1:min(top_n, length(data$keyword_freq))]
  
  freq_df <- data.frame(
    keyword = names(freq),
    frequency = as.numeric(freq),
    stringsAsFactors = FALSE
  )
  
  freq_df$keyword <- factor(freq_df$keyword, levels = rev(freq_df$keyword))
  
  p <- ggplot2::ggplot(freq_df, ggplot2::aes(x = keyword, y = frequency)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Frequency",
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Keywords by Frequency",
      subtitle = sprintf("Showing top %d keywords", nrow(freq_df)),
      x = NULL
    )
  
  p
}

#' Create co-occurrence heatmap
#' @keywords internal
create_cooccurrence_heatmap <- function(data, config) {
  if (is.null(data$adjacency_top) || nrow(data$adjacency_top) == 0) return(NULL)
  
  adj <- data$adjacency_top
  n <- nrow(adj)
  
  if (n < 2) return(NULL)
  
  # Melt matrix for heatmap
  adj_df <- reshape2::melt(as.matrix(adj))
  names(adj_df) <- c("keyword1", "keyword2", "weight")
  
  # Filter out zero weights
  adj_df <- adj_df[adj_df$weight > 0, ]
  
  # Order by weight
  keyword_order <- names(sort(colSums(adj), decreasing = TRUE))
  adj_df$keyword1 <- factor(adj_df$keyword1, levels = keyword_order)
  adj_df$keyword2 <- factor(adj_df$keyword2, levels = rev(keyword_order))
  
  p <- ggplot2::ggplot(adj_df, ggplot2::aes(x = keyword1, y = keyword2, fill = weight)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#F7FBFF",
      high = "#084594",
      na.value = "white",
      name = "Co-occurrence"
    ) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_discrete(name = NULL) +
    ieee_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      legend.position = "right"
    ) +
    ggplot2::labs(
      title = "Keyword Co-occurrence Heatmap",
      subtitle = "Strength of association between keyword pairs"
    )
  
  p
}

#' Create centrality comparison plot
#' @keywords internal
create_centrality_plot <- function(data, config) {
  if (is.null(data$metrics) || is.null(data$metrics$summary)) return(NULL)
  
  metrics_df <- data$metrics$summary
  
  if (nrow(metrics_df) == 0) return(NULL)
  
  top_n <- min(15, nrow(metrics_df))
  metrics_df <- metrics_df[order(-metrics_df$degree), ][1:top_n, ]
  
  metrics_long <- reshape2::melt(
    metrics_df[, c("keyword", "degree", "betweenness", "closeness", "eigenvector")],
    id.vars = "keyword",
    variable.name = "metric",
    value.name = "value"
  )
  
  metrics_long$metric <- factor(
    metrics_long$metric,
    levels = c("degree", "betweenness", "closeness", "eigenvector"),
    labels = c("Degree", "Betweenness", "Closeness", "Eigenvector")
  )
  
  metrics_long$keyword <- factor(metrics_long$keyword, levels = rev(metrics_df$keyword))
  
  p <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = keyword, y = value, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("#0072BD", "#D95319", "#EDB120", "#7E2F8E"),
      name = "Centrality"
    ) +
    ggplot2::scale_y_continuous(name = "Centrality Value", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Keyword Centrality Comparison",
      subtitle = "Multiple centrality measures for top keywords",
      x = NULL
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create community structure plot
#' @keywords internal
create_community_plot <- function(data, config) {
  if (is.null(data$communities) || is.null(data$communities$community_sizes)) return(NULL)
  
  sizes <- data$communities$community_sizes
  sizes_df <- data.frame(
    community = paste("Community", seq_along(sizes)),
    size = as.numeric(sizes),
    stringsAsFactors = FALSE
  )
  
  sizes_df$community <- factor(sizes_df$community, levels = sizes_df$community)
  
  colors <- get_ieee_palette(nrow(sizes_df))
  
  p <- ggplot2::ggplot(sizes_df, ggplot2::aes(x = community, y = size, fill = community)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = size), vjust = -0.3, size = 3, family = "mono") +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(
      name = "Number of Keywords",
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Keyword Community Structure",
      subtitle = sprintf("Number of communities: %d", data$communities$n_communities),
      x = NULL
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p
}

#' Create top pairs plot
#' @keywords internal
create_top_pairs_plot <- function(data, config) {
  if (is.null(data$summary) || is.null(data$summary$top_pairs)) return(NULL)
  if (nrow(data$summary$top_pairs) == 0) return(NULL)
  
  pairs_df <- head(data$summary$top_pairs, 20)
  pairs_df$pair <- paste(pairs_df$keyword1, "-", pairs_df$keyword2)
  pairs_df$pair <- factor(pairs_df$pair, levels = rev(pairs_df$pair))
  
  p <- ggplot2::ggplot(pairs_df, ggplot2::aes(x = pair, y = weight)) +
    ggplot2::geom_col(fill = "#77AC30", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Co-occurrence Count",
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Keyword Co-occurrence Pairs",
      subtitle = "Most frequently occurring keyword combinations",
      x = NULL
    )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
