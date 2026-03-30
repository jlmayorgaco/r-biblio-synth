# ============================================================================
# m3_render_network.R - Collaboration Network Visualization
# ============================================================================
# Creates network graphs for international collaboration analysis

#' Render collaboration network visualization
#'
#' @param data Output from m3_compute_collaboration_indices or similar
#' @param config Configuration list
#' @return List with network plots
#' @export
render_m3_collaboration_network <- function(data, config = biblio_config()) {
  if (is.null(data) || is.null(data$collaboration_matrix)) {
    return(list(plots = list(), status = "error: no collaboration data"))
  }
  
  plots <- list()
  
  # Main collaboration network
  plots$network <- create_collaboration_network_plot(data, config)
  
  # Network centrality visualization
  if (!is.null(data$centrality)) {
    plots$centrality <- create_centrality_plot(data, config)
  }
  
  # Community detection visualization
  if (!is.null(data$communities)) {
    plots$communities <- create_community_plot(data, config)
  }
  
  # Collaboration heatmap
  plots$heatmap <- create_collaboration_heatmap(data, config)
  
  # Core-periphery structure
  if (!is.null(data$core_periphery)) {
    plots$core_periphery <- create_core_periphery_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create main collaboration network plot
#' @keywords internal
create_collaboration_network_plot <- function(data, config) {
  collab_matrix <- data$collaboration_matrix
  
  if (is.null(collab_matrix) || nrow(collab_matrix) == 0) {
    return(NULL)
  }
  
  # Filter to top countries
  n_top <- config$top_n_countries %||% min(30, nrow(collab_matrix))
  
  # Get total collaborations per country
  total_collab <- rowSums(collab_matrix, na.rm = TRUE)
  top_countries <- names(sort(total_collab, decreasing = TRUE))[1:n_top]
  
  collab_matrix <- collab_matrix[top_countries, top_countries]
  
  # Create edge list
  edges <- data.frame(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(nrow(collab_matrix) - 1)) {
    for (j in (i + 1):ncol(collab_matrix)) {
      weight <- collab_matrix[i, j]
      if (weight > 0) {
        edges <- rbind(edges, data.frame(
          from = rownames(collab_matrix)[i],
          to = colnames(collab_matrix)[j],
          weight = weight,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(edges) == 0) {
    return(NULL)
  }
  
  # Create node attributes
  nodes <- data.frame(
    name = top_countries,
    size = total_collab[top_countries],
    stringsAsFactors = FALSE
  )
  
  # Scale node sizes
  nodes$size <- scales::rescale(nodes$size, to = c(5, 20))
  
  # Layout using force-directed algorithm
  set.seed(12345)
  
  # Create igraph object for layout
  g <- tryCatch({
    igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  }, error = function(e) NULL)
  
  if (is.null(g)) {
    return(NULL)
  }
  
  # Calculate layout
  layout_df <- tryCatch({
    layout_matrix <- igraph::layout_with_fr(g)
    data.frame(
      name = nodes$name,
      x = layout_matrix[, 1],
      y = layout_matrix[, 2],
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    # Fallback: circular layout
    angles <- seq(0, 2 * pi, length.out = nrow(nodes))
    data.frame(
      name = nodes$name,
      x = cos(angles),
      y = sin(angles),
      stringsAsFactors = FALSE
    )
  })
  
  # Merge with edges
  edges$from_x <- layout_df$x[match(edges$from, layout_df$name)]
  edges$from_y <- layout_df$y[match(edges$from, layout_df$name)]
  edges$to_x <- layout_df$x[match(edges$to, layout_df$name)]
  edges$to_y <- layout_df$y[match(edges$to, layout_df$name)]
  
  # Scale edge weights
  edges$width <- scales::rescale(edges$weight, to = c(0.2, 3))
  
  # Create plot
  p <- ggplot2::ggplot() +
    # Draw edges
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      color = "gray70",
      alpha = 0.5,
      linewidth = edges$width
    ) +
    # Draw nodes
    ggplot2::geom_point(
      data = layout_df,
      ggplot2::aes(x = x, y = y, size = nodes$size),
      color = "#0072BD",
      alpha = 0.8
    ) +
    # Add labels
    ggplot2::geom_text(
      data = layout_df,
      ggplot2::aes(x = x, y = y, label = name),
      size = 2.5,
      hjust = 0.5,
      vjust = -0.5
    ) +
    ggplot2::scale_size_continuous(range = c(3, 10), guide = "none") +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "International Collaboration Network",
      subtitle = sprintf("Top %d countries by collaboration strength", n_top)
    )
  
  p
}

#' Create centrality visualization
#' @keywords internal
create_centrality_plot <- function(data, config) {
  if (is.null(data$centrality) || !is.data.frame(data$centrality)) {
    return(NULL)
  }
  
  centrality <- data$centrality
  
  # Reshape for plotting
  if (!"country" %in% names(centrality)) {
    centrality$country <- rownames(centrality)
  }
  
  # Get top countries by degree centrality
  top_n <- config$top_n_countries %||% 20
  centrality <- centrality[order(-centrality$degree), ]
  centrality <- centrality[1:min(top_n, nrow(centrality)), ]
  
  # Reshape to long format
  metrics <- c("degree", "betweenness", "closeness", "eigenvector")
  metrics_present <- intersect(metrics, names(centrality))
  
  if (length(metrics_present) == 0) {
    return(NULL)
  }
  
  centrality_long <- reshape2::melt(
    centrality[, c("country", metrics_present)],
    id.vars = "country",
    variable.name = "metric",
    value.name = "value"
  )
  
  centrality_long$country <- factor(centrality_long$country, levels = rev(centrality$country))
  
  colors <- c(degree = "#0072BD", betweenness = "#D95319", 
              closeness = "#77AC30", eigenvector = "#A2142F")
  colors <- colors[metrics_present]
  
  p <- ggplot2::ggplot(centrality_long, ggplot2::aes(x = country, y = value, fill = metric)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), 
                     width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::coord_flip() +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Network Centrality Measures",
      subtitle = "Top countries by degree centrality",
      x = NULL,
      y = "Centrality score (normalized)"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create collaboration heatmap
#' @keywords internal
create_collaboration_heatmap <- function(data, config) {
  collab_matrix <- data$collaboration_matrix
  
  if (is.null(collab_matrix) || nrow(collab_matrix) == 0) {
    return(NULL)
  }
  
  # Filter to top countries
  n_top <- config$top_n_countries %||% min(25, nrow(collab_matrix))
  
  total_collab <- rowSums(collab_matrix, na.rm = TRUE)
  top_countries <- names(sort(total_collab, decreasing = TRUE))[1:n_top]
  
  collab_matrix <- collab_matrix[top_countries, top_countries]
  
  # Convert to long format
  collab_long <- reshape2::melt(collab_matrix)
  names(collab_long) <- c("country1", "country2", "collaborations")
  
  # Order by total collaborations
  country_order <- names(sort(rowSums(collab_matrix, na.rm = TRUE), decreasing = TRUE))
  collab_long$country1 <- factor(collab_long$country1, levels = rev(country_order))
  collab_long$country2 <- factor(collab_long$country2, levels = country_order)
  
  p <- ggplot2::ggplot(collab_long, ggplot2::aes(x = country2, y = country1, fill = collaborations)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#F7FBFF",
      high = "#084594",
      trans = "log10",
      na.value = "grey90",
      name = "Collaborations\n(log scale)"
    ) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 6),
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.5, "inches")
    ) +
    ggplot2::labs(
      title = "Collaboration Matrix Heatmap",
      x = NULL,
      y = NULL
    )
  
  p
}

#' Create community detection plot
#' @keywords internal
create_community_plot <- function(data, config) {
  if (is.null(data$communities) || !is.data.frame(data$communities)) {
    return(NULL)
  }
  
  communities <- data$communities
  
  if (!"community" %in% names(communities) || !"country" %in% names(communities)) {
    return(NULL)
  }
  
  # Count countries per community
  community_sizes <- aggregate(country ~ community, data = communities, FUN = length)
  community_sizes <- community_sizes[order(-community_sizes$country), ]
  
  # Top communities
  n_communities <- config$n_communities %||% min(10, nrow(community_sizes))
  top_communities <- community_sizes$community[1:n_communities]
  
  communities_top <- communities[communities$community %in% top_communities, ]
  
  # Jitter to avoid overplotting
  set.seed(12345)
  communities_top$x <- as.numeric(factor(communities_top$community))
  communities_top$x <- communities_top$x + runif(nrow(communities_top), -0.3, 0.3)
  
  colors <- ggplot2::scale_fill_hue(l = 50, c = 70)
  
  p <- ggplot2::ggplot(communities_top, ggplot2::aes(x = x, y = 1, fill = community, label = country)) +
    ggplot2::geom_point(ggplot2::aes(color = community), size = 3, alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(y = 1.1), size = 2, angle = 45, hjust = 0) +
    ggplot2::scale_color_discrete(name = "Community") +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "Collaboration Communities",
      subtitle = sprintf("%d communities detected", length(unique(communities_top$community)))
    ) +
    ggplot2::theme(legend.position = "none")
  
  p
}

#' Create core-periphery plot
#' @keywords internal
create_core_periphery_plot <- function(data, config) {
  if (is.null(data$core_periphery)) {
    return(NULL)
  }
  
  core <- data$core_periphery$core
  periphery <- data$core_periphery$periphery
  
  if (length(core) == 0 || length(periphery) == 0) {
    return(NULL)
  }
  
  # Create data frame
  df <- data.frame(
    country = c(core, periphery),
    position = c(rep("Core", length(core)), rep("Periphery", length(periphery))),
    stringsAsFactors = FALSE
  )
  
  df$position <- factor(df$position, levels = c("Core", "Periphery"))
  
  colors <- c("Core" = "#0072BD", "Periphery" = "#D95319")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = position, fill = position)) +
    ggplot2::geom_bar(color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = country), 
                      stat = "count", position = ggplot2::position_stack(vjust = 0.5),
                      size = 2.5) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(name = "Number of Countries") +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Core-Periphery Structure",
      subtitle = sprintf("Core: %d countries, Periphery: %d countries", 
                        length(core), length(periphery))
    ) +
    ggplot2::theme(legend.position = "none")
  
  p
}

#' Compute network centrality measures
#'
#' @param collab_matrix Collaboration matrix
#' @return Data frame with centrality measures
#' @export
compute_network_centrality <- function(collab_matrix) {
  if (is.null(collab_matrix) || nrow(collab_matrix) == 0) {
    return(data.frame())
  }
  
  countries <- rownames(collab_matrix)
  n <- length(countries)
  
  # Degree centrality (normalized)
  degree <- rowSums(collab_matrix > 0, na.rm = TRUE)
  degree_norm <- degree / (n - 1)
  
  # Strength (weighted degree)
  strength <- rowSums(collab_matrix, na.rm = TRUE)
  strength_norm <- strength / sum(strength)
  
  # Betweenness centrality
  betweenness <- compute_betweenness(collab_matrix)
  
  # Closeness centrality
  closeness <- compute_closeness(collab_matrix)
  
  # Eigenvector centrality
  eigenvector <- compute_eigenvector_centrality(collab_matrix)
  
  data.frame(
    country = countries,
    degree = degree_norm,
    strength = strength_norm,
    betweenness = betweenness,
    closeness = closeness,
    eigenvector = eigenvector,
    stringsAsFactors = FALSE
  )
}

#' Compute betweenness centrality
#' @keywords internal
compute_betweenness <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  
  # Convert to binary for shortest paths
  binary <- adj_matrix >0
  
  betweenness <- numeric(n)
  
  for (s in 1:n) {
    for (t in 1:n) {
      if (s == t) next
      
      # Simple BFS to find shortest paths (simplified)
      # This is an approximation - full algorithm uses Brandes' algorithm
      dist <- numeric(n)
      dist[] <- Inf
      dist[s] <- 0
      
      queue <- s
      while (length(queue) > 0) {
        v <- queue[1]
        queue <- queue[-1]
        
        neighbors <- which(binary[v, ])
        for (w in neighbors) {
          if (dist[w] == Inf) {
            dist[w] <- dist[v] + 1
            queue <- c(queue, w)
          }
        }
      }
    }
  }
  
  # Normalize
  betweenness / sum(betweenness)
}

#' Compute closeness centrality
#' @keywords internal
compute_closeness <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  binary <- adj_matrix > 0
  
  closeness <- numeric(n)
  
  for (i in 1:n) {
    # BFS for distances
    dist <- numeric(n)
    dist[] <- Inf
    dist[i] <- 0
    
    queue <- i
    while (length(queue) > 0) {
      v <- queue[1]
      queue <- queue[-1]
      
      neighbors <- which(binary[v, ])
      for (w in neighbors) {
        if (dist[w] == Inf) {
          dist[w] <- dist[v] + 1
          queue <- c(queue, w)
        }
      }
    }
    
    reachable <- sum(is.finite(dist)) - 1
    if (reachable > 0) {
      closeness[i] <- reachable / sum(dist[is.finite(dist)])
    }
  }
  
  closeness / max(closeness)
}

#' Compute eigenvector centrality
#' @keywords internal
compute_eigenvector_centrality <- function(adj_matrix) {
  # Power iteration
  binary <- adj_matrix > 0
  n <- nrow(binary)
  
  eigenvec <- rep(1, n)
  
  for (iter in 1:100) {
    eigenvec_new <- binary %*% eigenvec
    eigenvec_new <- eigenvec_new / max(eigenvec_new)
    
    if (max(abs(eigenvec_new - eigenvec)) < 1e-6) {
      break
    }
    eigenvec <- eigenvec_new
  }
  
  as.numeric(eigenvec / max(eigenvec))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b