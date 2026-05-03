# ============================================================================
# m3_render_collaboration_indices.R - Render Country Collaboration Indices
# ============================================================================
# Creates IEEE Q1 quality visualizations for collaboration indices
# including Salton's Index, Jaccard Index, and Affinity Index

#' Render collaboration indices visualizations
#'
#' @param data Output from compute_m3_collaboration_indices
#' @param config Configuration list
#' @return List with plots
#' @export
render_m3_collaboration_indices <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  if (!is.null(data$salton_matrix) && nrow(data$salton_matrix) > 0) {
    plots$salton_heatmap <- m3_create_collaboration_index_heatmap(
      data$salton_matrix,
      "Salton's Index (Cosine Similarity)",
      config
    )
  }
  
  if (!is.null(data$jaccard_matrix) && nrow(data$jaccard_matrix) > 0) {
    plots$jaccard_heatmap <- m3_create_collaboration_index_heatmap(
      data$jaccard_matrix,
      "Jaccard Index",
      config
    )
  }
  
  if (!is.null(data$affinity_matrix) && nrow(data$affinity_matrix) > 0) {
    plots$affinity_heatmap <- m3_create_collaboration_index_heatmap(
      data$affinity_matrix,
      "Affinity Index",
      config
    )
  }
  
  if (!is.null(data$bilateral) && nrow(data$bilateral) > 0) {
    plots$top_collaborations <- m3_create_top_collaborations_plot(data$bilateral, config)
  }
  
  if (!is.null(data$indices) && nrow(data$indices) > 0) {
    plots$country_indices <- m3_create_country_indices_plot(data$indices, config)
  }
  
  if (!is.null(data$summary$top_collaborators) && nrow(data$summary$top_collaborators) > 0) {
    plots$top_pairs <- m3_create_top_pairs_barplot(data$summary$top_collaborators, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create collaboration heatmap
#' @keywords internal
m3_create_collaboration_index_heatmap <- function(matrix, title, config) {
  n <- nrow(matrix)
  if (n < 2) return(NULL)
  
  max_countries <- config$max_heatmap_countries %||% 30
  if (n > max_countries) {
    diag_vals <- diag(matrix)
    if (is.null(diag_vals)) diag_vals <- rep(0, n)
    row_sums <- rowSums(abs(matrix))
    top_idx <- order(row_sums, decreasing = TRUE)[1:max_countries]
    matrix <- matrix[top_idx, top_idx]
    n <- max_countries
  }
  
  df <- reshape2::melt(as.matrix(matrix))
  names(df) <- c("country_1", "country_2", "value")
  
  df$country_1 <- factor(df$country_1, levels = rev(rownames(matrix)))
  df$country_2 <- factor(df$country_2, levels = colnames(matrix))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = country_2, y = country_1, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B2182B",
      midpoint = median(df$value, na.rm = TRUE),
      name = "Index",
      na.value = "grey90"
    ) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_discrete(name = NULL) +
    ieee_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.5, "inches")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Collaboration strength for %d countries", n)
    )
  
  p
}

#' Create top collaborations plot
#' @keywords internal
m3_create_top_collaborations_plot <- function(bilateral_data, config) {
  top_n <- config$top_n_collaborations %||% 20
  
  bilateral_data$pair <- paste(bilateral_data$country_1, "-", bilateral_data$country_2)
  df <- stats::aggregate(
    cbind(salton, jaccard, avg_affinity) ~ pair,
    data = bilateral_data,
    FUN = function(x) max(x, na.rm = TRUE)
  )
  df <- head(df[order(-df$salton), ], top_n)
  
  if (nrow(df) == 0) return(NULL)
  
  df$pair <- factor(df$pair, levels = unique(rev(df$pair)))
  
  metrics_long <- reshape2::melt(
    df[, c("pair", "salton", "jaccard", "avg_affinity")],
    id.vars = "pair",
    variable.name = "metric",
    value.name = "value"
  )
  
  metric_labels <- c(salton = "Salton", jaccard = "Jaccard", avg_affinity = "Affinity")
  metrics_long$metric <- factor(metrics_long$metric, levels = c("salton", "jaccard", "avg_affinity"),
                                labels = c("Salton", "Jaccard", "Affinity"))
  
  colors <- c("Salton" = "#0072BD", "Jaccard" = "#D95319", "Affinity" = "#77AC30")
  
  p <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = pair, y = value, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, name = "Index") +
    ggplot2::scale_y_continuous(name = "Index Value", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Country Collaborations",
      subtitle = sprintf("Top %d bilateral partnerships by collaboration strength", top_n),
      x = NULL
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create country indices plot
#' @keywords internal
m3_create_country_indices_plot <- function(indices_data, config) {
  top_n <- config$top_n_countries %||% 15
  
  df <- head(indices_data[order(-indices_data$n_documents), ], top_n)
  
  if (nrow(df) == 0) return(NULL)
  
  df$country <- factor(df$country, levels = rev(df$country))
  
  indices_long <- reshape2::melt(
    df[, c("country", "avg_salton_index", "avg_jaccard_index", "avg_affinity_index")],
    id.vars = "country",
    variable.name = "metric",
    value.name = "value"
  )
  
  metric_labels <- c(
    avg_salton_index = "Salton",
    avg_jaccard_index = "Jaccard",
    avg_affinity_index = "Affinity"
  )
  indices_long$metric <- factor(indices_long$metric,
                                 levels = c("avg_salton_index", "avg_jaccard_index", "avg_affinity_index"),
                                 labels = c("Salton", "Jaccard", "Affinity"))
  
  p <- ggplot2::ggplot(indices_long, ggplot2::aes(x = country, y = value, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("Salton" = "#0072BD", "Jaccard" = "#D95319", "Affinity" = "#77AC30"),
      name = "Index"
    ) +
    ggplot2::scale_y_continuous(name = "Average Index Value", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Country Collaboration Profiles",
      subtitle = sprintf("Average collaboration indices for top %d countries", top_n),
      x = NULL
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create top pairs barplot
#' @keywords internal
m3_create_top_pairs_barplot <- function(top_pairs, config) {
  top_n <- min(15, nrow(top_pairs))
  top_pairs$pair <- paste(top_pairs$country_1, "-", top_pairs$country_2)
  df <- stats::aggregate(salton ~ pair, data = top_pairs, FUN = function(x) max(x, na.rm = TRUE))
  df <- head(df[order(-df$salton), ], top_n)
  
  if (nrow(df) == 0) return(NULL)
  
  df$pair <- factor(df$pair, levels = unique(rev(df$pair)))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = pair, y = salton)) +
    ggplot2::geom_col(fill = "#A2142F", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Salton's Index",
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Strongest Collaboration Pairs",
      subtitle = sprintf("Top %d country pairs by Salton's Index", top_n),
      x = NULL
    )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
