# ============================================================================
# m3_render_sankey.R - Sankey Diagram for Collaboration Flows
# ============================================================================
# Creates Sankey diagrams for country collaboration networks
# (collaboration flows between countries/regions over time)

#' Render Sankey diagram for collaboration flows
#'
#' @param data Output from m3_compute_collaboration_indices or prepared data
#' @param type Type of Sankey: "country", "region", "temporal"
#' @param config Configuration list
#' @return List with Sankey plot
#' @export
render_m3_sankey <- function(data, type = "country", config = biblio_config()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(list(plots = list(), status = "error: ggplot2 not installed"))
  }
  
  plots <- list()
  
  # Prepare flow data
  flow_data <- prepare_sankey_data(data, type, config)
  
  if (is.null(flow_data) || nrow(flow_data) == 0) {
    return(list(plots = list(), status = "error: no collaboration data"))
  }
  
  # Create Sankey using ggalluvial or network approach
  if (requireNamespace("ggalluvial", quietly = TRUE)) {
    plots$sankey <- create_alluvial_sankey(flow_data, type, config)
  } else {
    # Fallback to geom_segment approach
    plots$sankey <- create_segment_sankey(flow_data, type, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Prepare data for Sankey diagram
#' @keywords internal
prepare_sankey_data <- function(data, type, config) {
  if (type == "country") {
    # Get collaboration matrix
    if (!is.null(data$collaboration_matrix)) {
      collab_matrix <- data$collaboration_matrix
    } else if (!is.null(data$collaboration_indices) && !is.null(data$collaboration_indices$matrix)) {
      collab_matrix <- data$collaboration_indices$matrix
    } else {
      return(NULL)
    }
    
    # Convert matrix to flow data
    flow_data <- matrix_to_flow(collab_matrix, config$top_n_countries %||% 20)
    
    return(flow_data)
    
  } else if (type == "temporal") {
    # Get temporal collaboration data
    if (!is.null(data$temporal_collaboration)) {
      return(data$temporal_collaboration)
    }
    return(NULL)
    
  } else if (type == "region") {
    # Aggregate by region
    if (!is.null(data$regional_collaboration)) {
      return(data$regional_collaboration)
    }
    return(NULL)
  }
  
  NULL
}

#' Convert collaboration matrix to flow format
#' @keywords internal
matrix_to_flow <- function(matrix, top_n) {
  # Get top countries
  total_collab <- rowSums(matrix, na.rm = TRUE) + colSums(matrix, na.rm = TRUE) - diag(matrix)
  top_countries <- names(sort(total_collab, decreasing = TRUE))[1:min(top_n, length(total_collab))]
  
  # Filter matrix to top countries
  matrix_subset <- matrix[top_countries, top_countries]
  
  # Create flow data
  flows <- data.frame(
    source = character(),
    target = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(matrix_subset))) {
    for (j in seq_len(ncol(matrix_subset))) {
      if (i < j && matrix_subset[i, j] > 0) {
        flows <- rbind(flows, data.frame(
          source = rownames(matrix_subset)[i],
          target = colnames(matrix_subset)[j],
          value = matrix_subset[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Sort by value
  flows <- flows[order(-flows$value), ]
  
  flows
}

#' Create alluvial Sankey plot
#' @keywords internal
create_alluvial_sankey <- function(flow_data, type, config) {
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    return(create_segment_sankey(flow_data, type, config))
  }
  
  # Limit to top flows
  top_n <- config$top_n_flows %||% 30
  flow_data <- flow_data[1:min(top_n, nrow(flow_data)), ]
  
  palette <- get_biblio_palette(n = length(unique(c(flow_data$source, flow_data$target))))
  
  # Create node positions
  nodes <- unique(c(flow_data$source, flow_data$target))
  node_df <- data.frame(
    name = nodes,
    id = seq_along(nodes),
    stringsAsFactors = FALSE
  )
  
  # Create alluvial format
  alluvial_df <- data.frame(
    source = factor(flow_data$source),
    target = factor(flow_data$target),
    value = flow_data$value,
    stringsAsFactors = FALSE
  )
  
  ggplot2::ggplot(alluvial_df,ggplot2::aes(
    axis1 = source,
    axis2 = target,
    y = value
  )) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = source), width = 1/12) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey50", color = "white") +
    ggplot2::geom_text(stat = "stratum", ggplot2::aes(label = after_stat(stratum)), size = 3) +
    ggplot2::scale_x_discrete(limits = c("Source", "Target"), expand = c(0.05, 0.05)) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "International Collaboration Network",
      subtitle = sprintf("Top %d collaboration flows", nrow(flow_data)),
      y = "Collaboration Count"
    ) +ieee_theme()
}

#' Create segment-based Sankey (fallback)
#' @keywords internal
create_segment_sankey <- function(flow_data, type, config) {
  top_n <- config$top_n_flows %||% 20
  flow_data <- flow_data[1:min(top_n, nrow(flow_data)), ]
  
  palette <- get_biblio_palette(n = nrow(flow_data))
  
  ggplot2::ggplot(flow_data, ggplot2::aes(
    x = 0,
    xend = 1,
    y = source,
    yend = target
  )) +
    ggplot2::geom_curve(
      ggplot2::aes(linewidth = value, color = source),
      curvature = -0.2,
      alpha = 0.6
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.5, 3), guide = "none") +
    ggplot2::scale_color_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "International Collaboration Network",
      subtitle =sprintf("Top %d collaboration flows", nrow(flow_data))
    ) +ieee_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

#' Render temporal Sankey diagram
#'
#' @param data Output from m3_compute_temporal_dynamics
#' @param config Configuration list
#' @return List with temporal Sankey plot
#' @export
render_m3_temporal_sankey <- function(data, config = biblio_config()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(list(plots = list(), status = "error: ggplot2 not installed"))
  }
  
  share_evolution <- data$share_evolution
  
  if (is.null(share_evolution) || nrow(share_evolution) == 0) {
    return(list(plots = list(), status = "error: no temporal data"))
  }
  
  # Prepare for alluvial plot
  years <- sort(unique(share_evolution$year))
  countries <- unique(share_evolution$country)
  
  # Get top countries
  top_countries <- share_evolution %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(total = sum(share, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    dplyr::slice(1:min(15, dplyr::n())) %>%
    dplyr::pull(country)
  
  # Filter and prepare
  plot_data <- share_evolution %>%
    dplyr::filter(country %in% top_countries) %>%
    dplyr::mutate(year_factor = factor(year))
  
  palette <- get_biblio_palette(n = length(top_countries))
  
  p <- ggplot2::ggplot(plot_data,
    ggplot2::aes(
      x = year_factor,
      stratum = country,
      alluvium = country,
      y = share,
      fill = country
    )
  ) +
    ggalluvial::geom_alluvium(alpha = 0.7) +
    ggalluvial::geom_stratum(alpha = 0.9, color = "white") +
    ggplot2::geom_text(stat = "stratum", ggplot2::aes(label = after_stat(stratum)), size = 2.5) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "Country Share Evolution Over Time",
      x = "Year",
      y = "Share (%)"
    ) +ieee_theme()
  
  list(plots = list(temporal_sankey = p), status = "success")
}

#' Build Sankey data table
#' @export
build_m3_sankey_table <- function(data, type = "country", config = biblio_config()) {
  flow_data <- prepare_sankey_data(data, type, config)
  
  if (is.null(flow_data)) {
    return(data.frame(
      source = character(),
      target = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  flow_data
}
