# ============================================================================
# m1_render_treemap.R - Treemap Visualization for M1
# ============================================================================
# Creates treemap visualizations for hierarchical data display
# (countries, sources, keywords, authors)

#' Render treemap visualization
#'
#' @param data Output from compute_m1_* functions
#' @param type Type of treemap: "countries", "sources", "keywords", "authors"
#' @param config Configuration list
#' @return List with treemap plot
#' @export
render_m1_treemap <- function(data, type = "countries", config = biblio_config()) {
  if (!requireNamespace("treemapify", quietly = TRUE)) {
    cli::cli_warn("treemapify package required for treemap visualization")
    return(list(plots = list(), status = "error: treemapify not installed"))
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(list(plots = list(), status = "error: ggplot2 not installed"))
  }
  
  plots <- list()
  
  # Prepare data based on type
  treemap_data <- prepare_treemap_data(data, type)
  
  if (is.null(treemap_data) || nrow(treemap_data) == 0) {
    return(list(plots = list(), status = "error: no data for treemap"))
  }
  
  # Create treemap
  plots$treemap <- create_treemap_plot(treemap_data, type, config)
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Prepare data for treemap
#' @keywords internal
prepare_treemap_data <- function(data, type) {
  df <- NULL
  
  if (type == "countries") {
    if (!is.null(data$countries) && is.data.frame(data$countries)) {
      df <- data.frame(
        name = data$countries$country,
        value = data$countries$total_docs,
        stringsAsFactors = FALSE
      )
    } else if (!is.null(data$country_summary) && is.data.frame(data$country_summary)) {
      df <- data.frame(
        name = data$country_summary$country,
        value = data$country_summary$total_docs,
        stringsAsFactors = FALSE
      )
    }
  } else if (type == "sources") {
    if (!is.null(data$sources) && is.data.frame(data$sources)) {
      df <- data.frame(
        name = data$sources$source,
        value = data$sources$n_docs,
        stringsAsFactors = FALSE
      )
    }
  } else if (type == "keywords") {
    if (!is.null(data$keywords) && is.data.frame(data$keywords)) {
      df <- data.frame(
        name = data$keywords$keyword,
        value = data$keywords$freq,
        stringsAsFactors = FALSE
      )
    }
  } else if (type == "authors") {
    if (!is.null(data$authors) && is.data.frame(data$authors)) {
      df <- data.frame(
        name = data$authors$author,
        value = data$authors$n_docs,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  # Remove NA values
  df <- df[!is.na(df$value) & df$value > 0, ]
  
  # Sort and take top N
  df <- df[order(-df$value), ]
  top_n <- min(50, nrow(df))
  df <- df[1:top_n, ]
  
  # Add parent for hierarchical treemap
  df$parent <- "All"
  
  # Add percentage
  df$percentage <- df$value / sum(df$value) * 100
  
  df
}

#' Create treemap plot
#' @keywords internal
create_treemap_plot <- function(df, type, config) {
  if (nrow(df) == 0) return(NULL)
  
  # Create labels
  df$label <- paste0(df$name, "\n", df$value, " (", sprintf("%.1f", df$percentage), "%)")
  
  # Color palette
  palette <- get_biblio_palette(n = nrow(df))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(
    area = value,
    fill = name,
    label = label
  )) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      colour = "white",
      place = "centre",
      size = 10,
      grow = TRUE
    ) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = paste0("Treemap: ", tools::toTitleCase(type)),
      subtitle = sprintf("Top %d by publication count", nrow(df))
    ) +
    ieee_theme() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
  
  p
}

#' Build treemap table
#' @export
build_m1_treemap_table <- function(data, type = "countries", config = biblio_config()) {
  df <- prepare_treemap_data(data, type)
  
  if (is.null(df)) {
    return(data.frame(
      name = character(),
      value = integer(),
      percentage = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  df
}

#' Render nested treemap with groupings
#'
#' @param data Output from compute_m1_* functions
#' @param group_col Column to group by
#' @param type Type of treemap
#' @param config Configuration list
#' @return List with nested treemap plot
#' @export
render_m1_nested_treemap <- function(data, group_col = NULL, type = "countries", config = biblio_config()) {
  if (!requireNamespace("treemapify", quietly = TRUE)) {
    return(list(plots = list(), status = "error: treemapify not installed"))
  }
  
  df <- prepare_treemap_data(data, type)
  
  if (is.null(df)) {
    return(list(plots = list(), status = "error: no data"))
  }
  
  # If grouping column provided, create nested structure
  if (!is.null(group_col) && group_col %in% names(df)) {
    df$group <- df[[group_col]]
  } else {
    # Auto-group: assign top items to "Top", others to "Other"
    top_n <- config$treemap_top_n %||% 10
    df$group <- ifelse(seq_len(nrow(df)) <= top_n, "Top", "Other")
  }
  
  palette <- get_biblio_palette(n = nrow(df))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(
    area = value,
    fill = name,
    subgroup = group,
    label = name
  )) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5) +
    treemapify::geom_treemap_text(colour = "white", place = "topleft", grow = FALSE, size = 8) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::labs(title = paste0("Nested Treemap: ", tools::toTitleCase(type))) +
    ieee_theme()
  
  list(plots = list(treemap = p), status = "success")
}