# ============================================================================
# m1_render_three_field.R - Three-Field Plot (Sankey Tripartite)
# ============================================================================
# Creates three-field plots showing relationships between three entities
# (e.g., Authors-Keywords-Sources, Countries-Keywords-Years)

#' Render three-field plot
#'
#' Creates a Sankey-style tripartite diagram showing relationships between
#' three fields (e.g., authors, keywords, sources).
#'
#' @param data Bibliographic data frame
#' @param fields Named list with three fields: field1, field2, field3
#'        Each should be a list with name and column (e.g., list(name = "Authors", column = "AU"))
#' @param config Configuration list
#' @param n_items Number of top items to show per field (default 10)
#' @return List with three-field plot
#' @export
render_m1_three_field <- function(data, fields = NULL, config = biblio_config(), n_items = 10) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(list(plots = list(), status = "error: ggplot2 not installed"))
  }
  
  # Default fields: Authors - Keywords - Sources
  if (is.null(fields)) {
    fields <- list(
      field1 = list(name = "Authors", column = "AU"),
      field2 = list(name = "Keywords", column = "DE"),
      field3 = list(name = "Sources", column = "SO")
    )
  }
  
  # Validate data has required columns
  required_cols <- c(fields$field1$column, fields$field2$column, fields$field3$column)
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    return(list(plots = list(), status = paste("error: missing columns:", paste(missing_cols, collapse = ", "))))
  }
  
  # Prepare three-field data
  three_field_data <- prepare_three_field_data(
    data,
    fields$field1$column,
    fields$field2$column,
    fields$field3$column,
    n_items
  )
  
  if (is.null(three_field_data) || nrow(three_field_data$flows) == 0) {
    return(list(plots = list(), status = "error: no valid relationships"))
  }
  
  # Create plot
  if (requireNamespace("ggalluvial", quietly = TRUE)) {
    plot <- create_alluvial_three_field(three_field_data, fields, n_items, config)
  } else {
    plot <- create_network_three_field(three_field_data, fields, n_items, config)
  }
  
  list(
    plots = list(three_field = plot),
    data = three_field_data,
    status = "success"
  )
}

#' Prepare three-field relationship data
#' @keywords internal
prepare_three_field_data <- function(data, col1, col2, col3, n_items) {
  # Extract and count items for each field
  extract_items <- function(column, n) {
    items <- unlist(strsplit(as.character(column), ";"))
    items <- trimws(items)
    items <- items[items != "" & !is.na(items)]
    tab <- table(items)
    tab <- sort(tab, decreasing = TRUE)
    names(tab)[1:min(n, length(tab))]
  }
  
  # Get top items
  top1 <- extract_items(data[[col1]], n_items)
  top2 <- extract_items(data[[col2]], n_items)
  top3 <- extract_items(data[[col3]], n_items)
  
  # Create flows between fields
  flows12 <- create_field_flows(data, col1, col2, top1, top2)
  flows23 <- create_field_flows(data, col2, col3, top2, top3)
  
  # Count node frequencies
  count1 <- table(unlist(strsplit(as.character(data[[col1]]), ";")))
  count2 <- table(unlist(strsplit(as.character(data[[col2]]), ";")))
  count3 <- table(unlist(strsplit(as.character(data[[col3]]), ";")))
  
  list(
    nodes1 = data.frame(name = top1, count = as.numeric(count1[top1])),
    nodes2 = data.frame(name = top2, count = as.numeric(count2[top2])),
    nodes3 = data.frame(name = top3, count = as.numeric(count3[top3])),
    flows = rbind(flows12, flows23),
    n_fields = 3
  )
}

#' Create flows between two fields
#' @keywords internal
create_field_flows <- function(data, col_a, col_b, top_a, top_b) {
  flows <- data.frame(
    from = character(),
    to = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(data))) {
    items_a <- trimws(strsplit(as.character(data[[col_a]][i]), ";")[[1]])
    items_b <- trimws(strsplit(as.character(data[[col_b]][i]), ";")[[1]])
    
    items_a <- items_a[items_a %in% top_a]
    items_b <- items_b[items_b %in% top_b]
    
    # Add flows for each combination
    for (a in items_a) {
      for (b in items_b) {
        idx <- which(flows$from == a & flows$to == b)
        if (length(idx) > 0) {
          flows$value[idx] <- flows$value[idx] + 1
        } else {
          flows <- rbind(flows, data.frame(from = a, to = b, value = 1, stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  flows
}

#' Create alluvial three-field plot
#' @keywords internal
create_alluvial_three_field <- function(tf_data, fields, n_items, config) {
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    return(create_network_three_field(tf_data, fields, n_items, config))
  }
  
  # Create alluvial format data
  alluvial_data <- data.frame(
    field1 = character(),
    field2 = character(),
    field3 = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process flows
  for (i in seq_len(nrow(tf_data$flows))) {
    row <- tf_data$flows[i, ]
    
    # Determine which fields this flow connects
    if (row$from %in% tf_data$nodes1$name) {
      # Flow from field1 to field2
      field1_val <- row$from
      field2_val <- row$to
      field3_val <- NA
    } else {
      # Flow from field2 to field3
      field1_val <- NA
      field2_val <- row$from
      field3_val <- row$to
    }
    
    alluvial_data <- rbind(alluvial_data, data.frame(
      field1 = field1_val,
      field2 = field2_val,
      field3 = field3_val,
      value = row$value,
      stringsAsFactors = FALSE
    ))
  }
  
  # Create separate alluvial for 1-2 and 2-3 links
  flows_12 <- tf_data$flows[tf_data$flows$from %in% tf_data$nodes1$name, ]
  flows_23 <- tf_data$flows[tf_data$flows$from %in% tf_data$nodes2$name, ]
  
  # Create long format for alluvial
  long_data_12 <- data.frame(
    source = factor(flows_12$from, levels = tf_data$nodes1$name),
    target = factor(flows_12$to, levels = tf_data$nodes2$name),
    value = flows_12$value,
    stringsAsFactors = FALSE
  )
  
  palette <- get_biblio_palette(n = n_items * 3)
  
  # Create simplified alluvial plot
  p <- ggplot2::ggplot(long_data_12,
    ggplot2::aes(
      axis1 = source,
      axis2 = target,
      y = value
    )
  ) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = source), width = 1/4, alpha = 0.7) +
    ggalluvial::geom_stratum(width = 1/4, fill = "grey80", color = "white") +
    ggplot2::geom_text(stat = "stratum", ggplot2::aes(label = after_stat(ggplot2::stratum)), size = 3) +
    ggplot2::scale_x_discrete(
      limits = c(fields$field1$name, fields$field2$name),
      expand = c(0.05, 0.05)
    ) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = paste(fields$field1$name, "-", fields$field2$name, "Relationship"),
      y = "Number of Co-occurrences"
    ) +ieee_theme()
  
  p
}

#' Create network-style three-field plot (fallback)
#' @keywords internal
create_network_three_field <- function(tf_data, fields, n_items, config) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "igraph package required") +
      ggplot2::labs(title = "Three-Field Plot") +
      ggplot2::theme_void())
  }
  
  # Create graph
  edges <- tf_data$flows
  nodes <- data.frame(
    name = c(tf_data$nodes1$name, tf_data$nodes2$name, tf_data$nodes3$name),
    field = c(
      rep(fields$field1$name, nrow(tf_data$nodes1)),
      rep(fields$field2$name, nrow(tf_data$nodes2)),
      rep(fields$field3$name, nrow(tf_data$nodes3))
    ),
    stringsAsFactors = FALSE
  )
  
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  
  # Layout
  layout <- igraph::layout_with_fr(g)
  
  # Plot
  palette <- get_biblio_palette(n = 3)
  
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                      label = "Install ggalluvial for proper three-field plot") +
    ggplot2::labs(title = paste(fields$field1$name, fields$field2$name, fields$field3$name, sep = " - ")) +
    ggplot2::theme_void()
}

#' Build three-field relationship table
#' @export
build_m1_three_field_table <- function(data, fields = NULL, config = biblio_config(), n_items = 10) {
  # Default fields
  if (is.null(fields)) {
    fields <- list(
      field1 = list(name = "Authors", column = "AU"),
      field2 = list(name = "Keywords", column = "DE"),
      field3 = list(name = "Sources", column = "SO")
    )
  }
  
  three_field_data <- prepare_three_field_data(
    data,fields$field1$column,
    fields$field2$column,
    fields$field3$column,
    n_items
  )
  
  if (is.null(three_field_data)) {
    return(data.frame(
      from = character(),
      to = character(),
      value = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  three_field_data$flows
}

#' Render Authors-Keywords-Sources three-field plot (convenience wrapper)
#' @param data Bibliographic data frame
#' @param config Configuration list
#' @param n_items Number of top items per field
#' @return List with three-field plot
#' @export
render_m1_authors_keywords_sources <- function(data, config = biblio_config(), n_items = 10) {
  fields <- list(
    field1 = list(name = "Authors", column = "AU"),
    field2 = list(name = "Keywords", column = "DE"),
    field3 = list(name = "Sources", column = "SO")
  )
  
  render_m1_three_field(data, fields, config, n_items)
}

#' Render Countries-Keywords-Years three-field plot (convenience wrapper)
#' @param data Bibliographic data frame with AU_CO, DE, PY columns
#' @param config Configuration list
#' @param n_items Number of top items per field
#' @return List with three-field plot
#' @export
render_m1_countries_keywords_years <- function(data, config = biblio_config(), n_items = 10) {
  fields <- list(
    field1 = list(name = "Countries", column = "AU_CO"),
    field2 = list(name = "Keywords", column = "DE"),
    field3 = list(name = "Years", column = "PY")
  )
  
  render_m1_three_field(data, fields, config, n_items)
}