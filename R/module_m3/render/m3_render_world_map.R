# ============================================================================
# m3_render_world_map.R - World Map Visualization for Country Data
# ============================================================================
# Creates choropleth world maps for production, citations, collaboration

#' Render world map visualizations
#'
#' @param data Output from m3_compute_production or similar
#' @param config Configuration list
#' @return List with plots
#' @export
render_m3_world_map <- function(data, config = biblio_config()) {
  if (!is.list(data)) {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # Check if rnaturalearth is available
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    return(list(plots = list(), status = "stub: rnaturalearth not available"))
  }
  if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
    return(list(plots = list(), status = "stub: rnaturalearthdata not available"))
  }
  
  # Get production data
  prod_data <- m3_world_map_extract_table(
    data$production,
    "country_production",
    c("article_count", "n_articles", "value")
  )
  if (!is.null(prod_data)) {
    plots$production_map <- create_world_map_production(prod_data, config)
  }
  
  # Get citation data
  cit_data <- m3_world_map_extract_table(
    data$citations,
    "country_citations",
    c("total_citations", "citations", "value")
  )
  if (!is.null(cit_data)) {
    plots$citation_map <- create_world_map_citations(cit_data, config)
  }
  
  # Get collaboration data
  collab_data <- m3_world_map_extract_table(
    data$collaboration_indices,
    "indices",
    c("collaboration_centrality", "avg_salton_index", "article_count", "n_documents")
  )
  if (!is.null(collab_data)) {
    plots$collaboration_map <- create_world_map_collaboration(collab_data, config)
  }
  
  # Growth rate map
  reg_data <- m3_world_map_extract_regressions(data$country_regressions)
  if (!is.null(reg_data) && length(reg_data) > 0) {
    plots$growth_map <- create_world_map_growth(reg_data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create world map for production
#' @keywords internal
create_world_map_production <- function(prod_data, config) {
  # Get world map data
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  # Normalize country names
  prod_data$country_norm <- normalize_for_map(prod_data$country)
  world$name_norm <- normalize_for_map(world$name)
  
  # Merge data
  world_data <- dplyr::left_join(world, prod_data, by = c("name_norm" = "country_norm"))
  world_data$fill_value <- suppressWarnings(as.numeric(world_data$article_count))
  world_data$fill_value[!is.finite(world_data$fill_value) | world_data$fill_value <= 0] <- NA_real_
  
  # Create plot
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = fill_value), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#F7FBFF",
      high = "#084594",
      na.value = "grey90",
      name = "Articles",
      trans = scales::pseudo_log_trans(base = 10)
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(crs = "+proj=robin") +
    ggplot2::labs(
      title = "World Map: Publication Output by Country",
      subtitle = sprintf("Total countries: %d", sum(!is.na(world_data$fill_value)))
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.5, "inches"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  p
}

#' Create world map for citations
#' @keywords internal
create_world_map_citations <- function(cit_data, config) {
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  cit_data$country_norm <- normalize_for_map(cit_data$country)
  world$name_norm <- normalize_for_map(world$name)
  
  world_data <- dplyr::left_join(world, cit_data, by = c("name_norm" = "country_norm"))
  world_data$fill_value <- suppressWarnings(as.numeric(world_data$total_citations))
  world_data$fill_value[!is.finite(world_data$fill_value) | world_data$fill_value <= 0] <- NA_real_
  
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = fill_value), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#FFF5F0",
      high = "#A50F15",
      na.value = "grey90",
      name = "Citations",
      trans = scales::pseudo_log_trans(base = 10)
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(crs = "+proj=robin") +
    ggplot2::labs(
      title = "World Map: Citation Impact by Country",
      subtitle = "Total citations received"
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.5, "inches"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  p
}

#' Create world map for collaboration
#' @keywords internal
create_world_map_collaboration <- function(collab_data, config) {
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  collab_data$country_norm <- normalize_for_map(collab_data$country)
  world$name_norm <- normalize_for_map(world$name)
  
  # Use collaboration centrality if available
  fill_var <- if ("collaboration_centrality" %in% names(collab_data)) {
    "collaboration_centrality"
  } else if ("avg_salton_index" %in% names(collab_data)) {
    "avg_salton_index"
  } else {
    "article_count"
  }
  
  world_data <- dplyr::left_join(world, collab_data, by = c("name_norm" = "country_norm"))
  world_data$fill_value <- suppressWarnings(as.numeric(world_data[[fill_var]]))
  world_data$fill_value[!is.finite(world_data$fill_value) | world_data$fill_value < 0] <- NA_real_
  
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = fill_value), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#F7FCF5",
      high = "#006D2C",
      na.value = "grey90",
      name = "Collaboration"
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(crs = "+proj=robin") +
    ggplot2::labs(
      title = "World Map: International Collaboration Index",
      subtitle = "Strength of international collaboration"
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.5, "inches"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  p
}

#' Create world map for growth rates
#' @keywords internal
create_world_map_growth <- function(reg_data, config) {
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)

  if (!is.list(reg_data) || length(reg_data) == 0) {
    return(NULL)
  }
  
  # Extract growth rates
  growth_rates <- vapply(names(reg_data), function(cntry) {
    x <- reg_data[[cntry]]
    if (is.list(x) && !is.null(x$growth_rate)) {
      return(x$growth_rate)
    }
    NA_real_
  }, numeric(1))
  
  trend_directions <- vapply(names(reg_data), function(cntry) {
    x <- reg_data[[cntry]]
    if (is.list(x) && !is.null(x$trend_direction)) {
      return(x$trend_direction)
    }
    "unknown"
  }, character(1))
  
  growth_df <- data.frame(
    country = names(reg_data),
    growth_rate = growth_rates,
    trend = trend_directions,
    stringsAsFactors = FALSE
  )
  
  growth_df <- growth_df[!is.na(growth_df$growth_rate), ]
  
  if (nrow(growth_df) == 0) return(NULL)
  
  growth_df$country_norm <- normalize_for_map(growth_df$country)
  growth_df$trend_color <- ifelse(growth_df$trend == "increasing", "Growing",
                                   ifelse(growth_df$trend == "decreasing", "Declining", "Stable"))
  
  world$name_norm <- normalize_for_map(world$name)
  
  world_data <- dplyr::left_join(world, growth_df, by = c("name_norm" = "country_norm"))
  
  # Diverging color scale
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = growth_rate), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient2(
      low = "#B2182B",
      mid = "#F7F7F7",
      high = "#2166AC",
      midpoint = 0,
      na.value = "grey90",
      name = "Growth Rate\n(%/year)"
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(crs = "+proj=robin") +
    ggplot2::labs(
      title = "World Map: Research Growth Rate by Country",
      subtitle = "Positive = growing, Negative = declining"
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.5, "inches"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  p
}

#' Normalize country names for map matching
#' @keywords internal
normalize_for_map <- function(names) {
  names <- toupper(trimws(as.character(names)))
  names <- gsub("[[:punct:]]", " ", names)
  names <- gsub("\\s+", " ", names)
  names <- trimws(names)
  
  # Map to natural earth names
  name_map <- c(
    "UNITED STATES" = "United States of America",
    "USA" = "United States of America",
    "UK" = "United Kingdom",
    "UNITED KINGDOM" = "United Kingdom",
    "RUSSIA" = "Russia",
    "RUSSIAN FEDERATION" = "Russia",
    "SOUTH KOREA" = "South Korea",
    "KOREA" = "South Korea",
    "NORTH KOREA" = "North Korea",
    "IRAN" = "Iran",
    "CHINA" = "China",
    "TAIWAN" = "Taiwan",
    "VIETNAM" = "Vietnam",
    "LAOS" = "Laos",
    "CZECH REPUBLIC" = "Czechia",
    "CZECHIA" = "Czechia",
    "SLOVAK REPUBLIC" = "Slovakia",
    "MYANMAR" = "Myanmar",
    "BURMA" = "Myanmar"
  )
  
  for (variant in names(name_map)) {
    names[names == variant] <- name_map[variant]
  }

  tools::toTitleCase(tolower(names))
}

#' Extract map-ready table from an M3 section
#' @keywords internal
m3_world_map_extract_table <- function(section, table_name, metric_candidates) {
  if (!is.list(section) || !identical(section$status %||% NA_character_, "success")) {
    return(NULL)
  }

  df <- section[[table_name]]
  if (!is.data.frame(df) || nrow(df) == 0 || !("country" %in% names(df))) {
    return(NULL)
  }

  metric_name <- metric_candidates[metric_candidates %in% names(df)][1]
  if (is.na(metric_name) || is.null(metric_name)) {
    return(NULL)
  }

  df$article_count <- if ("article_count" %in% names(df)) {
    suppressWarnings(as.numeric(df$article_count))
  } else if ("n_articles" %in% names(df)) {
    suppressWarnings(as.numeric(df$n_articles))
  } else if ("n_documents" %in% names(df)) {
    suppressWarnings(as.numeric(df$n_documents))
  } else {
    suppressWarnings(as.numeric(df[[metric_name]]))
  }

  df$total_citations <- if ("total_citations" %in% names(df)) {
    suppressWarnings(as.numeric(df$total_citations))
  } else {
    suppressWarnings(as.numeric(df[[metric_name]]))
  }

  df
}

#' Extract country regression fits for world maps
#' @keywords internal
m3_world_map_extract_regressions <- function(section) {
  if (!is.list(section) || !identical(section$status %||% NA_character_, "success")) {
    return(NULL)
  }

  fits <- section$country_regressions
  if (!is.list(fits) || length(fits) == 0) {
    return(NULL)
  }

  fits
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
