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
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # Check if rnaturalearth is available
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    cli::cli_warn("rnaturalearth package not available, skipping world map")
    return(list(plots = list(), status = "error: rnaturalearth not available"))
  }
  
  # Get production data
  if (!is.null(data$production) && !is.null(data$production$production_summary)) {
    plots$production_map <- create_world_map_production(data, config)
  }
  
  # Get citation data
  if (!is.null(data$citations) && !is.null(data$citations$citation_summary)) {
    plots$citation_map <- create_world_map_citations(data, config)
  }
  
  # Get collaboration data
  if (!is.null(data$collaboration_indices) && !is.null(data$collaboration_indices$indices)) {
    plots$collaboration_map <- create_world_map_collaboration(data, config)
  }
  
  # Growth rate map
  if (!is.null(data$country_regressions)) {
    plots$growth_map <- create_world_map_growth(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create world map for production
#' @keywords internal
create_world_map_production <- function(data, config) {
  # Get world map data
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  # Get production data
  prod_data <- data$production$production_summary
  
  if (is.null(prod_data) || !("country" %in% names(prod_data))) {
    return(NULL)
  }
  
  # Normalize country names
  prod_data$country_norm <- normalize_for_map(prod_data$country)
  world$name_norm <- normalize_for_map(world$name)
  
  # Merge data
  world_data <- merge(world, prod_data, by.x = "name_norm", by.y = "country_norm", all.x = TRUE)
  
  # Create plot
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = n_articles), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#F7FBFF",
      high = "#084594",
      na.value = "grey90",
      name = "Articles",
      trans = "log10"
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(crs = "+proj=robin") +
    ggplot2::labs(
      title = "World Map: Publication Output by Country",
      subtitle = sprintf("Total countries: %d", sum(!is.na(world_data$n_articles)))
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
create_world_map_citations <- function(data, config) {
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  cit_data <- data$citations$citation_summary
  
  if (is.null(cit_data) || !("country" %in% names(cit_data))) {
    return(NULL)
  }
  
  cit_data$country_norm <- normalize_for_map(cit_data$country)
  world$name_norm <- normalize_for_map(world$name)
  
  world_data <- merge(world, cit_data, by.x = "name_norm", by.y = "country_norm", all.x = TRUE)
  
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = total_citations), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient(
      low = "#FFF5F0",
      high = "#A50F15",
      na.value = "grey90",
      name = "Citations",
      trans = "log10"
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
create_world_map_collaboration <- function(data, config) {
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  collab_data <- data$collaboration_indices$indices
  
  if (is.null(collab_data) || !("country" %in% names(collab_data))) {
    return(NULL)
  }
  
  collab_data$country_norm <- normalize_for_map(collab_data$country)
  world$name_norm <- normalize_for_map(world$name)
  
  # Use collaboration centrality if available
  fill_var <- if ("collaboration_centrality" %in% names(collab_data)) {
    "collaboration_centrality"
  } else if ("avg_salton_index" %in% names(collab_data)) {
    "avg_salton_index"
  } else {
    "n_documents"
  }
  
  world_data <- merge(world, collab_data, by.x = "name_norm", by.y = "country_norm", all.x = TRUE)
  
  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = !!ggplot2::sym(fill_var)), color = "white", linewidth = 0.1) +
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
create_world_map_growth <- function(data, config) {
  world <- tryCatch({
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(world)) return(NULL)
  
  reg_data <- data$country_regressions
  
  if (is.null(reg_data) || !is.list(reg_data)) {
    return(NULL)
  }
  
  # Extract growth rates
  growth_rates <- sapply(names(reg_data), function(cntry) {
    x <- reg_data[[cntry]]
    if (is.list(x) && !is.null(x$growth_rate)) {
      return(x$growth_rate)
    }
    NA
  })
  
  trend_directions <- sapply(names(reg_data), function(cntry) {
    x <- reg_data[[cntry]]
    if (is.list(x) && !is.null(x$trend_direction)) {
      return(x$trend_direction)
    }
    "unknown"
  })
  
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
  
  world_data <- merge(world, growth_df, by.x = "name_norm", by.y = "country_norm", all.x = TRUE)
  
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

`%||%` <- function(a, b) if (!is.null(a)) a else b