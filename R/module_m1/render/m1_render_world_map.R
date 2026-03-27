# ============================================================================
# module_m1/render/m1_render_world_map.R - World map visualization
# ============================================================================

#' @export
render_m1_world_map <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_countries_by_articles" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  countries <- result$top_countries_by_articles
  if (nrow(countries) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()

  # Load world map data
  world <- tryCatch(
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
    error = function(e) NULL
  )

  if (!is.null(world)) {
    # Normalize country names for matching
    countries$name_clean <- toupper(trimws(countries$label))

    # Try to match using countrycode
    countries$iso3 <- tryCatch(
      countrycode::countrycode(countries$name_clean, "country.name", "iso3c"),
      error = function(e) rep(NA, nrow(countries))
    )

    # Merge with world data
    world$articles <- 0
    for (i in seq_len(nrow(countries))) {
      if (!is.na(countries$iso3[i])) {
        idx <- which(world$iso_a3 == countries$iso3[i])
        if (length(idx) > 0) world$articles[idx] <- countries$value[i]
      }
    }

    # Articles map
    plots$articles_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world, ggplot2::aes(fill = articles), color = "gray30", linewidth = 0.1) +
      ggplot2::scale_fill_viridis_c(option = "viridis", na.value = "gray90", name = "Articles") +
      ggplot2::labs(title = "Global Distribution of Articles") +
      ggplot2::theme_void(base_size = 8) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
        legend.position = "right"
      )

    # Citations map (if available)
    if ("top_countries_by_citations" %in% names(result)) {
      citations <- result$top_countries_by_citations
      citations$name_clean <- toupper(trimws(citations$label))
      citations$iso3 <- tryCatch(
        countrycode::countrycode(citations$name_clean, "country.name", "iso3c"),
        error = function(e) rep(NA, nrow(citations))
      )

      world$citations <- 0
      for (i in seq_len(nrow(citations))) {
        if (!is.na(citations$iso3[i])) {
          idx <- which(world$iso_a3 == citations$iso3[i])
          if (length(idx) > 0) world$citations[idx] <- citations$value[i]
        }
      }

      plots$citations_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = world, ggplot2::aes(fill = citations), color = "gray30", linewidth = 0.1) +
        ggplot2::scale_fill_viridis_c(option = "magma", na.value = "gray90", name = "Citations") +
        ggplot2::labs(title = "Global Distribution of Citations") +
        ggplot2::theme_void(base_size = 8) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5, family = "mono"),
          legend.position = "right"
        )
    }
  }

  list(status = "success", plots = plots, tables = list())
}
