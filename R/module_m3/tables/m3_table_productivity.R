# ============================================================================
# m3_table_productivity.R - Production tables for M3
# ============================================================================

#' Build country production tables
#'
#' @param production_data Output from \code{m3_compute_production}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{full} and \code{top}
#' @export
m3_table_productivity <- function(production_data, config = biblio_config()) {
  empty <- list(full = tibble::tibble(), top = tibble::tibble())
  if (!is.list(production_data) || nrow(production_data$country_production) == 0) {
    return(empty)
  }

  full <- production_data$country_production %>%
    dplyr::mutate(
      share            = round(share * 100, 2),
      cumulative_share = round(cumulative_share * 100, 2)
    ) %>%
    dplyr::rename(
      Country          = country,
      Articles         = article_count,
      `Share (%)`      = share,
      `Cum. Share (%)` = cumulative_share
    )

  top <- production_data$top_countries_by_production %>%
    dplyr::mutate(
      share            = round(share * 100, 2),
      cumulative_share = round(cumulative_share * 100, 2)
    ) %>%
    dplyr::rename(
      Rank             = rank,
      Country          = label,
      Articles         = value,
      `Share (%)`      = share,
      `Cum. Share (%)` = cumulative_share
    )

  list(full = full, top = top)
}
