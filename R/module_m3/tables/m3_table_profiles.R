# ============================================================================
# m3_table_profiles.R - Country profile tables for M3
# ============================================================================

#' Build country profile feature table
#'
#' @param profiles_data Output from \code{m3_compute_country_profiles}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{features} and \code{profiles}
#' @export
m3_table_profiles <- function(profiles_data, config = biblio_config()) {
  empty <- list(features = tibble::tibble(), profiles = tibble::tibble())
  if (!is.list(profiles_data)) return(empty)

  features <- if (nrow(profiles_data$country_features) > 0) {
    profiles_data$country_features %>%
      dplyr::mutate(
        average_citations = round(average_citations, 2),
        mcp_ratio         = round(mcp_ratio, 1)
      ) %>%
      dplyr::rename(
        Country           = country,
        Articles          = article_count,
        `Total Citations` = total_citations,
        `Avg Citations`   = average_citations,
        SCP               = scp,
        MCP               = mcp,
        `MCP Ratio (%)`   = mcp_ratio
      )
  } else tibble::tibble()

  # Profiles: drop z-score columns for display, keep country + cluster + PCs
  profiles <- if (nrow(profiles_data$country_profiles) > 0) {
    keep_cols <- c("country",
                   grep("_z$", names(profiles_data$country_profiles),
                        value = TRUE, invert = TRUE))
    profiles_data$country_profiles %>%
      dplyr::select(dplyr::any_of(keep_cols)) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
  } else tibble::tibble()

  list(features = features, profiles = profiles)
}
