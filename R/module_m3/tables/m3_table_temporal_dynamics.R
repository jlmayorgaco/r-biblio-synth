# ============================================================================
# m3_table_temporal_dynamics.R - Temporal dynamics tables for M3
# ============================================================================

#' Build temporal dynamics tables
#'
#' @param temporal_data Output from \code{m3_compute_temporal_dynamics}
#' @param config Configuration list
#' @return A list of tibbles for share trends, rank changes, and emergence
#' @export
m3_table_temporal_dynamics <- function(temporal_data, config = biblio_config()) {
  empty <- list(
    share_trends = tibble::tibble(),
    rank_changes = tibble::tibble(),
    emergence = tibble::tibble()
  )

  if (!is.list(temporal_data) || !identical(temporal_data$status, "success")) {
    return(empty)
  }

  share_trends <- if (is.list(temporal_data$share_evolution) &&
                      is.data.frame(temporal_data$share_evolution$share_trends) &&
                      nrow(temporal_data$share_evolution$share_trends) > 0) {
    temporal_data$share_evolution$share_trends %>%
      dplyr::mutate(
        country = m3_title_case_country(country),
        dplyr::across(c(first_share, last_share, change), ~ round(.x, 2))
      ) %>%
      dplyr::arrange(dplyr::desc(change)) %>%
      dplyr::rename(
        Country = country,
        `First Share (%)` = first_share,
        `Last Share (%)` = last_share,
        `Change (pp)` = change,
        Trend = trend
      )
  } else tibble::tibble()

  rank_changes <- if (is.list(temporal_data$rank_mobility) &&
                      is.list(temporal_data$rank_mobility$rank_changes) &&
                      is.data.frame(temporal_data$rank_mobility$rank_changes$changes) &&
                      nrow(temporal_data$rank_mobility$rank_changes$changes) > 0) {
    temporal_data$rank_mobility$rank_changes$changes %>%
      dplyr::mutate(country = m3_title_case_country(country)) %>%
      dplyr::arrange(dplyr::desc(abs(rank_change))) %>%
      dplyr::rename(
        Country = country,
        `Initial Rank` = rank_first,
        `Final Rank` = rank_last,
        `Rank Change` = rank_change,
        Direction = direction
      )
  } else tibble::tibble()

  emergence <- if (is.list(temporal_data$emergence) &&
                   !is.null(temporal_data$emergence$emergence_counts) &&
                   length(temporal_data$emergence$emergence_counts) > 0) {
    tibble::tibble(
      Year = as.numeric(names(temporal_data$emergence$emergence_counts)),
      new_countries = as.numeric(temporal_data$emergence$emergence_counts)
    ) %>%
      dplyr::filter(is.finite(Year), is.finite(new_countries)) %>%
      dplyr::rename(`New Countries` = new_countries) %>%
      dplyr::arrange(Year)
  } else tibble::tibble()

  list(
    share_trends = share_trends,
    rank_changes = rank_changes,
    emergence = emergence
  )
}
