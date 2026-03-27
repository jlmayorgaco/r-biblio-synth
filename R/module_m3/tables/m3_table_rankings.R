# ============================================================================
# m3_table_rankings.R - Rank diagnostics tables for M3
# ============================================================================

#' Build rank diagnostics tables
#'
#' @param rankings_data Output from \code{m3_compute_rankings}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{production}, \code{citations}, \code{summary}
#' @export
m3_table_rankings <- function(rankings_data, config = biblio_config()) {
  empty <- list(production = tibble::tibble(), citations = tibble::tibble(),
                summary = tibble::tibble())
  if (!is.list(rankings_data)) return(empty)

  production <- if (nrow(rankings_data$production_rankings) > 0) {
    rankings_data$production_rankings %>%
      dplyr::mutate(cumulative_share = round(cumsum(value) / sum(value) * 100, 2)) %>%
      dplyr::rename(Rank = rank, Country = label, Articles = value,
                    `Cum. Share (%)` = cumulative_share)
  } else tibble::tibble()

  citations <- if (nrow(rankings_data$citations_rankings) > 0) {
    rankings_data$citations_rankings %>%
      dplyr::mutate(cumulative_share = round(cumsum(value) / sum(value) * 100, 2)) %>%
      dplyr::rename(Rank = rank, Country = label, Citations = value,
                    `Cum. Share (%)` = cumulative_share)
  } else tibble::tibble()

  rs <- rankings_data$rank_summary
  summary <- tibble::tibble(
    Dimension               = c("Production", "Citations"),
    `Gini`                  = c(round(rs$production$gini, 4),
                                 round(rs$citations$gini,  4)),
    `Top-10% Share`         = c(round(rs$production$top_heavy, 4),
                                 round(rs$citations$top_heavy,  4))
  )

  list(production = production, citations = citations, summary = summary)
}
