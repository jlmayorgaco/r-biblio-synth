# ============================================================================
# m3_table_experiments.R - Experiments summary tables for M3
# ============================================================================

#' Build experiments summary tables
#'
#' @param experiments_data Output from \code{m3_compute_experiments}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{quadrant}, \code{concentration}, \code{momentum}
#' @export
m3_table_experiments <- function(experiments_data, config = biblio_config()) {
  empty <- list(quadrant = tibble::tibble(), concentration = tibble::tibble(),
                momentum = tibble::tibble())
  if (!is.list(experiments_data)) return(empty)

  quadrant <- if (is.data.frame(experiments_data$quadrant) &&
                  nrow(experiments_data$quadrant) > 0) {
    experiments_data$quadrant %>%
      dplyr::mutate(avg_citations = round(avg_citations, 2)) %>%
      dplyr::arrange(quadrant, dplyr::desc(article_count)) %>%
      dplyr::rename(
        Country        = country,
        Articles       = article_count,
        `Total Cit.`   = total_citations,
        `Avg Cit.`     = avg_citations,
        Quadrant       = quadrant
      )
  } else tibble::tibble()

  concentration <- if (is.data.frame(experiments_data$concentration) &&
                       nrow(experiments_data$concentration) > 0) {
    experiments_data$concentration %>%
      dplyr::mutate(
        gini       = round(gini, 4),
        top1_share = round(top1_share * 100, 2)
      ) %>%
      dplyr::rename(
        `Top-k`           = top_k,
        `Gini`            = gini,
        `Top-1 Share (%)` = top1_share
      )
  } else tibble::tibble()

  momentum <- if (is.data.frame(experiments_data$momentum) &&
                  nrow(experiments_data$momentum) > 0) {
    experiments_data$momentum %>%
      dplyr::mutate(
        recent_mean    = round(recent_mean, 2),
        prior_mean     = round(prior_mean, 2),
        momentum_score = round(momentum_score, 3)
      ) %>%
      dplyr::rename(
        Country          = country,
        `Years`          = n_years,
        `Recent Mean`    = recent_mean,
        `Prior Mean`     = prior_mean,
        `Momentum Score` = momentum_score
      )
  } else tibble::tibble()

  list(quadrant = quadrant, concentration = concentration, momentum = momentum)
}
