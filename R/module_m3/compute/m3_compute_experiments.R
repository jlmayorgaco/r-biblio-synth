# ============================================================================
# m3_compute_experiments.R - Exploratory experiments for M3
# ============================================================================
# NOTE: All results in this file are exploratory and should be interpreted
# with appropriate caution. They are separated from core outputs intentionally.

#' Compute exploratory experiments for country-level data
#'
#' Includes: productivity-impact quadrant, concentration sensitivity,
#' rank stability, and temporal momentum scoring.
#' All results are marked exploratory.
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of exploratory experiment results
#' @export
m3_compute_experiments <- function(prepared_data, config = biblio_config()) {
  country_summary <- prepared_data$country_summary

  empty_result <- list(
    quadrant      = tibble::tibble(),
    concentration = tibble::tibble(),
    momentum      = tibble::tibble(),
    experiments_summary = list(
      quadrant_performed      = FALSE,
      concentration_performed = FALSE,
      momentum_performed      = FALSE,
      notes = character()
    ),
    status = "success: no country data",
    exploratory = TRUE
  )

  if (nrow(country_summary) == 0) {
    empty_result$status <- "error: no country data"
    return(empty_result)
  }

  notes <- character()

  # --- E1. Productivity-Impact Quadrant ---
  # Requires article_count and total_citations.
  quadrant <- tibble::tibble()
  quadrant_performed <- FALSE

  if ("article_count" %in% names(country_summary) &&
      "total_citations" %in% names(country_summary) &&
      nrow(country_summary) >= 2) {

    prod_med   <- stats::median(country_summary$article_count,   na.rm = TRUE)
    impact_med <- stats::median(country_summary$total_citations, na.rm = TRUE)

    quadrant <- country_summary %>%
      dplyr::mutate(
        avg_citations = ifelse(article_count > 0,
                               total_citations / article_count, 0),
        quadrant = dplyr::case_when(
          article_count >= prod_med & total_citations >= impact_med ~ "High-Output / High-Impact",
          article_count >= prod_med & total_citations <  impact_med ~ "High-Output / Low-Impact",
          article_count <  prod_med & total_citations >= impact_med ~ "Low-Output  / High-Impact",
          TRUE                                                       ~ "Low-Output  / Low-Impact"
        )
      ) %>%
      dplyr::select(country, article_count, total_citations, avg_citations, quadrant)

    quadrant_performed <- TRUE
  } else {
    notes <- c(notes, "Quadrant skipped: insufficient columns or < 2 countries.")
  }

  # --- E2. Concentration sensitivity to top-k truncation ---
  # Shows how Gini changes as we include fewer top countries.
  concentration <- tibble::tibble()
  concentration_performed <- FALSE

  if ("article_count" %in% names(country_summary) && nrow(country_summary) >= 4) {
    n_total   <- nrow(country_summary)
    k_values  <- unique(c(5L, 10L, 20L, n_total))
    k_values  <- k_values[k_values >= 2 & k_values <= n_total]

    rows <- lapply(k_values, function(k) {
      vals <- sort(country_summary$article_count, decreasing = TRUE)[seq_len(k)]
      gini <- .m3_gini_vec(vals)
      top_share <- if (k >= 2) vals[1] / sum(vals) else NA_real_
      tibble::tibble(top_k = k, gini = gini, top1_share = top_share)
    })

    concentration <- dplyr::bind_rows(rows)
    concentration_performed <- TRUE
  } else {
    notes <- c(notes, "Concentration sensitivity skipped: < 4 countries.")
  }

  # --- E3. Temporal momentum score ---
  # Requires annual data with >= 3 years per country.
  momentum <- tibble::tibble()
  momentum_performed <- FALSE

  country_annual <- prepared_data$country_annual

  if (nrow(country_annual) > 0) {
    # Momentum = mean of last 3 years / mean of prior years (requires >= 4 years)
    momentum_rows <- country_annual %>%
      dplyr::group_by(country) %>%
      dplyr::arrange(PY) %>%
      dplyr::summarise(
        n_years     = dplyr::n(),
        recent_mean = ifelse(
          dplyr::n() >= 3,
          mean(utils::tail(article_count, 3L), na.rm = TRUE),
          NA_real_
        ),
        prior_mean  = ifelse(
          dplyr::n() >= 4,
          mean(utils::head(article_count, max(1L, dplyr::n() - 3L)), na.rm = TRUE),
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_years >= 4) %>%
      dplyr::mutate(
        momentum_score = ifelse(
          !is.na(prior_mean) & prior_mean > 0,
          recent_mean / prior_mean,
          NA_real_
        )
      ) %>%
      dplyr::arrange(dplyr::desc(momentum_score))

    if (nrow(momentum_rows) > 0) {
      momentum <- momentum_rows
      momentum_performed <- TRUE
    } else {
      notes <- c(notes, "Momentum skipped: no country has >= 4 years of annual data.")
    }
  } else {
    notes <- c(notes, "Momentum skipped: no annual data available.")
  }

  list(
    quadrant      = quadrant,
    concentration = concentration,
    momentum      = momentum,
    experiments_summary = list(
      quadrant_performed      = quadrant_performed,
      concentration_performed = concentration_performed,
      momentum_performed      = momentum_performed,
      notes                   = notes
    ),
    status      = "success",
    exploratory = TRUE
  )
}

# Internal Gini helper (avoids cross-module dependency on M1 helpers)
.m3_gini_vec <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  cum_ent <- seq_len(n) / n
  cum_val <- cumsum(x) / sum(x)
  ent_ext <- c(0, cum_ent)
  val_ext <- c(0, cum_val)
  auc <- sum(diff(ent_ext) * (val_ext[-1] + val_ext[-length(val_ext)]) / 2)
  max(0, min(1, 1 - 2 * auc))
}
