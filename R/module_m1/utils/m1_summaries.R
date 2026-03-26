# ============================================================================
# m1_summaries.R - Summary helpers for M1 (FIXED)
# ============================================================================

#' @export
m1_empty_summary_table <- function() {
  tibble::tibble(metric = character(), value = character())
}

#' @export
m1_empty_rank_table <- function() {
  tibble::tibble(rank = integer(), label = character(), value = numeric())
}

#' @export
m1_compute_percentage <- function(count, total) {
  if (total == 0) return(0)
  round((count / total) * 100, 2)
}

#' Compute Lorenz curve values (FIXED)
#'
#' Standard Lorenz curve: X = cumulative % of entities, Y = cumulative % of values
#' For perfect equality: points lie on diagonal
#'
#' @param x Numeric vector of values (e.g., articles per country).
#' @return A data frame with cumulative_entities and cumulative_values columns.
#' @export
m1_compute_lorenz <- function(x) {
  x <- sort(x, decreasing = FALSE)  # Ascending for standard Lorenz
  n <- length(x)
  cumulative_entities <- (1:n) / n  # X-axis: 1/n, 2/n, ..., 1
  cumulative_values <- cumsum(x) / sum(x)  # Y-axis: cumulative proportion
  data.frame(
    cumulative_entities = c(0, cumulative_entities),
    cumulative_values   = c(0, cumulative_values)
  )
}

#' Compute Gini coefficient from Lorenz curve (FIXED)
#'
#' @param cumulative_entities Cumulative proportion of entities (X-axis).
#' @param cumulative_values Cumulative proportion of values (Y-axis).
#' @return Gini coefficient (0 = perfect equality, 1 = perfect inequality).
#' @export
m1_compute_gini <- function(cumulative_entities, cumulative_values) {
  n <- length(cumulative_entities)
  if (n < 2) return(0)
  # Trapezoidal integration under Lorenz curve
  area_under_curve <- sum(diff(cumulative_entities) *
                          (cumulative_values[-1] + cumulative_values[-n]) / 2)
  gini <- 1 - 2 * area_under_curve
  # Clamp to valid range [0, 1]
  max(0, min(1, gini))
}
