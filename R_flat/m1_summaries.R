# ============================================================================
# m1_summaries.R - Summary helpers for M1
# ============================================================================

#' Create an empty summary table
#'
#' @return An empty tibble.
#' @export
m1_empty_summary_table <- function() {
  tibble::tibble(
    metric = character(),
    value  = character()
  )
}

#' Create an empty rank table
#'
#' @return An empty tibble.
#' @export
m1_empty_rank_table <- function() {
  tibble::tibble(
    rank  = integer(),
    label = character(),
    value = numeric()
  )
}

#' Compute percentage
#'
#' @param count Numeric count.
#' @param total Numeric total.
#' @return Percentage as numeric.
#' @export
m1_compute_percentage <- function(count, total) {
  if (total == 0) return(0)
  round((count / total) * 100, 2)
}

#' Compute Lorenz curve values
#'
#' @param x Numeric vector (sorted descending).
#' @return A data frame with cumulative_x and cumulative_y.
#' @export
m1_compute_lorenz <- function(x) {
  x <- sort(x, decreasing = TRUE)
  n <- length(x)
  cumulative_x <- cumsum(x) / sum(x)
  cumulative_y <- seq(0, 1, length.out = n)
  data.frame(
    cumulative_x = c(0, cumulative_x),
    cumulative_y = c(0, cumulative_y)
  )
}

#' Compute Gini coefficient from Lorenz curve
#'
#' @param cumulative_x Cumulative proportion of x.
#' @param cumulative_y Cumulative proportion of y.
#' @return Gini coefficient.
#' @export
m1_compute_gini <- function(cumulative_x, cumulative_y) {
  n <- length(cumulative_x)
  if (n < 2) return(0)
  area_trapezoids <- (cumulative_x[-1] + cumulative_x[-n]) * diff(cumulative_y) / 2
  area_under_curve <- sum(area_trapezoids)
  1 - 2 * area_under_curve
}
