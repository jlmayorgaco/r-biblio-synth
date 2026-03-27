# ============================================================================
# m1_summaries.R - Summary helpers (WITH SMOOTH LORENZ)
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

#' Compute Lorenz curve with interpolation for smooth visualization
#'
#' @param x Numeric vector of values.
#' @param smooth Logical. If TRUE, interpolates for smoother curve.
#' @param n_interp Number of interpolation points.
#' @return Data frame with cumulative_entities and cumulative_values.
#' @export
m1_compute_lorenz <- function(x, smooth = TRUE, n_interp = 200) {
  x <- sort(x, decreasing = FALSE)
  n <- length(x)

  cumulative_entities <- (1:n) / n
  cumulative_values <- cumsum(x) / sum(x)

  # Raw points
  entities_raw <- c(0, cumulative_entities)
  values_raw <- c(0, cumulative_values)

  # Interpolate for smooth curve
  if (smooth && length(entities_raw) >= 4) {
    smooth_x <- seq(0, 1, length.out = n_interp)
    tryCatch({
      smooth_y <- stats::spline(entities_raw, values_raw, xout = smooth_x, method = "hyman")$y
    }, error = function(e) {
      smooth_y <<- stats::approx(entities_raw, values_raw, xout = smooth_x)$y
    })
    # Clamp to [0, 1]
    smooth_y <- pmax(0, pmin(1, smooth_y))
    data.frame(cumulative_entities = smooth_x, cumulative_values = smooth_y)
  } else {
    data.frame(cumulative_entities = entities_raw, cumulative_values = values_raw)
  }
}

#' Compute Gini coefficient from Lorenz curve
#' @param cumulative_entities Cumulative proportion of entities.
#' @param cumulative_values Cumulative proportion of values.
#' @return Gini coefficient [0, 1].
#' @export
m1_compute_gini <- function(cumulative_entities, cumulative_values) {
  n <- length(cumulative_entities)
  if (n < 2) return(0)
  area_under_curve <- sum(diff(cumulative_entities) *
                          (cumulative_values[-1] + cumulative_values[-n]) / 2)
  max(0, min(1, 1 - 2 * area_under_curve))
}
