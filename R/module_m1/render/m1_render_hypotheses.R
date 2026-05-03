# ============================================================================
# m1_render_hypotheses.R - Hypothesis Testing Visualization
# ============================================================================

#' Render hypothesis testing results
#'
#' @param data Output from compute_m1_hypotheses
#' @param config Configuration list
#' @return List with hypothesis plots
#' @export
render_m1_hypotheses <- function(data, config = biblio_config()) {
  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(list(plots = list(), status = data$status %||% "error"))
  }

  plots <- list()
  summary_plot <- m1_create_hypothesis_summary_plot(data, config)
  if (!is.null(summary_plot)) {
    plots$summary <- summary_plot
  }

  list(
    plots = plots,
    status = "success"
  )
}

#' Create hypothesis summary plot
#' @keywords internal
m1_create_hypothesis_summary_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  summary <- data$summary
  if (is.null(summary)) return(NULL)

  plot_data <- data.frame(
    outcome = c("Rejected", "Failed to reject", "Inconclusive"),
    count = c(
      summary$n_rejected %||% 0,
      summary$n_failed_to_reject %||% 0,
      summary$n_inconclusive %||% 0
    ),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = outcome, y = count, fill = outcome)) +
    ggplot2::geom_col(show.legend = FALSE, alpha = 0.8) +
    ggplot2::labs(
      title = "Bibliometric Hypothesis Testing Summary",
      x = NULL,
      y = "Number of Hypotheses"
    ) +
    ieee_theme()
}

#' Build hypothesis test table
#' @export
build_m1_hypotheses_table <- function(data, config = biblio_config()) {
  empty <- list(
    status = "stub",
    table = tibble::tibble(),
    summary = list()
  )

  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(empty)
  }

  hypotheses <- data$hypotheses %||% list()
  if (length(hypotheses) == 0) {
    return(empty)
  }

  rows <- lapply(names(hypotheses), function(id) {
    item <- hypotheses[[id]]
    data.frame(
      hypothesis_id = id,
      hypothesis = item$hyphypothesis %||% item$hypothesis %||% NA_character_,
      result = item$result %||% NA_character_,
      interpretation = item$interpretation %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  table <- do.call(rbind, rows)
  rownames(table) <- NULL

  list(
    status = "success",
    table = tibble::as_tibble(table),
    summary = data$summary %||% list()
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
