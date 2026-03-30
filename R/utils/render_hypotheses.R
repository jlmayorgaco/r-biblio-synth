# ============================================================================
# render_hypotheses.R - Render Hypothesis Results
# ============================================================================
# Creates visualization for hypothesis test results

#' Render hypothesis test results
#'
#' @param data Output from compute_*_hypotheses
#' @param config Configuration list
#' @param module_id Module identifier (M1, M2, M3)
#' @return List with plots
#' @export
render_hypotheses <- function(data, config = biblio_config(), module_id = "M1") {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  hypotheses <- data$hyphypotheses %||% data$hypotheses
  
  if (is.null(hypotheses) || length(hypotheses) == 0) {
    return(list(plots = list(), status = "error: no hypotheses"))
  }
  
  plots <- list()
  
  # Summary bar chart
  plots$summary <- create_hypothesis_summary_plot(hypotheses, module_id, config)
  
  # Detailed hypothesis plot
  plots$detailed <- create_detailed_hypothesis_plot(hypotheses, module_id, config)
  
  # Interpretation text
  plots$interpretation <- create_hypothesis_interpretation_plot(hypotheses, module_id, config)
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create summary bar chart
#' @keywords internal
create_hypothesis_summary_plot <- function(hypotheses, module_id, config) {
  results <- sapply(hypotheses, function(h) h$result)
  
  df <- data.frame(
    result = names(results),
    status = ifelse(results == "reject", "Rejected", 
                   ifelse(results == "fail_to_reject", "Not Rejected", "Inconclusive")),
    stringsAsFactors = FALSE
  )
  
  df$status <- factor(df$status, levels = c("Rejected", "Not Rejected", "Inconclusive"))
  
  colors <- c("Rejected" = "#A2142F", "Not Rejected" = "#77AC30", "Inconclusive" = "#7E2F8E")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = result, y = 1, fill = status)) +
    ggplot2::geom_col(width = 0.8, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors, name = "Result") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.3))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = sprintf("%s Hypothesis Test Results", module_id),
      subtitle = sprintf("n = %d hypotheses tested", length(hypotheses)),
      x = "Hypothesis",
      y = NULL
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
      legend.position = "bottom"
    )
  
  p
}

#' Create detailed hypothesis plot
#' @keywords internal
create_detailed_hypothesis_plot <- function(hypotheses, module_id, config) {
  df <- do.call(rbind, lapply(names(hypotheses), function(h) {
    x <- hypotheses[[h]]
    data.frame(
      hypothesis = gsub("H0._", "H", h),
      description = substr(x$hyphypothesis %||% x$hypothesis %||% h, 1, 60),
      result = x$result %||% "unknown",
      stringsAsFactors = FALSE
    )
  }))
  
  df$result_color <- ifelse(df$result == "reject", "Rejected",
                           ifelse(df$result == "fail_to_reject", "Not Rejected", "Inconclusive"))
  df$result_color <- factor(df$result_color, levels = c("Rejected", "Not Rejected", "Inconclusive"))
  
  colors <- c("Rejected" = "#A2142F", "Not Rejected" = "#77AC30", "Inconclusive" = "#7E2F8E")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = hypothesis, y = result_color, fill = result_color)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = colors, name = "Result") +
    ggplot2::scale_x_discrete(name = "Hypothesis") +
    ggplot2::scale_y_discrete(name = NULL) +
    ieee_theme() +
    ggplot2::labs(
      title = sprintf("%s Hypothesis Matrix", module_id),
      subtitle = "Green = Not Rejected, Red = Rejected, Purple = Inconclusive"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom"
    )
  
  p
}

#' Create interpretation text plot
#' @keywords internal
create_hypothesis_interpretation_plot <- function(hypotheses, module_id, config) {
  n_total <- length(hypotheses)
  n_rejected <- sum(sapply(hypotheses, function(h) h$result == "reject"), na.rm = TRUE)
  n_not_rejected <- sum(sapply(hypotheses, function(h) h$result == "fail_to_reject"), na.rm = TRUE)
  n_inconclusive <- n_total - n_rejected - n_not_rejected
  
  # Create a text grob with interpretation
  text_content <- sprintf(
    "%s Hypothesis Summary\n\nTotal: %d\nRejected: %d (%.0f%%)\nNot Rejected: %d (%.0f%%)\nInconclusive: %d (%.0f%%)",
    module_id,
    n_total,
    n_rejected, n_rejected / n_total * 100,
    n_not_rejected, n_not_rejected / n_total * 100,
    n_inconclusive, n_inconclusive / n_total * 100
  )
  
  # Return as data for table output rather than plot
  list(
    text = text_content,
    n_total = n_total,
    n_rejected = n_rejected,
    n_not_rejected = n_not_rejected,
    n_inconclusive = n_inconclusive
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b