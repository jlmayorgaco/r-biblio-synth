# ============================================================================
# module_m2/render/m2_render_eda.R - EDA plots (FIXED)
# ============================================================================

#' @export
render_m2_eda <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"moving_averages" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()
  ma_data <- result$moving_averages

  if (length(ma_data) > 0) {
    combined <- do.call(rbind, lapply(names(ma_data), function(nm) {
      ma <- ma_data[[nm]]
      ma$Window <- paste0("MA-", gsub("ma_", "", nm))
      ma
    }))
    combined <- combined[!is.na(combined$articles), ]

    if (nrow(combined) > 0) {
      ec <- unlist(ieee_colors[1:length(unique(combined$Window))])
      plots$moving_averages <- ggplot2::ggplot(combined, ggplot2::aes(x = year, y = articles, color = Window)) +
        ggplot2::geom_line(linewidth = 0.6) +
        ggplot2::geom_point(size = 0.8) +
        ggplot2::scale_color_manual(values = ec) +
        ggplot2::labs(title = "Moving Averages Over Time", x = "Year", y = "Articles", color = "Window") +
        ieee_theme(base_size = 8)
    }
  }

  list(status = "success", plots = plots, tables = list())
}
