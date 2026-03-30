# ============================================================================
# m2_render_growth_models.R - Growth Models Visualization
# ============================================================================

#' Render growth models comparison
#'
#' @param data Output from compute_m2_growth_models_wrapper
#' @param config Configuration list
#' @return List with plots
#' @export
render_m2_growth_models <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$comparison) && nrow(data$comparison) > 0) {
    plots$model_comparison <- create_growth_comparison_plot(data, config)
  }
  
  if (!is.null(data$bass) && data$bass$status == "success") {
    plots$bass <- create_bass_plot(data$bass, config)
  }
  
  if (!is.null(data$gompertz) && data$gompertz$status == "success") {
    plots$gompertz <- create_gompertz_plot(data$gompertz, config)
  }
  
  if (!is.null(data$weibull) && data$weibull$status == "success") {
    plots$weibull <- create_weibull_plot(data$weibull, config)
  }
  
  if (!is.null(data$richards) && data$richards$status == "success") {
    plots$richards <- create_richards_plot(data$richards, config)
  }
  
  plots$all_models <- create_all_growth_models_plot(data, config)
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create growth model comparison plot
#' @keywords internal
create_growth_comparison_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  comparison <- data$comparison
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)
  
  valid_rows <- comparison[!is.na(comparison$AIC), ]
  if (nrow(valid_rows) == 0) return(NULL)
  
  valid_rows$model <- factor(valid_rows$model, levels = valid_rows$model)
  
  ggplot2::ggplot(valid_rows, ggplot2::aes(x = model, y = AIC)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", AIC)), 
                        vjust = -0.5, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Growth Model Comparison (AIC)",
      x = "Model",
      y = "AIC"
    ) +ieee_theme()
}

#' Create Bass diffusion plot
#' @keywords internal
create_bass_plot <- function(bass_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  years <- bass_data$years
  observed <- bass_data$cumsum
  fitted <- bass_data$fitted
  
  df <- data.frame(
    year = rep(years, 2),
    value = c(observed, fitted),
    type = c(rep("Observed", length(years)), rep("Fitted", length(years)))
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, color = type)) +
    ggplot2::geom_point(data = subset(df, type == "Observed"), 
                        ggplot2::aes(x = year, y = value), 
                        color = "black", size = 2) +
    ggplot2::geom_line(data = subset(df, type == "Fitted"),
                        linewidth = 1, color = "#2166AC") +
    ggplot2::scale_color_manual(values = c("Observed" = "black", "Fitted" = "#2166AC")) +
    ggplot2::labs(
      title = sprintf("Bass Diffusion Model (R² = %.3f)", bass_data$R_squared),
      x = "Year",
      y = "Cumulative Articles",
      color = NULL
    ) +ieee_theme()
}

#' Create Gompertz plot
#' @keywords internal
create_gompertz_plot <- function(gompertz_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  years <- gompertz_data$years
  observed <- gompertz_data$cumsum
  fitted <- gompertz_data$fitted
  
  df <- data.frame(
    year = rep(years, 2),
    value = c(observed, fitted),
    type = c(rep("Observed", length(years)), rep("Fitted", length(years)))
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data.frame(year = years, value = observed),
                        ggplot2::aes(x = year, y = value),
                        color = "black", size = 2) +
    ggplot2::geom_line(data = data.frame(year = years, value = fitted),
                        ggplot2::aes(x = year, y = value),
                        color = "#67A9CF", linewidth = 1) +
    ggplot2::labs(
      title = sprintf("Gompertz Growth Model (R² = %.3f)", gompertz_data$R_squared),
      x = "Year",
      y = "Cumulative Articles"
    ) +ieee_theme()
}

#' Create Weibull plot
#' @keywords internal
create_weibull_plot <- function(weibull_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  years <- weibull_data$years
  observed <- weibull_data$cumsum
  fitted <- weibull_data$fitted
  
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data.frame(year = years, value = observed),
                        ggplot2::aes(x = year, y = value),
                        color = "black", size = 2) +
    ggplot2::geom_line(data = data.frame(year = years, value = fitted),
                        ggplot2::aes(x = year, y = value),
                        color = "#D95319", linewidth = 1) +
    ggplot2::labs(
      title = sprintf("Weibull Growth Model (R² = %.3f)", weibull_data$R_squared),
      x = "Year",
      y = "Cumulative Articles"
    ) +ieee_theme()
}

#' Create Richards plot
#' @keywords internal
create_richards_plot <- function(richards_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  years <- richards_data$years
  observed <- richards_data$cumsum
  fitted <- richards_data$fitted
  
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data.frame(year = years, value = observed),
                        ggplot2::aes(x = year, y = value),
                        color = "black", size = 2) +
    ggplot2::geom_line(data = data.frame(year = years, value = fitted),
                        ggplot2::aes(x = year, y = value),
                        color = "#77AC30", linewidth = 1) +
    ggplot2::labs(
      title = sprintf("Richards Growth Model (R² = %.3f)", richards_data$R_squared),
      x = "Year",
      y = "Cumulative Articles"
    ) +ieee_theme()
}

#' Create all growth models overlay plot
#' @keywords internal
create_all_growth_models_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  model_names <- c("bass", "gompertz", "weibull", "richards", "von_bertalanffy", "mmf")
  colors <- c("bass" = "#2166AC", "gompertz" = "#67A9CF", "weibull" = "#D95319",
              "richards" = "#77AC30", "von_bertalanffy" = "#A2142F", "mmf" = "#ED772F")
  
  df_list <- list()
  
  for (nm in model_names) {
    if (!is.null(data[[nm]]) && data[[nm]]$status == "success") {
      df_list[[nm]] <- data.frame(
        year = data[[nm]]$years,
        fitted = data[[nm]]$fitted,
        model = nm
      )
    }
  }
  
  if (length(df_list) == 0) return(NULL)
  
  df <- do.call(rbind, df_list)
  
  # Get observed data
  observed <- NULL
  for (nm in model_names) {
    if (!is.null(data[[nm]]) && data[[nm]]$status == "success") {
      observed <- data.frame(
        year = data[[nm]]$years,
        observed = data[[nm]]$cumsum
      )
      break
    }
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = observed, 
                        ggplot2::aes(x = year, y = observed),
                        color = "black", size = 2)
  
  for (nm in unique(df$model)) {
    subset_df <- df[df$model == nm, ]
    p <- p + ggplot2::geom_line(data = subset_df,
                                ggplot2::aes(x = year, y = fitted, color = model),
                                linewidth = 1)
  }
  
  p + ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = "Growth Models Comparison",
      x = "Year",
      y = "Cumulative Articles",
      color = "Model"
    ) +ieee_theme()
}

#' Build growth models table
#' @export
build_m2_growth_models_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame(
      model = character(),
      AIC = numeric(),
      BIC = numeric(),
      R_squared = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  data$comparison
}

`%||%` <- function(a, b) if (!is.null(a)) a else b