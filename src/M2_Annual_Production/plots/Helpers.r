# -------------------------------------------
# 📌 Helper Functions
# -------------------------------------------
calculate_residuals <- function(x, y, regression_data) {
  # Extract the best model information
  model_info <- regression_data$metric_regression
  model_function <- regression_data$models_regression[[model_info$model_name]]
  model_params <- parse_params(model_info$model_params)

  # Ensure valid function and parameters exist
  if (is.null(model_function) || length(model_params) == 0) {
    stop("[ERROR] No valid regression model found.")
  }

  # Compute predicted values (y_model_real) at actual time points
  y_model_real <- do.call(model_function, c(list(t = x), model_params))

  # Compute residuals
  residuals <- y - y_model_real

  # Compute squared and square-root differences
  squared_diff <- residuals^2
  sqrt_diff <- sqrt(abs(residuals))

  # Create and return a data frame with residual data
  residual_data <- data.frame(
    Year = x,
    Residual = residuals,
    Squared_Difference = squared_diff,
    Sqrt_Difference = sqrt_diff
  )

  return(residual_data)
}

find_peaks <- function(values, n = 5) {
  library(zoo)  # Required for rollapply
  
  # Identify local maxima: Compare value with neighbors
  is_peak <- function(x) {
    return(x[2] > x[1] & x[2] > x[3])
  }
  
  # Apply rolling window to detect peaks
  peaks <- rollapply(values, width = 3, FUN = is_peak, fill = FALSE, align = "center")
  
  # Get indices of detected peaks
  peak_indices <- which(peaks)
  
  # If no peaks detected, return empty result
  if (length(peak_indices) == 0) return(integer(0))
  
  # Sort peaks by prominence (descending order)
  peak_indices <- peak_indices[order(values[peak_indices], decreasing = TRUE)]
  
  # Return top N peaks
  return(peak_indices[seq_len(min(n, length(peak_indices)))])
}


# ✅ **Ensure Directory Exists & Is Clean**
create_clean_directory <- function(path) {
  if (dir.exists(path)) unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE)
}

# ✅ **Save a Plot in PNG & SVG Formats** 
m2_save_plot <- function(plot_obj, plot_metric_name, name, one_column_path, double_column_path) {
  one_col_plot <- plot_obj$getOneColumnPlot()
  double_col_plot <- plot_obj$getDoubleColumnPlot()

  for (format in c("png", "svg")) {
    ggsave(filename = file.path(one_column_path, paste0("M2_",plot_metric_name,"_", name, ".", format)), 
           plot = one_col_plot$plot, width = one_col_plot$width, height = one_col_plot$height, 
           dpi = one_col_plot$dpi, device = format)

    ggsave(filename = file.path(double_column_path, paste0("M2_",plot_metric_name,"_", name, ".", format)), 
           plot = double_col_plot$plot, width = double_col_plot$width, height = double_col_plot$height, 
           dpi = double_col_plot$dpi, device = format)
  }
}

# ✅ **Generate JSON Report**
m2_save_json_report <- function(plot_classes, json_output_path) {
  all_reports <- list()

  for (plot_obj in plot_classes) {
    name <- plot_obj$title
    report <- plot_obj$getReport()
    all_reports[[name]] <- fromJSON(report)
  }

  json_file_path <- file.path(json_output_path, "m1_regression_plots_report.json")
  write(toJSON(all_reports, pretty = TRUE, auto_unbox = TRUE), file = json_file_path)

  message("[INFO] All reports saved in: ", json_file_path)
  message("[INFO] All regression and residual plots saved successfully.")
}

