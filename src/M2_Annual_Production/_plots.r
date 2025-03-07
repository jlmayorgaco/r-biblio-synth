########################################################################################
#' Create and save regression plots for article data
#'
#' @param metric_regression List containing regression model information
#' @param x Vector of time points (years)
#' @param y Vector of observed values (number of articles)
#' @param output_path Directory path for saving output files
#' @param models_regression List of model functions
#' @param lower_bound Optional vector of lower confidence bound values
#' @param upper_bound Optional vector of upper confidence bound values
#'
#' @return List of created plots
#' @export
########################################################################################

create_regression_articles_small_plots <- function(metric_regression, x, y, output_path,  models_regression, lower_bound = NULL,  upper_bound = NULL) {
  
  # Validate inputs
  validate_inputs(metric_regression, x, y, output_path)
  
  # Extract model information
  model_info <- extract_model_info(metric_regression, models_regression)
  
  # Log model information
  log_model_info(model_info)
  
  # Prepare data frames for plotting
  plot_data <- prepare_plot_data(x, y, model_info)
  
  # Create main regression plot
  main_plot <- create_main_regression_plot(
    plot_data, 
    model_info, 
    lower_bound, 
    upper_bound,
    metric_regression
  )
  
  # Create residual analysis plots
  residual_data <- calculate_residuals(x, y, model_info)
  residual_plots <- create_residual_plots(residual_data, x)
  
  # Save all plots
  save_plots(
    main_plot = main_plot,
    residual_plots = residual_plots,
    residual_data = residual_data,
    output_path = output_path
  )
  
  # Run statistical tests and save results
  # run_statistical_tests(residual_data, output_path)
  
  # Return all created plots as a list
  return(list(
    main_plot = main_plot,
    residual_plots = residual_plots
  ))
}

#' Validate function inputs
#'
#' @param metric_regression Regression metrics
#' @param x Time points
#' @param y Observed values
#' @param output_path Output directory
validate_inputs <- function(metric_regression, x, y, output_path) {
  if (is.null(metric_regression)) {
    stop("[ERROR] metric_regression is NULL.")
  }
  
  if (is.null(x) || length(x) == 0) {
    stop("[ERROR] x (time points) is NULL or empty.")
  }
  
  if (is.null(y) || length(y) == 0) {
    stop("[ERROR] y (observed values) is NULL or empty.")
  }
  
  if (is.null(output_path) || !dir.exists(output_path)) {
    stop("[ERROR] output_path is NULL or directory doesn't exist.")
  }
}

#' Extract model information from regression metrics
#'
#' @param metric_regression Regression metrics
#' @param models_regression List of model functions
#' @return List containing model function and parameters
extract_model_info <- function(metric_regression, models_regression) {
  list(
    model_name = metric_regression$model_name,
    model_function = models_regression[[metric_regression$model_name]],
    params = parse_params(metric_regression$model_params),
    r_squared = metric_regression$model_r_squared
  )
}

#' Log model information
#'
#' @param model_info List containing model information
log_model_info <- function(model_info) {
  message("\n[INFO] Running Regression Plot")
  message("[DEBUG] Model:", model_info$model_name)
  message("[DEBUG] R² Value:", model_info$r_squared)
}

#' Prepare data frames for plotting
#'
#' @param x Time points (years)
#' @param y Observed values (articles)
#' @param model_info Model information
#' @return List of data frames for plotting
prepare_plot_data <- function(x, y, model_info) {
  # Extract real data points
  t_real <- x
  y_real <- y
  
  # Generate regression line points
  t_regression <- seq(min(t_real), max(t_real), by = 0.1)
  y_regression <- do.call(model_info$model_function, 
                          c(list(t = t_regression), model_info$params))
  
  # Create data frames
  df_real <- data.frame(Year = t_real, Articles = y_real)
  df_regression <- data.frame(Year = t_regression, Articles = y_regression)
  
  list(
    t_real = t_real,
    y_real = y_real,
    t_regression = t_regression,
    y_regression = y_regression,
    df_real = df_real,
    df_regression = df_regression
  )
}

#' Create main regression plot
#'
#' @param plot_data Prepared data for plotting
#' @param model_info Model information
#' @param lower_bound Optional lower confidence bound
#' @param upper_bound Optional upper confidence bound
#' @return ggplot object
create_main_regression_plot <- function(plot_data, model_info, lower_bound, upper_bound, metric_regression) {
  # Extract data for easier access
  df_real <- plot_data$df_real
  df_regression <- plot_data$df_regression
  t_real <- plot_data$t_real
  y_real <- plot_data$y_real
  
  # Initialize plot
  p <- ggplot() +
    geom_point(data = df_real, aes(x = Year, y = Articles), size = 2, color = "black") +
    geom_line(data = df_regression, aes(x = Year, y = Articles), 
              color = "black", linewidth = 0.5, linetype = "dashed") +
    labs(
      title = "Annual Articles Regression Plot",
      x = "Year",
      y = "Number of Articles"
    ) +
    apply_plot_theme()
  
  # Add model name annotation
  p <- add_model_name_annotation(p, model_info$model_name, t_real, y_real)
  
  # Add R² value annotation
  p <- add_r_squared_annotation(p, model_info$r_squared, t_real, y_real)
  
  # Add confidence interval if provided
  p <- add_confidence_interval(p, lower_bound, upper_bound)
  
  # Add supporting features
  p <- add_plot_model_supporting_features_small(p, metric_regression, t_real, y_real)
  
  # Apply axis scales
  p <- apply_axis_scales(p, t_real, y_real)
  
  return(p)
}

#' Apply consistent theme to plots
#'
#' @return theme object
apply_plot_theme <- function() {
  theme_minimal(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(linewidth = 0.4, color = "#dddddd"),
      panel.grid.minor = element_line(linewidth = 0.2, color = "#f1f1f1"),
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold")
    )
}

#' Add model name annotation to plot
#'
#' @param p ggplot object
#' @param model_name Name of the model
#' @param t_real Vector of time points
#' @param y_real Vector of observed values
#' @return Updated ggplot object
add_model_name_annotation <- function(p, model_name, t_real, y_real) {
  if (!is.na(min(t_real)) && !is.na(max(y_real)) && !is.null(model_name)) {
    p <- p + geom_text(
      aes(x = min(t_real) + 0, y = min(y_real) + 20, label = model_name),
      hjust = 0, vjust = 1, size = 2.5, color = "black"
    )
    message("[INFO] Model name annotation added successfully.")
  } else {
    message("[WARNING] Model name annotation skipped due to missing values.")
  }
  return(p)
}

#' Add R² value annotation to plot
#'
#' @param p ggplot object
#' @param r_squared R-squared value
#' @param t_real Vector of time points
#' @param y_real Vector of observed values
#' @return Updated ggplot object
add_r_squared_annotation <- function(p, r_squared, t_real, y_real) {
  if (!is.null(r_squared) && !is.na(r_squared)) {
    label_r2 <- paste0("R^2 == ", sprintf("%.3f", r_squared))
    p <- p + geom_text(
      aes(x = min(t_real) + 0, y = min(y_real) + 32, label = label_r2),
      hjust = 0, vjust = 1, size = 2.5, color = "black", parse = TRUE
    )
    message("[INFO] R-squared annotation added successfully.")
  } else {
    message("[WARNING] R-squared annotation skipped due to missing value.")
  }
  return(p)
}

#' Add confidence interval to plot if available
#'
#' @param p ggplot object
#' @param lower_bound Vector of lower confidence bound values
#' @param upper_bound Vector of upper confidence bound values
#' @return Updated ggplot object
add_confidence_interval <- function(p, lower_bound, upper_bound) {
  if (!is.null(lower_bound) && !is.null(upper_bound)) {
    p <- p + geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), 
                          fill = "lightblue", alpha = 0.3)
    message("[INFO] Confidence interval added to the plot.")
  }
  return(p)
}

#' Apply consistent axis scales to plot
#'
#' @param p ggplot object
#' @param t_real Vector of time points
#' @param y_real Vector of observed values
#' @return Updated ggplot object
apply_axis_scales <- function(p, t_real, y_real) {
  p + 
    # Ensure Y-axis starts at 0 and extends to max(y_real)
    scale_y_continuous(
      limits = c(0, max(y_real)),  # Set y-axis range from 0 to max
      breaks = scales::extended_breaks()(y_real),
      minor_breaks = scales::extended_breaks(n = 10)(y_real)
    ) +
    # Ensure X-axis spans from min(t_real) to max(t_real)
    scale_x_continuous(
      limits = c(min(t_real), max(t_real)),  # Set x-axis range from min to max
      breaks = scales::extended_breaks()(t_real),
      minor_breaks = scales::extended_breaks(n = 10)(t_real)
    )
}

#' Calculate residuals for model evaluation
#'
#' @param x Time points
#' @param y Observed values
#' @param model_info Model information
#' @return Data frame with residual information
calculate_residuals <- function(x, y, model_info) {
  # Extract data
  t_real <- x
  y_real <- y
  
  # Calculate model predictions at actual time points
  y_model_real <- do.call(model_info$model_function, 
                          c(list(t = t_real), model_info$params))
  
  # Calculate differences
  squared_diff <- (y_real - y_model_real)^2
  sqrt_diff <- sqrt(abs(y_real - y_model_real))
  
  # Create data frame
  data.frame(
    Year = t_real, 
    Squared_Difference = squared_diff, 
    Sqrt_Difference = sqrt_diff,
    Residual = y_real - y_model_real
  )
}

#' Create residual analysis plots
#'
#' @param residual_data Data frame with residual information
#' @param x Time points
#' @return List of ggplot objects
create_residual_plots <- function(residual_data, x) {
  # Create error plot
  p_diff <- create_error_plot(residual_data, x)
  
  # Create histogram of residuals
  p_hist <- ggplot(residual_data, aes(x = Sqrt_Difference)) +
    geom_histogram(color = "black", fill = "skyblue", bins = 30) +
    labs(title = "Histogram of Regression Residuals", 
         x = "Residuals", y = "Frequency") +
    theme_minimal()
  
  # Create Q-Q plot
  p_qq <- ggplot(residual_data, aes(sample = Sqrt_Difference)) +
    stat_qq() + stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot of Residuals", 
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Create ACF plot
  p_acf <- autoplot(acf(residual_data$Sqrt_Difference, plot = FALSE)) + 
    labs(title = "Autocorrelation Function (ACF) of Residuals") +
    theme_minimal()
  
  # Create PACF plot
  p_pacf <- autoplot(pacf(residual_data$Sqrt_Difference, plot = FALSE)) + 
    labs(title = "Partial Autocorrelation Function (PACF) of Residuals") +
    theme_minimal()
  
  # Return all plots as a list
  list(
    error_plot = p_diff,
    histogram = p_hist,
    qq_plot = p_qq,
    acf_plot = p_acf,
    pacf_plot = p_pacf
  )
}

#' Create error plot showing square root differences
#'
#' @param residual_data Data frame with residual information
#' @param x Time points
#' @return ggplot object
create_error_plot <- function(residual_data, x) {
  t_real <- x
  
  ggplot() +
    geom_line(data = residual_data, aes(x = Year, y = Sqrt_Difference), 
              linewidth = 1, color = "blue") +
    geom_point(data = residual_data, aes(x = Year, y = Sqrt_Difference), 
               size = 2, color = "blue") +
    labs(
      title = "Square Root Differences (Regression Error) Plot",
      x = "Year",
      y = "Difference Values"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(linewidth = 0.4, color = "#dddddd"),
      panel.grid.minor = element_line(linewidth = 0.2, color = "#f1f1f1"),
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold")
    ) +
    scale_y_continuous(
      limits = c(0, max(residual_data$Sqrt_Difference)),  
      breaks = scales::extended_breaks()(residual_data$Sqrt_Difference),
      minor_breaks = scales::extended_breaks(n = 10)(residual_data$Sqrt_Difference)
    ) +
    scale_x_continuous(
      limits = c(min(t_real), max(t_real)),  
      breaks = scales::extended_breaks()(t_real),
      minor_breaks = scales::extended_breaks(n = 10)(t_real)
    )
}

#' Save all plots to specified output path
#'
#' @param main_plot Main regression plot
#' @param residual_plots List of residual analysis plots
#' @param residual_data Data frame with residual information
#' @param output_path Directory path for saving output files
save_plots <- function(main_plot, residual_plots, residual_data, output_path) {
  # Save main regression plot
    ggsave(
    filename = file.path(output_path, "Small_Regression_Articles_Plot.png"),
    plot = main_plot,
    width = 3.5,      # Width for a single-column plot in IEEE format
    height = 2.625,   # Aspect ratio of 4:3 to keep the plot balanced
    dpi = 900         # High resolution for print quality
    )
  
  ggsave(
    filename = file.path(output_path, "Small_Regression_Articles_Plot.svg"), 
    plot = main_plot, 
    width = 3.5,      # Width for a single-column plot in IEEE format
    height = 2.625,   # Aspect ratio of 4:3 to keep the plot balanced
    device = "svg"
  )
  
  message("[INFO] Regression plot saved successfully.")
  
  # Save residual error plot
  ggsave(
    filename = file.path(output_path, "Small_Squared_Differences_Plot.png"), 
    plot = residual_plots$error_plot, 
    width = 3.16, 
    height = 2.5, 
    dpi = 900
  )
  
  ggsave(
    filename = file.path(output_path, "Small_Squared_Differences_Plot.svg"), 
    plot = residual_plots$error_plot, 
    width = 3.16, 
    height = 2.5, 
    device = "svg"
  )
  
  message("[INFO] Squared differences plot saved successfully.")
  
  # Save histogram and Q-Q plot
  ggsave(
    file.path(output_path, "Small_Histogram_Residuals.png"), 
    plot = residual_plots$histogram, 
    dpi = 900
  )
  
  ggsave(
    file.path(output_path, "Small_QQ_Plot_Residuals.png"), 
    plot = residual_plots$qq_plot, 
    dpi = 900
  )
  
  message("[INFO] Residual histogram and Q-Q plot saved successfully.")
  
  # Save ACF and PACF plots
  ggsave(
    file.path(output_path, "Small_ACF_Residuals.png"), 
    plot = residual_plots$acf_plot, 
    dpi = 900
  )
  
  ggsave(
    file.path(output_path, "Small_PACF_Residuals.png"), 
    plot = residual_plots$pacf_plot, 
    dpi = 900
  )
  
  message("[INFO] ACF and PACF plots saved successfully.")
}

#' Run statistical tests on residuals and save results
#'
#' @param residual_data Data frame with residual information
#' @param output_path Directory path for saving output files
run_statistical_tests <- function(residual_data, output_path) {
  # Load required libraries
  require(lmtest)
  require(ggfortify)
  
  # Run Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(residual_data$Sqrt_Difference)
  message("[INFO] Shapiro-Wilk Test for Normality")
  message("[INFO] W-statistic: ", shapiro_test$statistic)
  message("[INFO] p-value: ", shapiro_test$p.value)
  
  # Interpretation
  if (shapiro_test$p.value < 0.05) {
    message("[WARNING] Residuals are NOT normally distributed (p < 0.05). Consider further analysis.")
  } else {
    message("[INFO] Residuals appear to be normally distributed (p >= 0.05).")
  }
  
  # Run Durbin-Watson test for autocorrelation
  dw_test <- dwtest(Sqrt_Difference ~ Year, data = residual_data)
  message("[INFO] Durbin-Watson test result: DW Statistic = ", 
          dw_test$statistic, ", p-value = ", dw_test$p.value)
  
  # Run Breusch-Pagan test for heteroscedasticity
  bp_test <- bptest(Sqrt_Difference ~ Year, data = residual_data)
  message("[INFO] Breusch-Pagan test result: BP Statistic = ", 
          bp_test$statistic, ", p-value = ", bp_test$p.value)
  
  # Run additional statistical tests and save to JSON
  stat_results <- perform_statistical_tests(residual_data, "Sqrt_Difference", output_path)
  
  # Convert to JSON-friendly format
  stat_results_json <- toJSON(stat_results, pretty = TRUE, auto_unbox = TRUE)
  
  # Modify output_path: Replace "figures" with "jsons"
  json_output_path <- gsub("figures", "jsons", output_path)
  
  # Save JSON file
  write(stat_results_json, 
        file = file.path(json_output_path, "m2_regression_model_stats_result.json"))
}




add_plot_model_supporting_features_small <- function(p, metric_regression, t_real, y_real) {
  
  library(ggplot2)
  library(rsvg)
  library(grid)

  # Ensure `p` is a valid ggplot object
  if (!inherits(p, "ggplot")) {
    stop("[ERROR] `p` is NOT a ggplot object. Check its construction.")
  }

  # Ensure `metric_regression` is valid
  if (is.null(metric_regression) || !"model_name" %in% names(metric_regression)) {
    stop("[ERROR] `metric_regression` is NULL or missing `model_name`.")
  }

  # Check if this is the Gompertz model
  if (metric_regression$model_name == 'Gompertz') {
    
    # Extract parameters safely
    extract_params <- function(param_string) {
      param_list <- strsplit(param_string, ", ")[[1]]  
      param_dict <- list()
      for (param in param_list) {
        parts <- strsplit(param, " = ")[[1]]  
        if (length(parts) == 2) {
          key <- trimws(parts[1])  
          value <- as.numeric(parts[2])  
          param_dict[[key]] <- value  
        }
      }
      return(param_dict)
    }

    model_params <- extract_params(metric_regression$best_model$params)

    # Validate parameter extraction
    if (!all(c("N0", "t0", "Nmax", "k") %in% names(model_params))) {
      stop("[ERROR] Missing required parameters `N0`, `t0`, `Nmax`, or `k`. Check parameter extraction.")
    }

    # Extract parameter values
    N0 <- model_params[["y0"]] + model_params[["N0"]]
    t0 <- model_params[["t0"]]

    # Compute `tr`
    tr_index <- which(y_real >= 0.9 * N0)[1]
    tr <- if (!is.na(tr_index)) t_real[tr_index] else NA

    # Ensure `THEME_COLORS` exists
    if (!exists("THEME_COLORS") || !is.list(THEME_COLORS)) {
      stop("[ERROR] `THEME_COLORS` is not defined. Make sure you have a valid color scheme.")
    }

    # Add Features to Plot
    p <- p +
    geom_vline(xintercept = t0, linetype = "dashed", color = THEME_COLORS$Main[1]) +
    geom_text(
        data = data.frame(x = t0, y = max(y_real) - 24),
        aes(x = x, y = y, label = "t0"), 
        color = THEME_COLORS$Main[1], angle = 90, vjust = -0.5, hjust = -0.2,
        size = 7 / .pt  # Adjust font size for annotations (7 pt)
    )

    if (!is.na(tr)) {
    p <- p +
        geom_vline(xintercept = tr, linetype = "dashed", color = THEME_COLORS$Main[3]) +
        geom_text(
        data = data.frame(x = tr, y = max(y_real) - 23),
        aes(x = x, y = y, label = "tr"), 
        color = THEME_COLORS$Main[3], angle = 90, vjust = -0.5, hjust = -0.2,
        size = 7 / .pt  # Adjust font size for annotations (7 pt)
        )
    }

    p <- p +
    geom_hline(yintercept = N0, linetype = "dashed", color = THEME_COLORS$Main[5]) +
    geom_text(
        data = data.frame(x = min(t_real), y = N0),
        aes(x = x, y = y, label = "N0")), 
        color = THEME_COLORS$Main[5], hjust = 0.0, vjust = -0.5,
        size = 7 / .pt  # Adjust font size for annotations (7 pt)
    ) 
    message("[INFO] Added t0, tr, and N0 reference lines to plot.")

    # **🔹 Define File Paths Correctly**
    eq_dir <- normalizePath("../../src/M2_Annual_Production/latex/", mustWork = FALSE)
    eq_template <- file.path(eq_dir, "m2_regression_model_gompertz.tex")  
    eq_rendered <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.tex")  
    pdf_file <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.pdf")
    eps_file <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.eps")
    svg_file <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.svg")

    # **🔹 Read LaTeX Template and Replace Placeholders**
    if (!file.exists(eq_template)) stop("[ERROR] Template LaTeX file not found: ", eq_template)
    
    latex_template <- readLines(eq_template)

    # **Apply Parameter Values to LaTeX Template**
    format_number <- function(num) {
      if (num < 1) {
        return(sprintf("%.3f", num))  # 3 decimals if smaller than 1
      } else if (num < 10) {
        return(sprintf("%.1f", num))  # 1 decimal if between 1 and 10
      } else {
        return(sprintf("%.0f", num))  # Rounded integer if 10 or greater
      }
    }

    latex_rendered <- gsub("<<N0>>", format_number(N0), latex_template)
    latex_rendered <- gsub("<<k>>", format_number(model_params[["k"]]), latex_rendered)
    latex_rendered <- gsub("<<c>>", format_number(model_params[["Nmax"]]), latex_rendered)
    latex_rendered <- gsub("<<t0>>", format_number(t0), latex_rendered)

    # **🔹 Save Rendered LaTeX**
    writeLines(latex_rendered, eq_rendered)
    message("[INFO] Rendered LaTeX file saved: ", eq_rendered)

    # **🔹 Compile LaTeX**
    old_wd <- getwd()
    setwd(eq_dir)
    system(paste0("pdflatex -interaction=nonstopmode ", shQuote(basename(eq_rendered))))

    # **Check if PDF was created**
    if (!file.exists(pdf_file)) stop("[ERROR] PDF file was not created: ", pdf_file)

    # **🔹 Convert PDF to EPS & SVG**
    system(paste0("pdftops -eps ", shQuote(basename(pdf_file)), " ", shQuote(basename(eps_file))))
    if (!file.exists(eps_file)) stop("[ERROR] EPS file was not created: ", eps_file)

    system(paste0("dvisvgm --eps --libgs=/opt/homebrew/lib/libgs.dylib --output=", 
                  shQuote(basename(svg_file)), " ", shQuote(basename(eps_file))))
    if (!file.exists(svg_file)) stop("[ERROR] SVG file was not created: ", svg_file)

    setwd(old_wd)  # Restore working directory

    # **🔹 Read and Attach SVG to Plot**
    message("[INFO] Loading SVG equation into plot...")
    svg_rendered <- rsvg::rsvg(svg_file, width = 5000)
    grob_svg <- rasterGrob(svg_rendered, interpolate = TRUE)

    # **Define Placement Variables Dynamically**
    x0 <- min(t_real) - 7
    y0 <- max(y_real) - 70  
    delta <- 50  

    #p <- p + annotation_custom(grob_svg, xmin = x0, xmax = x0 + delta, ymin = y0, ymax = y0 + delta)

    message("[INFO] Gompertz equation added successfully.")
  }

  return(p)
}
