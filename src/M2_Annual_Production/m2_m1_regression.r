# ---------------------------------------------------------------------------- #
# Metric 1: Regression Analysis
# ---------------------------------------------------------------------------- #

M2_M1_Regression <- setRefClass(
  "M2_M1_Regression",
  
  # Fields
  fields = list(
    df = "data.frame",
    year_col = "character",
    articles_col = "character",
    results = "list"
  ),
  
  # Methods
  methods = list(
    
    # Constructor
    initialize = function(df, year_col = "Year", articles_col = "Articles") {
      .self$df <- df
      .self$year_col <- year_col
      .self$articles_col <- articles_col
      .self$results <- list()
    },
    
    # Run the Regression Analysis
    run = function() {
      message(" ==> M2_M1 :: run")
      
        x <- .self$df[[.self$year_col]]
        y <- .self$df[[.self$articles_col]]

        # Fit regression models
        models <- get_regression_models(x = x, y = y)

        # Generate metrics for model comparison
        comparison_table <- get_metrics_comparison_table(models, .self$df)

        # Find the best model
        best_model <- get_best_model(comparison_table)

        # Evaluate Model Performance : Error Distribution, Is it Normal? Is periodic?
        performance_model <- get_performance_model(x=x,y=y, models, best_model)

        models_serialized <- lapply(models, serialize_model)

        .self$results <- list(
                comparison_table = comparison_table,  # This is already JSON-compatible
                best_model = list(
                    name = best_model$name,
                    params = best_model$params,
                    r_squared = best_model$R2
                ),
                models = models_serialized,
                model_name = best_model$name,
                model_params = best_model$params,
                model_r_squared = best_model$R2,
                model_performance = performance_model
        )



    },
    
    # Save the regression plots
    save_plot = function(output_path) {
      message(" ==> M2_M1 :: save_plot")
      
      # Extract year and article data
      x <- .self$df[[.self$year_col]]
      y <- .self$df[[.self$articles_col]]

      # Generate regression plots
      create_regression_articles_plots(
        metric_regression = .self$results,
        models_regression = model_function_map,
        x = x, 
        y = y,
        output_path = output_path
      )

      create_regression_articles_small_plots(
        metric_regression = .self$results,
        models_regression = model_function_map,
        x = x, 
        y = y,
        output_path = output_path
      )
      
      # Additional derivative plots
      create_diff_nominal_articles_plots(
        x = .self$df, 
        y = .self$df, 
        output_path = output_path
      )
      
      create_diff_percentage_articles_plots(
        x = .self$df, 
        y = .self$df, 
        output_path = output_path
      )
    },
    
    # Save the results to JSON
    save_json = function(output_path) {
      json_data <- toJSON(.self$results, pretty = TRUE, auto_unbox = TRUE)
      write(json_data, file = file.path(output_path, "m1_regression.json"))
    },
    
    # Save the regression comparison table as a report
    save_report = function(output_path) {
      message(" ==> M2_M1 :: save_report")
      create_regression_articles_table(
        table = .self$results$models_comparison, 
        path = output_path
      )
    }
  )
)



# ---------------------------------------------------------------------------- #
# Plot Regression Models for Annual Articles
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Plot Regression Models for Annual Articles
# ---------------------------------------------------------------------------- #

create_regression_articles_plots <- function(metric_regression, models_regression, x, y, output_path) {
  
  # Validate Input
  if (is.null(metric_regression)) {
    stop("[ERROR] metric_regression is NULL.")
  }

  # Retrieve model function and parameters
  v_model <- models_regression[[metric_regression$model_name]]
  v_params <- parse_params(metric_regression$model_params)
  v_r_squared <- metric_regression$model_r_squared

  message("\n[INFO] Running Regression Plot")
  message("[DEBUG] Model:", metric_regression$model_name)
  message("[DEBUG] R² Value:", v_r_squared)

  # Prepare Data
  t_real <- x
  y_real <- y
  t_regression <- seq(min(t_real), max(t_real), by = 0.1)
  y_regression <- do.call(v_model, c(list(t = t_regression), v_params))

  df_real <- data.frame(Year = t_real, Articles = y_real)
  df_regression <- data.frame(Year = t_regression, Articles = y_regression)

  # Initialize Plot
  p <- ggplot() +
    geom_point(data = df_real, aes(x = Year, y = Articles), size = 2, color = "black") +
    geom_line(data = df_regression, aes(x = Year, y = Articles), color = "black", linewidth = 0.5, linetype = "dashed") +
    labs(
      title = "Annual Articles Regression Plot",
      x = "Year",
      y = "Number of Articles"
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
    )

  # **🔹 Add Model Name Annotation**
  label_model <- as.character(metric_regression$model_name)
  if (!is.na(min(t_real)) && !is.na(max(y_real)) && !is.null(label_model)) {
    p <- p + geom_text(
      aes(x = min(t_real) + 0, y = max(y_real) - 2, label = label_model),
      hjust = 0, vjust = 1, size = 3.85, color = "black"
    )
    message("[INFO] Model name annotation added successfully.")
  } else {
    message("[WARNING] Model name annotation skipped due to missing values.")
  }

  # **🔹 Add R² Value Annotation**
  if (!is.null(v_r_squared) && !is.na(v_r_squared)) {
    label_r2 <- paste0("R^2 == ", sprintf("%.3f", v_r_squared))
    p <- p + geom_text(
      aes(x = min(t_real) + 0, y = max(y_real) - 8, label = label_r2),
      hjust = 0, vjust = 1, size = 3.85, color = "black", parse = TRUE
    )
    message("[INFO] R-squared annotation added successfully.")
  } else {
    message("[WARNING] R-squared annotation skipped due to missing value.")
  }

  # **🔹 Add Confidence Interval**
  if (exists("lower_bound") && exists("upper_bound")) {
    p <- p + geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightblue", alpha = 0.3)
    message("[INFO] Confidence interval added to the plot.")
  }

  # **🔹 Add Supporting Features (t0, tr, N0)**
  p <- add_plot_model_supporting_features(p, metric_regression, t_real, y_real)


  p <- p + 
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

    # **🔹 Save Plot as PNG & SVG**
    output_file_png <- file.path(output_path, "Regression_Articles_Plot.png")
    output_file_svg <- file.path(output_path, "Regression_Articles_Plot.svg")

    ggsave(filename = output_file_png, plot = p, width = 7.16, height = 4.5, dpi = 600)
    ggsave(filename = output_file_svg, plot = p, width = 7.16, height = 4.5, device = "svg")

    message("[INFO] Regression plot saved successfully.")



  # Retrieve model function and parameters
  v_model <- models_regression[[metric_regression$model_name]]
  v_params <- parse_params(metric_regression$model_params)
  v_r_squared <- metric_regression$model_r_squared

  message("\n[INFO] Running Regression Plot")
  message("[DEBUG] Model:", metric_regression$model_name)
  message("[DEBUG] R² Value:", v_r_squared)


  # Prepare Data
  t_real <- x
  y_real <- y
  t_regression <- seq(min(t_real), max(t_real), by = 0.1)
  y_regression <- do.call(v_model, c(list(t = t_regression), v_params))

  df_real <- data.frame(Year = t_real, Articles = y_real)
  df_regression <- data.frame(Year = t_regression, Articles = y_regression)

  # Compute Squared Differences
  y_model_real <- do.call(v_model, c(list(t = t_real), v_params))  # Model predictions at t_real
  squared_diff <- (y_real - y_model_real)^2
  sqrt_diff <- sqrt(abs(y_real - y_model_real))

  # Create Data Frame with Squared and Square Root Differences
  df_diff <- data.frame(Year = t_real, Squared_Difference = squared_diff, Sqrt_Difference = sqrt_diff)

  # I want a regression on df_diff and find if is constant, lineal, cuadradtic, exponencial, poliniomic, normal,etc.



        # Perform Shapiro-Wilk test for normality
        shapiro_test <- shapiro.test(sqrt_diff)

        # Display results
        message("[INFO] Shapiro-Wilk Test for Normality")
        message("[INFO] W-statistic: ", shapiro_test$statistic)
        message("[INFO] p-value: ", shapiro_test$p.value)

        # Interpretation
        if (shapiro_test$p.value < 0.05) {
          message("[WARNING] Residuals are NOT normally distributed (p < 0.05). Consider further analysis.")
        } else {
          message("[INFO] Residuals appear to be normally distributed (p >= 0.05).")
        }

      p_diff <- ggplot() +
        geom_line(data = df_diff, aes(x = Year, y = Sqrt_Difference), linewidth = 1, color = "blue") +
        geom_point(data = df_diff, aes(x = Year, y = Sqrt_Difference), size = 2, color = "blue") +
        
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
          limits = c(0, max(df_diff$Sqrt_Difference)),  
          breaks = scales::extended_breaks()(df_diff$Sqrt_Difference),
          minor_breaks = scales::extended_breaks(n = 10)(df_diff$Sqrt_Difference)
        ) +
        scale_x_continuous(
          limits = c(min(t_real), max(t_real)),  
          breaks = scales::extended_breaks()(t_real),
          minor_breaks = scales::extended_breaks(n = 10)(t_real)
        )



      # Save Squared Differences Plot
      output_file_diff_png <- file.path(output_path, "Squared_Differences_Plot.png")
      output_file_diff_svg <- file.path(output_path, "Squared_Differences_Plot.svg")

      ggsave(filename = output_file_diff_png, plot = p_diff, width = 7.16, height = 4.5, dpi = 600)
      ggsave(filename = output_file_diff_svg, plot = p_diff, width = 7.16, height = 4.5, device = "svg")

      message("[INFO] Squared differences plot saved successfully.")




    # Histogram of Residuals
    p_hist <- ggplot(df_diff, aes(x = Sqrt_Difference)) +
      geom_histogram(color = "black", fill = "skyblue", bins = 30) +
      labs(title = "Histogram of Regression Residuals", x = "Residuals", y = "Frequency") +
      theme_minimal()

    # Q-Q Plot (to check normality visually)
    p_qq <- ggplot(df_diff, aes(sample = Sqrt_Difference)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()

    # Save Plots
    ggsave(file.path(output_path, "Histogram_Residuals.png"), plot = p_hist, dpi = 600)
    ggsave(file.path(output_path, "QQ_Plot_Residuals.png"), plot = p_qq, dpi = 600)

    message("[INFO] Residual histogram and Q-Q plot saved successfully.")


    library(lmtest)
    dw_test <- dwtest(Sqrt_Difference ~ Year, data = df_diff)
    message("[INFO] Durbin-Watson test result: DW Statistic = ", dw_test$statistic, ", p-value = ", dw_test$p.value)

    library(ggplot2)
    library(ggfortify)

    p_acf <- autoplot(acf(df_diff$Sqrt_Difference, plot = FALSE)) + 
      labs(title = "Autocorrelation Function (ACF) of Residuals") +
      theme_minimal()

    p_pacf <- autoplot(pacf(df_diff$Sqrt_Difference, plot = FALSE)) + 
      labs(title = "Partial Autocorrelation Function (PACF) of Residuals") +
      theme_minimal()

    # Save Plots
    ggsave(file.path(output_path, "ACF_Residuals.png"), plot = p_acf, dpi = 600)
    ggsave(file.path(output_path, "PACF_Residuals.png"), plot = p_pacf, dpi = 600)

    message("[INFO] ACF and PACF plots saved successfully.")

    library(lmtest)
    bp_test <- bptest(Sqrt_Difference ~ Year, data = df_diff)
    message("[INFO] Breusch-Pagan test result: BP Statistic = ", bp_test$statistic, ", p-value = ", bp_test$p.value)


    # Perform statistical tests
    stat_results <- perform_statistical_tests(df_diff, "Sqrt_Difference", output_path)
    # Convert to JSON-friendly format
    stat_results_json <- toJSON(stat_results, pretty = TRUE, auto_unbox = TRUE)
    # Modify output_path: Replace "figures" with "jsons"
    json_output_path <- gsub("figures", "jsons", output_path)
    write(stat_results_json, file = file.path(json_output_path, "m2_regression_model_stats_result.json"))

}









create_diff_nominal_articles_plots <- function(x, y, output_path) {
  # Ensure x and y have the expected columns
  if (!("Year" %in% names(x)) || !("Articles" %in% names(y))) {
    stop("[ERROR] Data frames must contain 'Year' and 'Articles' columns.")
  }

  # Extract real data
  t_real <- x$Year
  y_real <- y$Articles

  # Calculate the numerical derivative of y with respect to x (Year)
  y_diff <- diff(y_real)
  t_diff <- t_real[-1]  # Remove the first time point to match the length of dy_dt

  # Create a data frame for plotting
  df_diff <- data.frame(Year = t_diff, Articles = y_diff)

  # Create the plot
  p <- ggplot(data = df_diff, aes(x = Year, y = Articles)) +
    geom_point(size = 1.25, shape = 20, color = "#565656") +
    geom_line(size = 0.25, linetype = "solid", color = "#565656") +
    labs(
      title = "Nominal Change in Annual Articles",
      x = "Year",
      y = "Difference in Articles"
    ) +
    ieee_theme

  # Save the plot as PNG and SVG
  output_file_png <- file.path(output_path, "Nominal_Annual_Diff_Articles_Plot_PNG.png")
  output_file_svg <- file.path(output_path, "Nominal_Annual_Diff_Articles_Plot_SVG.svg")
  
  v_k_scaling <- 0.5
  v_k_width_hight <- 1.5
  v_width <- 8.8 * v_k_scaling
  v_height <- v_width / v_k_width_hight

  ggsave(filename = output_file_png, plot = p, width = v_width, height = v_height, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_height, device = "svg")
}




create_diff_percentage_articles_plots <- function(x, y, output_path) {
  # Ensure x and y have the expected columns
  if (!("Year" %in% names(x)) || !("Articles" %in% names(y))) {
    stop("[ERROR] Data frames must contain 'Year' and 'Articles' columns.")
  }

  # Extract real data
  t_real <- x$Year
  y_real <- y$Articles

  # Calculate the percentage change in articles
  t_diff <- t_real[-1]
  y_diff_percentage <- sapply(1:(length(y_real) - 1), function(i) {
    100 * (y_real[i + 1] - y_real[i]) / (y_real[i] + 1e-8)  # Prevent division by zero
  })

  # Create a data frame for plotting
  df_diff <- data.frame(Year = t_diff, PercentageChange = y_diff_percentage)

  # Create the plot
  p <- ggplot(data = df_diff, aes(x = Year, y = PercentageChange)) +
    geom_point(size = 1.25, shape = 20, color = "#565656") +
    geom_line(size = 0.25, linetype = "solid", color = "#565656") +
    labs(
      title = "Percentage Change in Annual Articles",
      x = "Year",
      y = "Percentage Change (%)"
    ) +
    ieee_theme

  # Save the plot as PNG and SVG
  output_file_png <- file.path(output_path, "Percentage_Annual_Diff_Articles_Plot_PNG.png")
  output_file_svg <- file.path(output_path, "Percentage_Annual_Diff_Articles_Plot_SVG.svg")
  
  v_k_scaling <- 0.5
  v_k_width_hight <- 1.5
  v_width <- 8.8 * v_k_scaling
  v_height <- v_width / v_k_width_hight

  ggsave(filename = output_file_png, plot = p, width = v_width, height = v_height, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_height, device = "svg")
}





add_plot_model_supporting_features <- function(p, metric_regression, t_real, y_real) {
  
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
        aes(x = x, y = y, label = paste0("t0 = ", format_number(t0))), 
        color = THEME_COLORS$Main[1], angle = 90, vjust = -0.5, hjust = -0.2
      )

    if (!is.na(tr)) {
      p <- p +
        geom_vline(xintercept = tr, linetype = "dashed", color = THEME_COLORS$Main[3]) +
        geom_text(
          data = data.frame(x = tr, y = max(y_real) - 23),
          aes(x = x, y = y, label = paste0("tr = ", format_number(tr))), 
          color = THEME_COLORS$Main[3], angle = 90, vjust = -0.5, hjust = -0.2
        )
    }

    p <- p +
      geom_hline(yintercept = N0, linetype = "dashed", color = THEME_COLORS$Main[5]) +
      geom_text(
        data = data.frame(x = min(t_real), y = N0),
        aes(x = x, y = y, label = paste0("N0 = ", format_number(N0))), 
        color = THEME_COLORS$Main[5], hjust = 0.0, vjust = -0.5
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

    p <- p + annotation_custom(grob_svg, xmin = x0, xmax = x0 + delta, ymin = y0, ymax = y0 + delta)

    message("[INFO] Gompertz equation added successfully.")
  }

  return(p)
}


