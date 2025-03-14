library(R6)
library(ggplot2)
library(ggfortify)
library(jsonlite)
library(gridExtra)
library(rsvg)

source('../../src/M2_Annual_Production/plots/R_IEEE_Plot.r')
source('../../src/M2_Annual_Production/plots/Regression_Model_Gompertz_Plot.r')

# -------------------------------------------
# ACF Residuals Plot
# -------------------------------------------
ACF_Residuals_Plot <- R6Class("ACF_Residuals_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Autocorrelation Function (ACF) \n of Residuals", "Lag", "Autocorrelation")
      self$plot <- autoplot(acf(residual_data$Residual, plot = FALSE)) + super$getTheme()
      self$metrics <- list(
        acf_values = acf(residual_data$Residual, plot = FALSE)$acf
      )
    }
  )
)

# -------------------------------------------
# PACF Residuals Plot
# -------------------------------------------
PACF_Residuals_Plot <- R6Class("PACF_Residuals_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Partial Autocorrelation Function (PACF)\nof Residuals", "Lag", "Partial Autocorrelation")
      self$plot <- autoplot(pacf(residual_data$Residual, plot = FALSE)) + super$getTheme()
      self$metrics <- list(
        pacf_values = pacf(residual_data$Residual, plot = FALSE)$acf
      )
    }
  )
)

# -------------------------------------------
# Histogram of Residuals
# -------------------------------------------
Histogram_Residuals_Plot <- R6Class("Histogram_Residuals_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Histogram of Residuals", "Residuals", "Density")
      residual_mean <- mean(residual_data$Residual, na.rm = TRUE)
      std_dev <- sd(residual_data$Residual, na.rm = TRUE)

      self$plot <- ggplot(residual_data, aes(x = Residual)) +
        geom_histogram(aes(y = ..density..), color = "black", fill = "gray80", bins = 30) +
        stat_function(fun = dnorm, args = list(mean = residual_mean, sd = std_dev), color = "black", linewidth = 1) +
        geom_vline(aes(xintercept = residual_mean), color = "red", linewidth = 0.4, linetype = "dashed") +
        super$getTheme()

      self$metrics <- list(
        mean_residual = residual_mean,
        std_dev_residual = std_dev
      )
    }
  )
)

# -------------------------------------------
# Regression Residuals Plot
# -------------------------------------------
Regression_Residuals_Plot <- R6Class("Regression_Residuals_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Residuals Over Time", "Year", "Residual")
      self$plot <- ggplot(residual_data, aes(x = Year, y = Residual)) +
        geom_point(size = 1, shape = 21, fill = "white", color = "black", stroke = 0.5) +
        geom_line(color = "black", linewidth = 0.5, linetype = "solid") +
        super$getTheme()

      self$metrics <- list(
        residual_summary = summary(residual_data$Residual)
      )
    }
  )
)

# -------------------------------------------
# Model Selection and Best Fit Plot
# -------------------------------------------
Residual_Sqrt_Diff_Model_Plot <- R6Class("Residual_Sqrt_Diff_Model_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Square Root Differences (Regression Error)", "Year", "Difference Values")

      # Fit multiple models
      fit_model <- function(formula, data) {
        model <- try(lm(formula, data = data), silent = TRUE)
        if (inherits(model, "try-error")) return(NULL)
        r_squared <- summary(model)$r.squared
        return(list(model = model, r_squared = r_squared))
      }

      models <- list(
        "Linear" = fit_model(Sqrt_Difference ~ Year, residual_data),
        "Quadratic" = fit_model(Sqrt_Difference ~ poly(Year, 2, raw = TRUE), residual_data),
        "Cubic" = fit_model(Sqrt_Difference ~ poly(Year, 3, raw = TRUE), residual_data),
        "Exponential" = fit_model(log(Sqrt_Difference) ~ Year, residual_data),
        "Logarithmic" = fit_model(Sqrt_Difference ~ log(Year), residual_data)
      )

      # Remove failed models
      models <- models[!sapply(models, is.null)]
      model_r2_values <- sapply(models, function(m) m$r_squared)

      # Select the best model
      best_model_name <- names(which.max(model_r2_values))
      best_model <- models[[best_model_name]]$model
      best_r2 <- round(models[[best_model_name]]$r_squared, 3)

      # Generate plot
      residual_data$Fitted <- predict(best_model, newdata = residual_data)
      self$plot <- ggplot(residual_data, aes(x = Year, y = Sqrt_Difference)) +
        geom_line(color = "black", linewidth = 0.5) +
        geom_point(size = 1.2, shape = 21, fill = "white", color = "black", stroke = 0.5) +
        geom_line(aes(y = Fitted), color = "black", linetype = "dashed", linewidth = 0.5) +
        super$getTheme()

      # Store metrics
      self$metrics <- list(
        best_model = best_model_name,
        r_squared = best_r2,
        model_comparisons = model_r2_values
      )
    }
  )
)



# -------------------------------------------
# Q-Q Plot of Residuals
# -------------------------------------------
QQ_Residuals_Plot <- R6Class("QQ_Residuals_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Q-Q Plot of Residuals", "Theoretical Quantiles", "Sample Quantiles")

      y_max <- max(abs(residual_data$Residual), na.rm = TRUE)

      self$plot <- ggplot(residual_data, aes(sample = Residual)) +
        stat_qq(size = 1, shape = 21, fill = "white", color = "black", stroke = 0.5) +
        stat_qq_line(color = "black", linewidth = 0.8, linetype = "solid") +
        scale_y_continuous(limits = c(-y_max, y_max)) +
        super$getTheme()

      self$metrics <- list(
        residual_summary = summary(residual_data$Residual)
      )
    }
  )
)

# -------------------------------------------
# Durbin-Watson Test Plot for Residual Autocorrelation
# -------------------------------------------
DW_Test_Plot <- R6Class("DW_Test_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
        super$initialize("Durbin-Watson Test for\nResidual Autocorrelation", "Year", "Residuals")

      require(lmtest)
      dw_test <- dwtest(Residual ~ Year, data = residual_data)

      # Get the maximum absolute residual value for symmetric scaling
      max_abs_residual <- max(abs(residual_data$Residual), na.rm = TRUE)

      self$plot <- ggplot(residual_data, aes(x = Year, y = Residual)) +
        geom_point(size = 1, shape = 21, fill = "white", color = "black", stroke = 0.5) +
        geom_line(color = "black", linewidth = 0.5, linetype = "solid") +
        ylim(-max_abs_residual, max_abs_residual) +  # Set symmetrical y-axis limits
        super$getTheme()

      self$metrics <- list(
        DW_statistic = dw_test$statistic,
        p_value = dw_test$p.value
      )
    }
  )
)


# -------------------------------------------
# Breusch-Pagan Test Plot for Heteroscedasticity
# -------------------------------------------
BP_Test_Plot <- R6Class("BP_Test_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Breusch-Pagan Test for\nHeteroscedasticity", "Year", "Variance of Residuals")

      require(lmtest)
      bp_test <- bptest(Residual ~ Year, data = residual_data)

      self$plot <- ggplot(residual_data, aes(x = Year, y = Residual^2)) +
        geom_point(size = 1, shape = 21, fill = "white", color = "black", stroke = 0.5) +
        geom_smooth(method = "lm", color = "red", linetype = "dashed",size = 0.25) +
        super$getTheme()

      self$metrics <- list(
        BP_statistic = bp_test$statistic,
        p_value = bp_test$p.value
      )
    }
  )
)

# -------------------------------------------
# Shapiro-Wilk Test Plot for Normality
# -------------------------------------------
Shapiro_Wilk_Plot <- R6Class("Shapiro_Wilk_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Shapiro-Wilk Test for\nResidual Normality", "Residuals", "Density")

      shapiro_test <- shapiro.test(residual_data$Residual)

      self$plot <- ggplot(residual_data, aes(x = Residual)) +
        geom_histogram(aes(y = ..density..), color = "black", fill = "gray80", bins = 30) +
        stat_function(fun = dnorm, args = list(mean = mean(residual_data$Residual), sd = sd(residual_data$Residual)),
                      color = "black", linewidth = 1) +
        geom_vline(aes(xintercept = mean(residual_data$Residual)), color = "red", linewidth = 0.4, linetype = "dashed") +
        super$getTheme()

      self$metrics <- list(
        W_statistic = shapiro_test$statistic,
        p_value = shapiro_test$p.value
      )
    }
  )
)

# -------------------------------------------
# Residuals Standardized Plot
# -------------------------------------------
Standardized_Residuals_Plot <- R6Class("Standardized_Residuals_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(residual_data) {
      super$initialize("Standardized Residuals", "Year", "Standardized Residuals")

      std_dev <- sd(residual_data$Residual, na.rm = TRUE)
      residual_data$Standardized_Residuals <- residual_data$Residual / std_dev

      self$plot <- ggplot(residual_data, aes(x = Year, y = Standardized_Residuals)) +
        geom_point(size = 1, shape = 21, fill = "white", color = "black", stroke = 0.5) +
        geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
        super$getTheme()

      self$metrics <- list(
        mean_standardized_residuals = mean(residual_data$Standardized_Residuals, na.rm = TRUE),
        std_dev_standardized_residuals = sd(residual_data$Standardized_Residuals, na.rm = TRUE)
      )
    }
  )
)


# -------------------------------------------
# Regression Articles Plot
# -------------------------------------------
Regression_Articles_Plot <- R6Class("Regression_Articles_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(metric_regression, x, y, models_regression, lower_bound = NULL, upper_bound = NULL) {
      super$initialize("Annual Articles Regression Plot", "Year", "Number of Articles")

      # Validate inputs
      self$validate_inputs(metric_regression, x, y)

      # Extract model information
      model_info <- self$extract_model_info(metric_regression, models_regression)
      self$log_model_info(model_info)

      # Prepare data for plotting
      plot_data <- self$prepare_plot_data(x, y, model_info)

        # Ensure create_plot is a function
    if (!is.function(self$create_plot)) {
        stop("[ERROR] `create_plot` is not a function! Check its definition in `Regression_Articles_Plot`.")
    } 



      # Generate regression plot
     self$plot <- self$create_plot(plot_data, model_info, lower_bound, upper_bound, metric_regression)


      # Store metrics for JSON report
      self$metrics <- list(
        model_name = model_info$model_name,
        r_squared = model_info$r_squared,
        fitted_params = model_info$params
      )
    },

    # -------------------------------------------
    # Create Regression Plot
    # -------------------------------------------
    create_plot = function(plot_data, model_info, lower_bound, upper_bound, metric_regression) {
      df_real <- plot_data$df_real
      df_regression <- plot_data$df_regression
      t_real <- plot_data$t_real
      y_real <- plot_data$y_real

      p <- ggplot() +
        geom_point(data = df_real, aes(x = Year, y = Articles), size = 1, shape = 20, fill = "white", color = "black", stroke = 0.7) +
        geom_line(data = df_regression, aes(x = Year, y = Articles), color = "black", linewidth = 0.5, linetype = "solid") +
        labs(title = self$title, x = self$x_label, y = self$y_label) +
        super$getTheme()

      # Add model annotations
      p <- self$add_model_name_annotation(p, model_info$model_name, t_real, y_real)
      p <- self$add_r_squared_annotation(p, model_info$r_squared, t_real, y_real)

      # Add confidence interval if available
      #p <- self$add_confidence_interval(p, lower_bound, upper_bound)

      # Add supporting features (e.g., Gompertz model parameters)
      p <- self$add_plot_model_supporting_features(p, metric_regression, t_real, y_real)

      # Apply axis scales
      p <- self$apply_axis_scales(p, t_real, y_real)

        message(' ')
        message(' ')
        message(' ')
        message(' [ END OF PLOT] !!!!!!!!!')
        message(' ')
        message(' ')
        message(' ')
      return(p)
    },

    # -------------------------------------------
    # Prepare Plot Data
    # -------------------------------------------
    prepare_plot_data = function(x, y, model_info) {
      t_real <- x
      y_real <- y

      # Generate regression line points
      t_regression <- seq(min(t_real), max(t_real), by = 0.1)
      y_regression <- do.call(model_info$model_function, c(list(t = t_regression), model_info$params))

      df_real <- data.frame(Year = t_real, Articles = y_real)
      df_regression <- data.frame(Year = t_regression, Articles = y_regression)

      return(list(
        t_real = t_real,
        y_real = y_real,
        t_regression = t_regression,
        y_regression = y_regression,
        df_real = df_real,
        df_regression = df_regression
      ))
    },

    # -------------------------------------------
    # Validate Inputs
    # -------------------------------------------
    validate_inputs = function(metric_regression, x, y) {
      if (is.null(metric_regression)) stop("[ERROR] metric_regression is NULL.")
      if (is.null(x) || length(x) == 0) stop("[ERROR] x (time points) is NULL or empty.")
      if (is.null(y) || length(y) == 0) stop("[ERROR] y (observed values) is NULL or empty.")
    },

    # -------------------------------------------
    # Extract Model Information
    # -------------------------------------------
    extract_model_info = function(metric_regression, models_regression) {
      return(list(
        model_name = metric_regression$model_name,
        model_function = models_regression[[metric_regression$model_name]],
        params = self$parse_params(metric_regression$model_params),
        r_squared = metric_regression$model_r_squared
      ))
    },

    # -------------------------------------------
    # Parse Model Parameters
    # -------------------------------------------
    parse_params = function(param_string) {
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
    },

    # -------------------------------------------
    # Log Model Information
    # -------------------------------------------
    log_model_info = function(model_info) {
      message("\n[INFO] Running Regression Plot")
      message("[DEBUG] Model: ", model_info$model_name)
      message("[DEBUG] R² Value: ", model_info$r_squared)
    },

    # -------------------------------------------
    # Add Model Name Annotation
    # -------------------------------------------
    add_model_name_annotation = function(p, model_name, t_real, y_real) {
      p + geom_text(
        aes(x = min(t_real) + 0, y = min(y_real) + 16, label = model_name),
        hjust = 0, vjust = 1, size =  10 / .pt, color = "black"
      )
    },

    # -------------------------------------------
    # Add R² Value Annotation
    # -------------------------------------------
    add_r_squared_annotation = function(p, r_squared, t_real, y_real) {
        label_r2 <- paste0("R^2 == ", sprintf("%.3f", r_squared))
        #label_r2 <- bquote(R^2 ~ "=" ~ .(sprintf("%.3f", r_squared)))
        #label_r2 <- "Mi R2"#paste0("R^2 = ", sprintf("%.3f", r_squared))
        p <- p + geom_text(
            aes(
                x = min(t_real) + 0, 
                y = min(y_real) + 28, 
                label = label_r2
            ),
            hjust = 0, vjust = 1, size = 10 / .pt, color = "black",parse = TRUE
        ) 
        return(p)
    },

    # -------------------------------------------
    # Add Confidence Interval
    # -------------------------------------------
    add_confidence_interval = function(p, lower_bound, upper_bound) {
      if (!is.null(lower_bound) && !is.null(upper_bound)) {
        p <- p + geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightblue", alpha = 0.3)
      }
      return(p)
    },

    # -------------------------------------------
    # Apply Axis Scales
    # -------------------------------------------
    apply_axis_scales = function(p, t_real, y_real) {
      p + 
        scale_y_continuous(
          limits = c(0, max(y_real)),
          breaks = scales::extended_breaks()(y_real),
          minor_breaks = scales::extended_breaks(n = 10)(y_real)
        ) +
        scale_x_continuous(
          limits = c(min(t_real), max(t_real)),
          breaks = scales::extended_breaks()(t_real),
          minor_breaks = scales::extended_breaks(n = 10)(t_real)
        )
    },

############################
## Gompertz Model
############################
    extract_model_params = function(param_string) {
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
    },

    validate_model_params = function(params) {
        required_keys <- c("N0", "t0", "Nmax", "k", "y0")
        if (!all(required_keys %in% names(params))) {
            stop("[ERROR] Missing required parameters: ", paste(setdiff(required_keys, names(params)), collapse = ", "))
        }
    },

    compute_tr = function(y_real, N0, t_real) {
        tr_index <- which(y_real >= 0.9 * N0)[1]
        return(if (!is.na(tr_index)) t_real[tr_index] else NA)
    },

    add_plot_features = function(p, N0, t0, tr, t_real, y_real, THEME_COLORS) {
        p <- p +
            geom_vline(xintercept = t0, size= 0.5, linetype = "dashed", color = THEME_COLORS$Main[1]) +
            geom_text(
            aes(x = t0, y = max(y_real) - 6, label = "t0"), 
            color = THEME_COLORS$Main[1], angle = 90, vjust = -0.5, hjust = -0.2, size = 10 / .pt
            ) +
            geom_hline(yintercept = N0, size= 0.5, linetype = "dashed", color = THEME_COLORS$Main[5]) +
            geom_text(
            aes(x = min(t_real), y = N0, label = "N0"), 
            color = THEME_COLORS$Main[5], hjust = 0.0, vjust = -0.5, size = 10 / .pt
            )
        
        if (!is.na(tr)) {
            p <- p +
            geom_vline(xintercept = tr, size= 0.5, linetype = "dashed", color = THEME_COLORS$Main[3]) +
            geom_text(
                aes(x = tr, y = max(y_real) - 5, label = "tr"), 
                color = THEME_COLORS$Main[3], angle = 90, vjust = -0.5, hjust = -0.2, size = 10 / .pt
            )
        }
    
        return(p)
    },

    process_latex_equation = function(eq_dir, params) {
        eq_template <- file.path(eq_dir, "m2_regression_model_gompertz.tex")  
        eq_rendered <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.tex")  
        pdf_file <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.pdf")
        eps_file <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.eps")
        svg_file <- file.path(eq_dir, "m2_regression_model_gompertz_rendered.svg")
        
        if (!file.exists(eq_template)) stop("[ERROR] Template LaTeX file not found: ", eq_template)

        # Read and replace placeholders
        latex_template <- readLines(eq_template)
        format_number <- function(num) {
            if (num < 1) return(sprintf("%.3f", num))
            else if (num < 10) return(sprintf("%.1f", num))
            else return(sprintf("%.0f", num))
        }
        
        latex_rendered <- gsub("<<N0>>", format_number(params[["N0"]]), latex_template)
        latex_rendered <- gsub("<<k>>", format_number(params[["k"]]), latex_rendered)
        latex_rendered <- gsub("<<c>>", format_number(params[["Nmax"]]), latex_rendered)
        latex_rendered <- gsub("<<t0>>", format_number(params[["t0"]]), latex_rendered)

        # Save LaTeX file
        writeLines(latex_rendered, eq_rendered)

        # Compile LaTeX and convert formats
        old_wd <- getwd()
        setwd(eq_dir)
        system(paste0("pdflatex -interaction=nonstopmode ", shQuote(basename(eq_rendered))))
        
        if (!file.exists(pdf_file)) stop("[ERROR] PDF file was not created: ", pdf_file)
        system(paste0("pdftops -eps ", shQuote(basename(pdf_file)), " ", shQuote(basename(eps_file))))
        if (!file.exists(eps_file)) stop("[ERROR] EPS file was not created: ", eps_file)
        system(paste0("dvisvgm --eps --libgs=/opt/homebrew/lib/libgs.dylib --output=", 
                        shQuote(basename(svg_file)), " ", shQuote(basename(eps_file))))
        if (!file.exists(svg_file)) stop("[ERROR] SVG file was not created: ", svg_file)

        setwd(old_wd)
        
        return(svg_file)
    },

    add_equation_to_plot = function(p, svg_file, t_real, y_real) {
        svg_rendered <- rsvg::rsvg(svg_file, width = 5000)
        grob_svg <- rasterGrob(svg_rendered, interpolate = TRUE)

        x0 <- min(t_real) - 7
        y0 <- max(y_real) - 70  
        delta <- 50  

        p <- p + annotation_custom(grob_svg, xmin = x0, xmax = x0 + delta, ymin = y0, ymax = y0 + delta)
        return(p)
    },
############################
## / Gompertz Model
############################
    add_plot_model_supporting_features = function(p, metric_regression, t_real, y_real) {
        
        if (!inherits(p, "ggplot")) stop("[ERROR] `p` is NOT a ggplot object.")
        if (is.null(metric_regression) || !"model_name" %in% names(metric_regression)) stop("[ERROR] Invalid metric_regression.")

        if (metric_regression$model_name == 'Gompertz') {
            params <- metric_regression$best_model$params
            gompertz_plot <- Regression_Model_Gompertz_Plot$new()
            gompertz_plot$setP(p)
            gompertz_plot$setData(t_real, y_real)
            gompertz_plot$setColors(THEME_COLORS)
            gompertz_plot$setParams(params)
            #gompertz_plot$set_latex_directory("../../src/M2_Annual_Production/latex/")  # OPTIONAL
            p <- gompertz_plot$render()
            print(p)
            message("[INFO] Gompertz equation added successfully.")
        }
        
        return(p)
    },

    # -------------------------------------------
    # Generate JSON Report
    # -------------------------------------------
    getReport = function() {
      return(toJSON(self$metrics, pretty = TRUE, auto_unbox = TRUE))
    }
  )
)



# -------------------------------------------
# Save All Regression and Residual Plots
# -------------------------------------------
save_all_m2_regression_plots <- function(regression_data, residual_data, output_path) {
  one_column_path <- file.path(output_path, "OneColumn")
  double_column_path <- file.path(output_path, "DoubleColumn")

  if (dir.exists(one_column_path)) unlink(one_column_path, recursive = TRUE)
  dir.create(one_column_path, recursive = TRUE)
  if (dir.exists(double_column_path)) unlink(double_column_path, recursive = TRUE)
  dir.create(double_column_path, recursive = TRUE)

  # Instantiate all plot classes (Residuals + Regression)
  plot_classes <- list(
    Regression_Articles_Plot$new(regression_data$metric_regression, 
                                 regression_data$x, 
                                 regression_data$y, 
                                 regression_data$models_regression, 
                                 regression_data$lower_bound, 
                                 regression_data$upper_bound),
    ACF_Residuals_Plot$new(residual_data),
    PACF_Residuals_Plot$new(residual_data),
    Histogram_Residuals_Plot$new(residual_data),
    QQ_Residuals_Plot$new(residual_data),
    DW_Test_Plot$new(residual_data),
    BP_Test_Plot$new(residual_data),
    Shapiro_Wilk_Plot$new(residual_data),
    Standardized_Residuals_Plot$new(residual_data)
  )

  # Define names for each plot type
  plot_names <- c("Regression_Articles", "ACF_Residuals", "PACF_Residuals",
                  "Histogram_Residuals", "QQ_Residuals", "DW_Test", 
                  "BP_Test", "Shapiro_Wilk", "Standardized_Residuals")

  # Helper function to save plots in PNG & SVG formats
  save_plot <- function(plot_obj, name) {

    one_col_plot <- plot_obj$getOneColumnPlot()
    double_col_plot <- plot_obj$getDoubleColumnPlot()

    ggsave(filename = file.path(one_column_path, paste0(name, ".png")), plot = one_col_plot$plot, width = one_col_plot$width, height = one_col_plot$height, dpi = one_col_plot$dpi)
    ggsave(filename = file.path(one_column_path, paste0(name, ".svg")), plot = one_col_plot$plot, width = one_col_plot$width, height = one_col_plot$height, device = "svg")
    
    ggsave(filename = file.path(double_column_path, paste0(name, ".png")), plot = double_col_plot$plot, width = double_col_plot$width, height = double_col_plot$height, dpi = double_col_plot$dpi)
    ggsave(filename = file.path(double_column_path, paste0(name, ".svg")), plot = double_col_plot$plot, width = double_col_plot$width, height = double_col_plot$height, device = "svg")
  
  }

    # Loop through and save each plot
    for (i in seq_along(plot_classes)) {
        save_plot(plot_classes[[i]], plot_names[i])
    }

    # Define JSON output path by replacing '/figures/' with '/jsons/'
    json_output_path <- gsub("figures", "jsons", output_path)
    # Ensure the directory exists
    if (!dir.exists(json_output_path)) {
        dir.create(json_output_path, recursive = TRUE)
    }
    all_reports <- list()
    for (plot_obj in plot_classes) {
        name <- plot_obj$title  # Use the plot title as the key
        report <- plot_obj$getReport()
        # Convert JSON string to R list before storing it
        all_reports[[name]] <- fromJSON(report)
    }
    json_file_path <- file.path(json_output_path, "m1_regression_plots_report.json")
    write(toJSON(all_reports, pretty = TRUE, auto_unbox = TRUE), file = json_file_path)
    message("[INFO] All reports saved in: ", json_file_path)
    message("[INFO] All regression and residual plots saved successfully.")
}



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
