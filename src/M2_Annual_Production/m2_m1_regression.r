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
                model_r_squared = best_model$R2
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
create_regression_articles_plots <- function(metric_regression, models_regression, x, y, output_path) {
  
  # Validate Input
  if (is.null(metric_regression)) {
    stop("[ERROR] `metric_regression` is NULL.")
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
    ) +
    scale_y_continuous(
      breaks = scales::extended_breaks()(y_real),
      minor_breaks = scales::extended_breaks(n = 10)(y_real)
    ) +
    scale_x_continuous(
      breaks = scales::extended_breaks()(t_real),
      minor_breaks = scales::extended_breaks(n = 10)(t_real)
    )

  # **🔹 Add Model Name Annotation**
  label_model <- as.character(metric_regression$model_name)
  if (!is.na(min(t_real)) && !is.na(max(y_real)) && !is.null(label_model)) {
    p <- p + geom_text(
      aes(x = min(t_real) + 2, y = max(y_real) - 5, label = label_model),
      hjust = 0, vjust = 1, size = 4.5, color = "black"
    )
    message("[INFO] Model name annotation added successfully.")
  } else {
    message("[WARNING] Model name annotation skipped due to missing values.")
  }

  # **🔹 Add R² Value Annotation**
  if (!is.null(v_r_squared) && !is.na(v_r_squared)) {
    label_r2 <- paste0("R^2 == ", sprintf("%.3f", v_r_squared))
    p <- p + geom_text(
      aes(x = min(t_real) + 2, y = max(y_real) - 12, label = label_r2),
      hjust = 0, vjust = 1, size = 4.5, color = "black", parse = TRUE
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

  p <- p + add_plot_model_supporting_features(p, metric_regression$model_name, t_real, y_real)

  # **🔹 Save Plot as PNG & SVG**
  output_file_png <- file.path(output_path, "Regression_Articles_Plot.png")
  output_file_svg <- file.path(output_path, "Regression_Articles_Plot.svg")

  ggsave(filename = output_file_png, plot = p, width = 6, height = 4, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = 6, height = 4, device = "svg")

  message("[INFO] Regression plot saved successfully.")
}

library(grid)
library(rsvg)

add_plot_model_supporting_features <- function(p, model_name, t_real, y_real) {
  
  # **🔹 Add Gompertz Model Equation if Applicable**
  if (model_name == "Gompertz") {
    
    # Define file paths
    eq_dir <- normalizePath("../../src/M2_Annual_Production/latex/", mustWork = FALSE)
    eq_file <- file.path(eq_dir, "m2_regression_model_gompertz")
    tex_file <- paste0(eq_file, ".tex")
    pdf_file <- paste0(eq_file, ".pdf")
    eps_file <- paste0(eq_file, ".eps")
    svg_file <- paste0(eq_file, ".svg")

    # **🔹 Print Debugging Information**
    message("[DEBUG] Current Working Directory: ", getwd())
    message("[DEBUG] Expected TEX Path: ", tex_file)
    message("[DEBUG] Expected PDF Path: ", pdf_file)
    message("[DEBUG] Expected EPS Path: ", eps_file)
    message("[DEBUG] Expected SVG Path: ", svg_file)

    # **🔹 Change working directory to ensure correct file paths**
    message("[INFO] Changing working directory to: ", eq_dir)
    old_wd <- getwd()
    setwd(eq_dir)

    # **🔹 Compile LaTeX to PDF**
    message("[INFO] Compiling LaTeX to PDF...")
    system(paste0("pdflatex -interaction=nonstopmode ", shQuote(tex_file)))

    # **🔹 Convert PDF to EPS**
    message("[INFO] Converting PDF to EPS...")
    system(paste0("pdftops -eps ", shQuote(pdf_file), " ", shQuote(eps_file)))

    # **🔹 Convert EPS to SVG with Ghostscript**
    message("[INFO] Converting EPS to SVG...")
    system(paste0("dvisvgm --eps --libgs=/opt/homebrew/lib/libgs.dylib --output=", 
                  shQuote(svg_file), " ", shQuote(eps_file)))

    # **🔹 Restore original working directory**
    setwd(old_wd)

    # **🔹 Read SVG and Convert to Grid Object**
    if (file.exists(svg_file)) {
      message("[INFO] Loading SVG equation into plot...")
      
      # Convert the SVG to a raster image
      grob_svg <- rasterGrob(rsvg::rsvg(svg_file), interpolate = TRUE)
      
      # Add the SVG as an annotation
      p <- p + annotation_custom(grob_svg, 
                                 xmin = min(t_real) + 2, xmax = min(t_real) + 10, 
                                 ymin = max(y_real) - 15, ymax = max(y_real) - 5)
      
      message("[INFO] Gompertz equation added successfully.")
    } else {
      stop("[ERROR] SVG file not found: ", svg_file)
    }
  }

  return(p)
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
