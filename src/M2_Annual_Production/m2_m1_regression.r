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
  
  # Validate the selected model
  if (is.null(metric_regression)) {
    stop("[ERROR] `metric_regression` is NULL.")
  }

  # Retrieve the model function and parameters
  v_model <- models_regression[[metric_regression$model_name]]
  
  # Parse the parameters
  v_params <- parse_params(metric_regression$model_params)
  v_r_squared <- metric_regression$model_r_squared

  message(' ')
  message(' ')
  message(' ========> v_r_squared <- metric_regression$model_r_squared ')
  message(' v_r_squared')
  message(v_r_squared)
  message(' ')
  message(' ')
  
  # Prepare the real data
  t_real <- x
  y_real <- y

  # Generate regression points
  dt <- 0.1
  t_regression <- seq(min(t_real), max(t_real), by = dt)
  y_regression <- do.call(v_model, c(list(t = t_regression), v_params))

  # Data frames for plotting
  df_real <- data.frame(Year = t_real, Articles = y_real)
  df_regression <- data.frame(Year = t_regression, Articles = y_regression)

  # Create the plot
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
      panel.background = element_rect(fill = "white", color = NA), # White inside axes
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      axis.line = element_line(color = "black", linewidth = 0.5), # Increased thickness for axes
      panel.grid.major = element_line(linewidth = 0.4, color = "#dddddd"), # Major grid lines
      panel.grid.minor = element_line(linewidth = 0.2, color = "#f1f1f1"), # Subgrid with half resolution
      panel.border = element_blank(), # Removed the panel border
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold")
    ) +
    scale_y_continuous(
      breaks = scales::extended_breaks()(y_real),  # Major grid breaks
      minor_breaks = scales::extended_breaks(n = 10)(y_real) # Subgrid breaks
    ) +
    scale_x_continuous(
      breaks = scales::extended_breaks()(t_real), # Major grid breaks
      minor_breaks = scales::extended_breaks(n = 10)(t_real) # Subgrid breaks
    )

  # Add a text annotation for the model name and R² value
  #label <- bquote(.(metric_regression$model_name) ~ "\n" ~ R^2 ~ "=" ~ .(sprintf("%.3f", v_r_squared)))
  label_model <-metric_regression$model_name
  p <- p + annotate(
    "text",
    x = min(t_real) + 2, # Adjust the X position
    y = max(y_real) - 5, # Adjust the Y position
    label = label_model,
    hjust = 0, 
    vjust = 1, 
    size = 4.5, 
    color = "black",
    parse = FALSE # Do not re-parse the expression; `bquote()` handles it
  )

    # Ensure v_r_squared is valid and properly formatted
    if (!is.null(v_r_squared) && !is.na(v_r_squared)) {
    label_r2 <- paste0("R^2 == ", sprintf("%.3f", v_r_squared)) # Use '==' for mathematical expressions
    p <- p + annotate(
        "text",
        x = min(t_real) + 2, # Adjust the X position
        y = max(y_real) - 12, # Adjust the Y position
        label = label_r2,
        hjust = 0, 
        vjust = 1, 
        size = 4.5, 
        color = "black",
        parse = TRUE # Enable parsing for the R^2 expression
    )
    } else {
        message("[WARNING] R-squared value is missing or invalid.")
    }

  # Define file paths for saving plots
  output_file_png <- file.path(output_path, "Regression_Articles_Plot.png")
  output_file_svg <- file.path(output_path, "Regression_Articles_Plot.svg")
  
  # Save the plot with wider dimensions
  ggsave(filename = output_file_png, plot = p, width = 6, height = 4, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = 6, height = 4, device = "svg")
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
