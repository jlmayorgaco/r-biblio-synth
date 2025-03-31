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

      lower_bound <- NULL
      upper_bound <- NULL
          
      # Prepare regression_data structure
      regression_data <- list(
        metric_regression = .self$results,
        models_regression = model_function_map,
        x = x, 
        y = y,
        lower_bound = lower_bound,  # Ensure this exists
        upper_bound = upper_bound   # Ensure this exists
      )

      # Prepare residual_data structure
      residual_data <- calculate_residuals(x, y, regression_data)  # Ensure this function exists

      # **Create Directory Paths**
      one_column_path <- file.path(output_path, "OneColumn")
      double_column_path <- file.path(output_path, "DoubleColumn")
      json_output_path <- gsub("figures", "jsons", output_path)

      # **Ensure Clean Directories**
      create_clean_directory(one_column_path)
      create_clean_directory(double_column_path)
      create_clean_directory(json_output_path)

      # **List of Plot Classes**
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
      Standardized_Residuals_Plot$new(residual_data),
      Residuals_Squared_Plot$new(residual_data),
      Residuals_Squared_Model_Fit_Plot$new(residual_data),
      FFT_Residuals_Plot$new(residual_data)
      )

      # **List of Corresponding Plot Names**
      plot_names <- c("Regression_Articles", "ACF_Residuals", "PACF_Residuals",
                  "Histogram_Residuals", "QQ_Residuals", "DW_Test", 
                  "BP_Test", "Shapiro_Wilk", "Standardized_Residuals",
                  "Residuals_Squared", "Residuals_Squared_Model_Fit",
                  "FFT_Residuals")

      # **Save Each Plot**
      for (i in seq_along(plot_classes)) {
      m2_save_plot(plot_classes[[i]], "M1_Regression", plot_names[i], one_column_path, double_column_path)
      }

      # **Generate JSON Report**
      m2_save_json_report(plot_classes, json_output_path)
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











