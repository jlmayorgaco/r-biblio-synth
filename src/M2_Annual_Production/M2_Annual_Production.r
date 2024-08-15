# ---------------------------------------------------------------------------- #
# -- Module 2 : M2 Annual Production ----------------------------------------- #
# ---------------------------------------------------------------------------- #
  # Set CRAN mirror
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  # Function to install and load packages
  install_and_load <- function(packages) {
    new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
    if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
    invisible(lapply(packages, require, character.only = TRUE))
  }

  # List of required packages
  packages <- c("ggplot2", "dplyr", "kableExtra", "knitr", "broom", "Metrics", "zoo" , "jsonlite", "rlang")

  # Install and load necessary packages
  install_and_load(packages)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# -- Packages and Install Dependencies
# ---------------------------------------------------------------------------- #
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  library(broom)
  library(Metrics)
  library(minpack.lm)
  library(rlang)
  library(zoo)
  library(jsonlite)

  # Load additional scripts
  source('../../src/M2_Annual_Production/M2_Annual_Production___Data_Cleaning.r')
  source('../../src/M2_Annual_Production/M2_Annual_Production___Metrics.r')
  source('../../src/M2_Annual_Production/M2_Annual_Production___Models.r')
  source('../../src/M2_Annual_Production/M2_Annual_Production___Regression.r')
  source('../../src/M2_Annual_Production/M2_Annual_Production___Plotter.r')
  source('../../src/M2_Annual_Production/M2_Annual_Production___Report.r')
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# -- M2_Annual_Production class
# ---------------------------------------------------------------------------- #
M2_Annual_Production <- setRefClass(

  # ClassName
  "M2_Annual_Production",

  # Attributes
  fields = list(

    df = "data.frame",
    df_column_name_year = "character",
    df_column_name_articles = "character",

    models_table = "ANY",

    models_best_obj = "ANY",
    models_best_name = "ANY",
    models_best_params = "ANY",

    path_results_jsons = "character",
    path_results_plots = "character"

  ),

  # Methods
  methods = list(
    # Constructor
    initialize = function(df, year_col = "Year", articles_col = "Articles", report_path = "results/M2_Annual_Production") {

      .self$df <- df
      .self$df_column_name_year <- year_col
      .self$df_column_name_articles <- articles_col
      .self$models_table <- list()
      .self$models_best_obj <- NULL
      .self$models_best_name <- ""
      .self$models_best_params <- ""
      .self$path_results_jsons <- paste0(report_path, '/jsons')
      .self$path_results_plots <- paste0(report_path, '/figures')

      .self$initializeReportDir()
    },
    
    # Initialize report directory
    initializeReportDir = function() {
      # Ensure the output directory exists
      if (dir.exists(.self$path_results_jsons)) {
        unlink(.self$path_results_jsons, recursive = TRUE)  # Delete the directory and its contents
      }
      # Create a new, clean output directory
      dir.create(.self$path_results_jsons, recursive = TRUE)

      # Ensure the output directory exists
      if (dir.exists(.self$path_results_plots)) {
        unlink(.self$path_results_plots, recursive = TRUE)  # Delete the directory and its contents
      }
      # Create a new, clean output directory
      dir.create(.self$path_results_plots, recursive = TRUE)
    },
    
    # Accessor methods
    getYearData = function() {
      return(.self$df[[.self$df_column_name_year]])
    },
    
    getArticlesData = function() {
      return(.self$df[[.self$df_column_name_articles]])
    },
    
    # Run regression models and determine best fit
    runRegression = function() {

      x <- .self$getYearData()
      y <- .self$getArticlesData()

      v_models_regression <- get_regression_models(x = x, y = y)
      v_models_table <- get_metrics_comparison_table(models = v_models_regression, data = .self$df)
      v_models_best_obj <- get_best_model(v_models_table)

      .self$models_table <- v_models_table
      .self$models_best_obj <- v_models_best_obj
      .self$models_best_name <- v_models_best_obj$name
      .self$models_best_params <- v_models_best_obj$params

    },
    
    # Run all metrics
    runMetrics = function() {
      list(
        eda = .self$runMetricEDA(),
        trend = .self$runMetricTrending(),
        periodic = .self$runMetricPeriodic()
      )
    },
    
    # Run EDA metric
    runMetricEDA = function() {
      x <- .self$getYearData()
      y <- .self$getArticlesData()
      
      df <- .self$df
      
      start_year <- min(x, na.rm = TRUE)
      end_year <- max(x, na.rm = TRUE)
      peak_row <- df[which.max(y), ]
      
      anomalies <- get_anomalies(
        df, 
        .self$df_column_name_articles, 
        .self$df_column_name_year, 
        .self$models_best_obj$name,
        .self$models_best_obj$params
      )

      window_sizes <- c(1, 5, 10)
      moving_avgs <- lapply(window_sizes, function(window) {
        df_avg <- data.frame(
          Year = df[[.self$df_column_name_year]], 
          Articles = df[[.self$df_column_name_articles]]
        )
        return(calculate_moving_average(df_avg, "Articles", window_size = window))
      })
      
      m0_eda <- list(
        start_date = start_year,
        end_year = end_year,
        peak_year = peak_row[[.self$df_column_name_year]],
        peak_articles = peak_row[[.self$df_column_name_articles]],
        anomalies = anomalies,
        outliers = list(
          zscore = detect_outliers_zscore(df, .self$df_column_name_articles),
          identified = identify_outliers(df, .self$df_column_name_articles, .self$df_column_name_year)
        ),
        moving_averages_arrays = moving_avgs,
        moving_averages_window_size = window_sizes
      )

      # Saving JSON 
      json_data <- toJSON(m0_eda, pretty = TRUE, auto_unbox = TRUE)
      write(json_data, file = file.path(.self$path_results_jsons, "m0_eda.json"))

      # Saving Plots
      create_moving_average_multiplot(m0_eda, .self$path_results_plots);
      create_moving_average_individual_plots(m0_eda, .self$path_results_plots);
      
      return(m0_eda)
    },
    
    # Run Trending metric
    runMetricTrending = function() {

      message(" ");message(" ");
      message(" [METRIC] M1 TREND");
      message(" ");message(" ");

      v_models_best_obj <- .self$models_best_obj
      v_models_table <- .self$models_table %>% select(-Model_Object)
      
      m1_trending <- list(
        model_name = v_models_best_obj$name,
        model_params = v_models_best_obj$params,
        model_r_squared = v_models_best_obj$R2,
        model_aic = v_models_best_obj$AIC,
        model_rmse = v_models_best_obj$RMSE,
        model_regression_others = v_models_table
      )

      x <- .self$df[.self$df_column_name_year]
      y <- .self$df[.self$df_column_name_articles]

      # Saving JSON 
      json_data <- toJSON(m1_trending, pretty = TRUE, auto_unbox = TRUE)
      write(json_data, file = file.path(.self$path_results_jsons, "m1_trending.json"))

      # Saving Plots
      create_regression_articles_plots(m1_trending, model_function_map, x, y, .self$path_results_plots)   
      
      # Create Regression Table
      create_regression_articles_table(v_models_table, .self$path_results_plots)
      return(m1_trending)
    },
    
    # Run Periodic metric
    runMetricPeriodic = function() {

      message(" ");message(" ");
      message(" [METRIC] M2 PERIODIC");
      message(" ");message(" ");
      
      is_periodic <- FALSE # Placeholder logic for periodic analysis
      
      m2_periodic <- list(
        is_periodic = is_periodic
      )
      
      #save_metric_m2_periodic_table(m2_periodic)
      #save_metric_m2_periodic_plots(m2_periodic)
      
      return(m2_periodic)
    },
    
    # Run report generation
    runReport = function() {
      message("=============== RUN REPORT ==================")
      # Uncomment the following line to run all metrics before generating the report
      # .self$runMetrics()
    }
  )
)
