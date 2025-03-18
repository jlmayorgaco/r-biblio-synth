# ---------------------------------------------------------------------------- #
# -- Module 2 : M2 Annual Production ----------------------------------------- #
# ---------------------------------------------------------------------------- #

# Load necessary files
message("Loaded file: _helpers.r")
source('../../src/M2_Annual_Production/_helpers.r')

message("Loaded file: _utils.r")
source('../../src/M2_Annual_Production/_utils.r')

message("Loaded file: _settings.r")
source('../../src/M2_Annual_Production/_settings.r')

message("Loaded file: _performances.r")
source('../../src/M2_Annual_Production/_performances.r')

message("Loaded file: _plotsr")
source('../../src/M2_Annual_Production/plots/_plots.r')

source('../../src/M2_Annual_Production/m2_m0_eda.r')
source('../../src/M2_Annual_Production/m2_m1_regression.r')
source('../../src/M2_Annual_Production/m2_m2_harmonics.r')

# ---------------------------------------------------------------------------- #
# -- M2_Annual_Production Class
# ---------------------------------------------------------------------------- #
M2_Annual_Production <- setRefClass(

  # ClassName
  "M2_Annual_Production",

  # Attributes
  fields = list(
    df = "data.frame",
    df_column_name_year = "character",
    df_column_name_articles = "character",
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
      .self$path_results_jsons <- file.path(report_path, "jsons")
      .self$path_results_plots <- file.path(report_path, "figures")
      ensure_directory(.self$path_results_jsons)
      ensure_directory(.self$path_results_plots)
    },

    # Run all metrics
    runMetrics = function() {
      message("=============== RUNNING METRICS ==================")

      # Metric 0: EDA
      m0_eda <- M2_M0_EDA$new(.self$df, .self$df_column_name_year, .self$df_column_name_articles)
      m0_eda$run()
      m0_eda$save_plot(.self$path_results_plots)
      m0_eda$save_json(.self$path_results_jsons)

      # Metric 1: Regression
      m1_regression <- M2_M1_Regression$new(.self$df, .self$df_column_name_year, .self$df_column_name_articles)
      m1_regression$run()
      m1_regression$save_plot(.self$path_results_plots)
      m1_regression$save_json(.self$path_results_jsons)

      # Metric 2: Harmonics and Periodicis
      m2_harmonics <- M2_M2_Harmonic$new(.self$df, .self$df_column_name_year, .self$df_column_name_articles)
      m2_harmonics$run()
      m2_harmonics$save_plot(.self$path_results_plots)
      m2_harmonics$save_json(.self$path_results_jsons)


      message("=============== METRICS COMPLETED ==================")
    },

    # Run report generation
    runReport = function() {
      message("=============== RUN REPORT ==================")
      # Uncomment the following line to run all metrics before generating the report
      # .self$runMetrics()
    }
  )
)
