# ---------------------------------------------------------------------------- #
# Metric 3: Author Analysis
# ---------------------------------------------------------------------------- #

# Load necessary files
message("Loaded file: _helpers.r")
source('../../src/M3_Authors/_helpers.r')

message("Loaded file: _utils.r")
source('../../src/M3_Authors/_utils.r')

message("Loaded file: _settings.r")
source('../../src/M3_Authors/_settings.r')

source('../../src/M3_Authors/m3_m0_eda.r')

M3_Authors <- setRefClass(
  "M3_Authors",

  # Fields
  fields = list(
    df = "data.frame",
    author_col = "character",
    year_col = "character",
    results = "list"
  ),

  # Methods
  methods = list(

    # Constructor
    initialize = function(df, author_col = "Authors", year_col = "Year") {
      .self$df <- df
      .self$author_col <- author_col
      .self$year_col <- year_col
      .self$results <- list()
    },

    # Run all metrics
    runMetrics = function() {
      message("=============== RUNNING METRICS ==================")

      # Metric 0: EDA
      m0_eda <- M3_M0_EDA$new(.self$df, .self$df_column_name_year, .self$df_column_name_articles)
      m0_eda$run()
      m0_eda$save_plot(.self$path_results_plots)
      m0_eda$save_json(.self$path_results_jsons)


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
