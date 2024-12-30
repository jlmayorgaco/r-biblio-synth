# ---------------------------------------------------------------------------- #
# Metric 3: Most Productive Authors
# ---------------------------------------------------------------------------- #

# Load necessary files
message("Loaded file: _helpers.r")
source('../../src/M3_Most_Prod_Authors/_helpers.r')

message("Loaded file: _utils.r")
source('../../src/M3_Most_Prod_Authors/_utils.r')

message("Loaded file: _settings.r")
source('../../src/M3_Most_Prod_Authors/_settings.r')

source('../../src/M3_Most_Prod_Authors/m3_m0_eda.r')

M3_Most_Prod_Authors <- setRefClass(
  "M3_Most_Prod_Authors",

  # Fields
  fields = list(
    df = "data.frame",
    author_col = "character",
    articles_col = "character",   
    fractionalized_col = "character",
    path_results_jsons = "character",
    path_results_plots = "character",
    results = "list"
  ),


  # Methods
  methods = list(

    # Constructor
    initialize = function(df, author_col = "Authors", articles_col = "Articles", fractionalized_col = "Articles Fractionalized", report_path = "results/M3_Most_Prod_Authors") {
      .self$df <- df
      .self$author_col <- author_col
      .self$articles_col <- articles_col
      .self$fractionalized_col <- fractionalized_col
      .self$results <- list()

      .self$path_results_jsons <- file.path(report_path, "jsons")
      .self$path_results_plots <- file.path(report_path, "figures")

      ensure_directory(.self$path_results_jsons)
      ensure_directory(.self$path_results_plots)
    },

    runMetrics = function() {
      message("=============== RUNNING METRICS ==================")

      # Metric 0: EDA
      m0_eda <- M3_M0_EDA$new(.self$df, .self$author_col, .self$articles_col, .self$fractionalized_col)

      m0_eda$run()
      #m0_eda$save_plot(.self$path_results_plots)
      #m0_eda$save_json(.self$path_results_jsons)
    }

  )
)
