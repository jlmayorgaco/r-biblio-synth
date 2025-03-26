# ---------------------------------------------------------------------------- #
# -- Module 4 : M4 Countries Analysis ---------------------------------------- #
# ---------------------------------------------------------------------------- #

message("Loaded file: _helpers.r")
source('../../src/M4_Countries/_helpers.r')

message("Loaded file: _plots.r")
source('../../src/M4_Countries/plots/_plots.r')

source('../../src/M4_Countries/m4_m0_eda.r')
#source('../../src/M4_Countries/m4_m1_tcp_mcp.r')
#source('../../src/M4_Countries/m4_m2_tp_tc.r')

M4_Countries <- setRefClass(
  "M4_Countries",
  fields = list(
    df = "data.frame",
    df_column_name_country = "character",
    df_column_name_year = "character",
    path_results_jsons = "character",
    path_results_plots = "character"
  ),
  methods = list(

    initialize = function(df, country_col = "Country", year_col = "Year", report_path = "results/M4_Countries") {
      .self$df <- as.data.frame(df)
      .self$df_column_name_country <- country_col
      .self$df_column_name_year <- year_col
      .self$path_results_jsons <- file.path(report_path, "jsons")
      .self$path_results_plots <- file.path(report_path, "figures")
      ensure_directory(.self$path_results_jsons)
      ensure_directory(.self$path_results_plots)
    },

    runMetrics = function() {
      message("=============== RUNNING M4 METRICS ==================")

      # Metric 0: EDA
      m0_eda <- M4_M0_EDA$new(.self$df, .self$df_column_name_country)
      m0_eda$run()
      m0_eda$save_plot(.self$path_results_plots)
      m0_eda$save_json(.self$path_results_jsons)

      # Metric 1: SCP vs MCP
      #m1_tcp_mcp <- M4_M1_TCP_MCP$new(.self$df, .self$df_column_name_country, .self$df_column_name_year)
      #m1_tcp_mcp$run()
      #m1_tcp_mcp$save_plot(.self$path_results_plots)
      #m1_tcp_mcp$save_json(.self$path_results_jsons)

      # Metric 2: TP vs TC
      #m2_tp_tc <- M4_M2_TP_TC$new(.self$df, .self$df_column_name_country, .self$df_column_name_year)
      #m2_tp_tc$run()
      #m2_tp_tc$save_plot(.self$path_results_plots)
      #m2_tp_tc$save_json(.self$path_results_jsons)

      message("=============== M4 METRICS COMPLETED ==================")
    },

    runReport = function() {
      message("=============== RUN M4 REPORT ==================")
      # .self$runMetrics()
    }
  )
)
