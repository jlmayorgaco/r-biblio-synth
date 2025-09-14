# ---------------------------------------------------------------------------- #
# -- Module 4 : M4 Countries Analysis ---------------------------------------- #
# ---------------------------------------------------------------------------- #
install.packages("extrafont")
library(extrafont)
font_import()        # tarda un poco la primera vez
loadfonts(device="pdf")
loadfonts(device="postscript")

message("Loaded file: _helpers.r")
source('../../src/M4_Countries/_helpers.r')

message("Loaded file: _plots.r")
source('../../src/M4_Countries/plots/_plots.r')
source('../../src/M4_Countries/m4_m0_eda.r')
source('../../src/M4_Countries/m4_m1_tcp_mcp.r')
source('../../src/M4_Countries/m4_m2_geographic_mapping.r')
source('../../src/M4_Countries/m4_m3_mcp_share_trend.r')
#source('../../src/M4_Countries/m4_m4_collab_network_clusters.r')
#source('../../src/M4_Countries/m4_m5_country_meta.r')

# ======================================================================
# Normalize df_papers: rename useful columns + drop the unnecessary ones
# ======================================================================

normalize_papers_df <- function(df) {
  message("[M4_M5] Normalizing df_papers...")

  # Diccionario de mapeo de columnas
  col_map <- c(
    "Country_Array"       = "country_array",
    "Year"                = "year",
    "Title"               = "title",
    "Authors"             = "authors",
    "Authors_Array"       = "authors_array",
    "Keywords"            = "keywords",
    "Index Terms"         = "index_terms",
    "Author Affiliations" = "affiliations",
    "Source Title"        = "journal",
    "Language"            = "language",
    "Times Cited"         = "times_cited",
    "Document Type"       = "doc_type"
  )

  # Columnas a mantener
  keep_cols <- intersect(names(df), names(col_map))

  # Renombrar y quedarnos solo con las útiles
  df_clean <- df[, keep_cols, drop = FALSE]
  names(df_clean) <- unname(col_map[keep_cols])

  message("[M4_M5] Columns after normalization:")
  print(names(df_clean))

  return(df_clean)
}


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

      # Metric 1: TP vs TP and SCP vs MCP
      m1_tcp_mcp <- M4_M1_QUADRANTS_TP_TC_MCP_SCP$new(.self$df, 
      .self$df_column_name_country, .self$df_column_name_year)
      m1_tcp_mcp$run()
      m1_tcp_mcp$save_plot(.self$path_results_plots)
      m1_tcp_mcp$save_json(.self$path_results_jsons)


      m2_geo_tp <- M4_M2_GEO_MAPPING$new(
        df,
        country_col = .self$df_column_name_country,
        year_col    = .self$df_column_name_year,
        tc_col      = "Times Cited",
        metric      = "TP",
        palette     = "viridis",
        projection  = "robinson",
        label_top_n = 0,
        bin_style   = "jenks",
        bins        = 7,
        show_ocean  = TRUE,
        ocean_fill  = "#f6f7fa",
        show_north  = FALSE,
        north_scale = 0.04,
        north_pos   = c(left = 0.06, bottom = 0.78, width = 0.09, height = 0.12),
        show_scale  = FALSE,
        scale_km    = 1000,
        scale_cells = 10,
        debug       = TRUE   # optional: see logs
      )


      m2_geo_tp$run()
      m2_geo_tp$save_plot(.self$path_results_plots) 
      m2_geo_tp$save_json(.self$path_results_jsons)



  m2_geo_tc <- M4_M2_GEO_MAPPING$new(
        df,
        country_col = .self$df_column_name_country,
        year_col    = .self$df_column_name_year,
        tc_col      = "Times Cited",
        metric      = "TC",
        palette     = "viridis",
        projection  = "robinson",
        label_top_n = 0,
        bin_style   = "jenks",
        bins        = 7,
        show_ocean  = TRUE,
        ocean_fill  = "#f6f7fa",
        show_north  = FALSE,
        north_scale = 0.04,
        north_pos   = c(left = 0.06, bottom = 0.78, width = 0.09, height = 0.12),
        show_scale  = FALSE,
        scale_km    = 1000,
        scale_cells = 10,
        debug       = TRUE   # optional: see logs
      )

      m2_geo_tc$run()
      m2_geo_tc$save_plot(.self$path_results_plots) 
      m2_geo_tc$save_json(.self$path_results_jsons)
         # Metric 4: MCP Share Over Time (Hypothesis 1)



      print(' ')
      print(' ')
      print(' ')
      print(' m3_mcp_share <- M4_M3_MCP_SHARE_TREND$new :: df ')
      print(names(df))
      print(' ')
      print(' ')

      m3_mcp_share <-  M4_M3_MCP_SHARE_TREND$new(
        df,
        country_col = "Country_Array",   # your column
              year_col         = .self$df_column_name_year,
        countries_is_raw = TRUE
      )

      m3_mcp_share$run()
      m3_mcp_share$save_plots(.self$path_results_plots)
      m3_mcp_share$save_json(.self$path_results_jsons)
        

    },


    

    runReport = function() {
      message("=============== RUN M4 REPORT ==================")
      # .self$runMetrics()
    }
  )
)
