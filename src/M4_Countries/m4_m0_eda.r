# ---------------------------------------------------------------------------- #
# Metric 0: Exploratory Data Analysis (EDA) for M4 Countries (Extended Version)
# ---------------------------------------------------------------------------- #
library(dplyr)
library(purrr)
library(ineq)
library(fitdistrplus)
library(moments)
library(skimr)
library(ggplot2)
library(tidyr)
library(GGally)

M4_M0_EDA <- setRefClass(
  "M4_M0_EDA",

  fields = list(
    df = "data.frame",
    df_expanded = "data.frame",
    country_col = "character",
    results = "list"
  ),

  methods = list(

    # Constructor
    initialize = function(df, country_col = "Country") {
      .self$df <- as.data.frame(df)
      .self$country_col <- country_col
      .self$results <- list()
    },

    run = function() {

      library(dplyr)
      library(purrr)
      library(tidyr)
      library(ineq)
      library(fitdistrplus)
      library(moments)
      library(skimr)

      df <- .self$df
      country_col <- .self$country_col
      citation_col <- "Times Cited"

      # Step 1: Add MCP, SCP, TCP, Citations
      df <- df %>%
        mutate(
          MCP = purrr::map_int(Country_Array, ~ length(unique(.x)) > 1),
          SCP = purrr::map_int(Country_Array, ~ length(unique(.x)) == 1),
          TCP = 1,
          Citations = ifelse(is.na(.data[[citation_col]]), 0, .data[[citation_col]])
        )

      # Step 2: Prepare Country field for unnesting
      df <- df %>% rename(Country_Combined = Country)
      if (!is.list(df$Country_Array)) {
        df$Country_Array <- strsplit(df$Country_Combined, ";")
      }

      # Step 3: Expand by country
      df_expanded <- df %>%
        unnest(Country_Array) %>%
        rename(Country = Country_Array)

      .self$df_expanded <- df_expanded

      # Step 4: Aggregated statistics by country
      agg_by_country <- df_expanded %>%
        group_by(!!sym(country_col)) %>%
        summarise(
          TP = sum(TCP),
          TC = sum(Citations),
          MCP = sum(MCP),
          SCP = sum(SCP),
          MCP_percent = round(100 * sum(MCP) / n(), 2),
          SCP_percent = round(100 * sum(SCP) / n(), 2),
          Citations_per_Article = ifelse(sum(TCP) > 0, sum(Citations) / sum(TCP), 0),
          .groups = "drop"
        ) %>%
        mutate(Score = TC + log1p(TP) + 2 * MCP)

      
      # Step 5: Clustering countries
      clustering_data <- dplyr::select(agg_by_country, TP, TC, SCP, MCP) %>% scale()
      kmeans_result <- kmeans(clustering_data, centers = 3, nstart = 25)
      agg_by_country$Cluster <- as.factor(kmeans_result$cluster)

      # Step 6: Top/Bottom statistics
        top_bottom <- function(df, colname, country_col = "Country") {
            message("\n[DEBUG] top_bottom() called for column: ", colname)

            # Validación: columna numérica existe
            if (!colname %in% names(df)) stop(paste("[ERROR] Column", colname, "not found in dataframe"))
            if (!country_col %in% names(df)) stop(paste("[ERROR] Column", country_col, "not found in dataframe"))

            # Limpiar valores "NA" como texto y NA reales en columna de país
            df[[country_col]][df[[country_col]] == "NA"] <- NA
            df <- df[!is.na(df[[country_col]]), ]

            # Limpiar NA en la columna numérica
            vec <- df[[colname]]
            if (!is.numeric(vec)) stop(paste("[ERROR] Column", colname, "is not numeric."))
            df <- df[!is.na(vec), ]

            # Top 10 y Bottom 10 (excluye ceros en bottom)
            top10 <- tryCatch({ df[order(-df[[colname]]), ][1:10, ] }, error = function(e) NULL)
            bottom10 <- tryCatch({
                df_bottom <- df[df[[colname]] > 0, ]
                df_bottom[order(df_bottom[[colname]]), ][1:10, ]
            }, error = function(e) NULL)

            list(top10 = top10, bottom10 = bottom10)
    }





      top_bottom_stats <- list(
        TP = top_bottom(agg_by_country, "TP"),
        TC = top_bottom(agg_by_country, "TC"),
        SCP = top_bottom(agg_by_country, "SCP"),
        MCP = top_bottom(agg_by_country, "MCP")
      )

      # Step 7: Gini Coefficients
      gini_stats <- list(
        TP = Gini(agg_by_country$TP),
        TC = Gini(agg_by_country$TC),
        SCP = Gini(agg_by_country$SCP),
        MCP = Gini(agg_by_country$MCP)
      )

      # Step 8: Distribution fitting
      fit_distributions <- function(values, name) {
        list(
          variable = name,
          shapiro_p = tryCatch(shapiro.test(values)$p.value, error = function(e) NA),
          skewness = moments::skewness(values),
          kurtosis = moments::kurtosis(values),
          norm_fit = tryCatch(fitdist(values, "norm")$estimate, error = function(e) NULL),
          lnorm_fit = tryCatch(fitdist(values, "lnorm")$estimate, error = function(e) NULL)
        )
      }

      dist_fits <- list(
        TP = fit_distributions(agg_by_country$TP, "TP"),
        TC = fit_distributions(agg_by_country$TC, "TC"),
        SCP = fit_distributions(agg_by_country$SCP, "SCP"),
        MCP = fit_distributions(agg_by_country$MCP, "MCP")
      )

      # Step 9: Summary table
      summary_table <- skimr::skim(df_expanded)

      year_range <-range(df_expanded$Year, na.rm = TRUE);

  
      # Final Results
      .self$results <- list(
        summary = list(
          total_articles = nrow(df),
          unique_countries = length(unique(df_expanded[[country_col]]))
        ),
        year_range = year_range,
        aggregated = agg_by_country,
        top_bottom = top_bottom_stats,
        gini = gini_stats,
        distributions = dist_fits,
        summary_table = summary_table
      )
    },

    save_json = function(output_path) {
      jsonlite::write_json(.self$results, path = file.path(output_path, "m0_eda.json"), pretty = TRUE, auto_unbox = TRUE)
    },

   save_plot = function(output_path) { 

        library(ggplot2)
        library(GGally)
        library(ineq)
        library(dplyr)
        library(gridExtra)

        ## ################################################################### ##
        ## 
        ##  PLOT: TOPS TP, TC, MCP, SCP
        ## 
        ## ################################################################### ##
        agg <- .self$results$aggregated
        year_range <- .self$results$year_range
        top_tp <- .self$results$top_bottom$TP$top10
        top_tc <- .self$results$top_bottom$TC$top10
        top_mcp <- .self$results$top_bottom$MCP$top10
        top_scp <- .self$results$top_bottom$SCP$top10

        # Top 10 by TP
        plot_tp_top <- Plot_TopBottom10_Countries$new(
            data = results$top_bottom$TP$top10,
            year_range = year_range,
            metric = "TP",
            type = "top"
        ) 
        plot_tp_top$generatePlot()
        message(' ')
        message(' ')
        message('  plot_tp_top$gini  ====> ')
        print( .self$results$gini )
        message(' ')
        message(' plot_tp_top$gini ')
        message(plot_tp_top$gini)
        message(' ')
        message(' ')
        message(' ')
        plot_tp_top$save("results/M4_Countries/figures")

        

        
   }
  )
)
