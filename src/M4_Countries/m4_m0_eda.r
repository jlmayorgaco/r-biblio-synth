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
            Country_Array = if (!is.list(Country_Array)) strsplit(as.character(Country_Array), ";") else Country_Array,
            Country_Array = lapply(Country_Array, function(x) trimws(x))
          ) %>%
          mutate(
            MCP = purrr::map_int(Country_Array, ~ as.integer(length(unique(.x)) > 1)),
            SCP = purrr::map_int(Country_Array, ~ as.integer(length(unique(.x)) == 1)),
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
        #message("\n[DEBUG] top_bottom() called for column: ", colname)

        if (!colname %in% names(df)) stop(paste("[ERROR] Column", colname, "not found in dataframe"))
        if (!country_col %in% names(df)) stop(paste("[ERROR] Column", country_col, "not found in dataframe"))

        # limpiar "NA" como texto y NA reales
        df[[country_col]][df[[country_col]] == "NA"] <- NA
        df <- df[!is.na(df[[country_col]]), ]

        # limpiar NA en la columna numérica
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

      year_range <- range(df_expanded$Year, na.rm = TRUE)

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

      ## ------------------------------------------------------------------ ##
      ##  PLOT: TOPS TP, TC, MCP, SCP
      ## ------------------------------------------------------------------ ##
      agg        <- .self$results$aggregated
      year_range <- .self$results$year_range
      top_tp     <- .self$results$top_bottom$TP$top10
      top_tc     <- .self$results$top_bottom$TC$top10
      top_mcp    <- .self$results$top_bottom$MCP$top10
      top_scp    <- .self$results$top_bottom$SCP$top10

      # Asegura directorio de salida
      if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

      # 1) Top-10 by Total Publications (TP)
      render_top_metric(
        metric         = "TP",
        top_df         = top_tp,
        full_vec       = agg$TP,
        year_range     = year_range,
        x_axis_pattern = "Total Publications ({years})",
        out_dir        = output_path
      )

      # 2) Top-10 by Total Citations (TC)
      render_top_metric(
        metric         = "TC",
        top_df         = top_tc,
        full_vec       = agg$TC,
        year_range     = year_range,
        x_axis_pattern = "Total Citations ({years})",
        out_dir        = output_path
      )

      # 3) Top-10 by Multi-Country Publications (MCP)
      render_top_metric(
        metric         = "MCP",
        top_df         = top_mcp,
        full_vec       = agg$MCP,
        year_range     = year_range,
        x_axis_pattern = "Multi-Country Publications ({years})",
        out_dir        = output_path
      )

      # 4) Top-10 by Single-Country Publications (SCP)
      render_top_metric(
        metric         = "SCP",
        top_df         = top_scp,
        full_vec       = agg$SCP,
        year_range     = year_range,
        x_axis_pattern = "Single-Country Publications ({years})",
        out_dir        = output_path
      )
    }
  )
)

## ------------------------------------------------------------------
## Helper: render & save one Top-10 plot for a given metric
## ------------------------------------------------------------------
render_top_metric <- function(metric, top_df, full_vec, year_range, x_axis_pattern, out_dir) {
  p <- Plot_TopBottom10_Countries$new(
    data       = top_df,
    year_range = year_range,
    metric     = metric,
    type       = "top"
  )

  # Accuracy: Lorenz/Gini on the full distribution for that metric
  p$set_lorenz_values(full_vec)

  # Journal-friendly defaults (keep these values!)
  p$config$labels$title_in_plot  <- FALSE
  p$config$labels$title_pattern  <- "{type} Countries by {metric} ({years})"
  p$config$labels$x_axis_pattern <- x_axis_pattern
  p$config$labels$format_commas  <- TRUE

  # >>> PRESERVAR ESTOS VALORES <<<
  p$config$sizes$value_text <- 3.2
  p$config$insets$w     <- 0.28
  p$config$insets$h     <- 0.28
  p$config$insets$x_pad <- 0.03
  p$config$insets$y     <- 0.17

  p$generatePlot()

  # Si tienes save() multi-formato, usa: p$save(out_dir, export = c("png","svg","eps"))
  p$save(out_dir)

  invisible(p)
}
