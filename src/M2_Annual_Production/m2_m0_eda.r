# ---------------------------------------------------------------------------- #
# Metric 0: Exploratory Data Analysis (EDA)
# ---------------------------------------------------------------------------- #

M2_M0_EDA <- setRefClass(
  "M2_M0_EDA",

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

    # Run the EDA analysis
    run = function() {
      year_data <- .self$df[[.self$year_col]]
      articles_data <- .self$df[[.self$articles_col]]

      # Calculate basic EDA metrics
      start_year <- min(year_data, na.rm = TRUE)
      end_year <- max(year_data, na.rm = TRUE)
      peak_row <- .self$df[which.max(articles_data), ]
      min_row <- .self$df[which.min(articles_data), ]

      mean_articles <- mean(articles_data, na.rm = TRUE)
      median_articles <- median(articles_data, na.rm = TRUE)
      sd_articles <- sd(articles_data, na.rm = TRUE)
      total_articles <- sum(articles_data, na.rm = TRUE)

      # Detect anomalies and outliers
      anomalies <- get_anomalies(
        df = .self$df,
        value_col = .self$articles_col,
        time_col = .self$year_col
      )

      outliers_zscore <- detect_outliers_zscore(.self$df, .self$articles_col)
      identified_outliers <- identify_outliers(.self$df, .self$articles_col, .self$year_col)

      # Calculate moving averages
      window_sizes <- c(1, 5, 10)
      moving_averages <- lapply(window_sizes, function(window) {
        calculate_moving_average(.self$df, .self$articles_col, window_size = window)
      })

      # Store results
      .self$results <- list(
        start_year = start_year,
        end_year = end_year,
        peak_year = peak_row[[.self$year_col]],
        peak_articles = peak_row[[.self$articles_col]],
        min_year = min_row[[.self$year_col]],
        min_articles = min_row[[.self$articles_col]],
        mean_articles = mean_articles,
        median_articles = median_articles,
        sd_articles = sd_articles,
        total_articles = total_articles,
        anomalies = anomalies,
        outliers = list(
          zscore = outliers_zscore,
          identified = identified_outliers
        ),
        moving_averages_arrays = moving_averages,
        moving_averages_window_size = window_sizes
      )
    },

    # Save the plots
    save_plot = function(output_path) {
      message(" ==> M2_M0 :: save_plot")
      create_moving_average_multiplot(.self$results, output_path)
      create_moving_average_individual_plots(.self$results, output_path)
    },

    # Save the results to JSON
    save_json = function(output_path) {
      json_data <- toJSON(.self$results, pretty = TRUE, auto_unbox = TRUE)
      write(json_data, file = file.path(output_path, "m0_eda.json"))
    },

    # Save the report (optional, extend as needed)
    save_report = function(output_path) {
      message("[INFO] Report generation for Metric 0 is not implemented yet.")
    }
  )
)


create_moving_average_individual_plots <- function(metric_eda, output_dir) {
  
  moving_averages_window_size <- metric_eda$moving_averages_window_size
  moving_averages_arrays <- metric_eda$moving_averages_arrays
  
  # Generate labels dynamically based on the moving averages window size
  labels <- sapply(moving_averages_window_size, function(size) {
      paste("Avg ", size, " Years", sep = "")
  })
  
  # Convert lists to data frames with types
  df_list <- lapply(seq_along(moving_averages_arrays), function(i) {
      convert_to_df(moving_averages_arrays[i], labels[i])
  })
  
  # Combine all data frames
  combined_df <- do.call(rbind, df_list)
  data <- combined_df
  
  # Extract unique types from the data
  types <- unique(data$Type)
  
  # Loop through each type to create and save individual plots
  for (type in types) {
    # Filter data for the current type
    df_type <- subset(data, Type == type)
    
    # Define the output file paths
    output_file_png <- file.path(output_dir, paste0("SinglePlot_", gsub(" ", "_", type), ".png"))
    output_file_svg <- file.path(output_dir, paste0("SinglePlot_", gsub(" ", "_", type), ".svg"))
    
    # Create the plot
    p <- ggplot(df_type, aes(x = Year, y = Articles)) +
     geom_line() +  # Explicitly add the line
        geom_point() + # Explicitly add the points
      labs(
        title = paste(type, "Over the Years"),
        x = "Year",
        y = "Articles (Moving Average)"
      ) +
      ieee_theme + 
      scale_y_continuous(expand = c(0, 0), limits = range(df_type$Articles, na.rm = TRUE)) +
      scale_x_continuous(expand = c(0, 0), limits = range(df_type$Year, na.rm = TRUE))
    
    # Save the plot as PNG
    ggsave(filename = output_file_png, plot = p, width = 4, height = 3, dpi = 900)
    
    # Save the plot as SVG
    ggsave(filename = output_file_svg, plot = p, width = 4, height = 3, device = "svg")
  }
}
create_moving_average_multiplot <- function(results, output_path) {
  # Combine moving averages into a single data frame
  combined_df <- do.call(rbind, lapply(seq_along(results$moving_averages_arrays), function(i) {
    data.frame(
      Year = results$moving_averages_arrays[[i]]$Year,
      Articles = results$moving_averages_arrays[[i]]$Articles,
      Type = paste0("Avg ", results$moving_averages_window_size[i], " Years")
    )
  }))

  # Define a custom color palette
  num_types <- length(results$moving_averages_window_size)
  colors <- c("#8f2802", "#565656", "#cfcfcf")[1:num_types]  # Adjust if more types are present

  # Create the plot
  p <- ggplot(combined_df, aes(x = Year, y = Articles, color = Type)) +
    geom_line(linetype = "dashed") +  # Thin, dashed lines
    geom_point(size = 1.2, shape = 20) +  # Small points
    scale_color_manual(values = colors) +  # Custom colors
    labs(
      title = "Moving Averages Over Time",
      x = "Year",
      y = "Articles",
      color = "Window Size"
    ) +
    ieee_theme +  # Custom IEEE theme
    theme(
      legend.position = "bottom", 
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0))

  # File paths
  output_file_png <- file.path(output_path, "Multiplot_Moving_Averages_PNG.png")
  output_file_svg <- file.path(output_path, "Multiplot_Moving_Averages_SVG.svg")

  # Save the plots
  ggsave(filename = output_file_png, plot = p, width = 4, height = 3, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = 4, height = 3, device = "svg")
}
