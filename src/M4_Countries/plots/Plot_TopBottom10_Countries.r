
# -------------------------------------------
# PLOT CLASS: Top/Bottom Country Plots
# -------------------------------------------
Plot_TopBottom10_Countries <- R6::R6Class("Plot_TopBottom10_Countries",

  inherit = R_IEEE_Plot,

  public = list(
    data = NULL,
    gini = NULL,
    metric = NULL,
    year_range = NULL,
    type = NULL, # "top" or "bottom"
    country_col = "Country",
    value_col = NULL,

    # Constructor
    initialize = function(data, gini, year_range, metric, type = "top", country_col = "Country") {
      self$data <- data
      self$year_range <- year_range
      self$metric <- metric
      self$type <- type
      self$country_col <- country_col
      self$value_col <- metric

      title_prefix <- ifelse(type == "top", "Top 10", "Bottom 10")

        # Compute year range if available
        year_suffix <- ""
        if (!is.null(self$year_range) && length(self$year_range) == 2) {
            year_suffix <- paste0(" (", self$year_range[1], "–", self$year_range[2], ")")
        }


 
        
      super$initialize(
        title = paste(title_prefix, "Countries by", metric, "", year_suffix),
        x_label = metric,
        y_label = "Country"
      )

      message(' ')
      message(' ')
      message(' ')
      message(' ============> self$data <- data ')
      message(' ')
      print(data)
      message(' ')
      message(' ')
      message(' ')
      message(' ')
      message(' ')

      #self$generatePlot()
    },

generatePlot = function() {
  fill_color <- ifelse(self$type == "top", "steelblue", "darkred")

  # Sort and compute top 80% threshold
  df_sorted <- self$data[order(-self$data[[self$value_col]]), ]
  cum_sum <- cumsum(df_sorted[[self$value_col]])
  total_sum <- sum(df_sorted[[self$value_col]], na.rm = TRUE)
  index_80 <- which(cum_sum >= 0.8 * total_sum)[1]
  top80_countries <- df_sorted[[self$country_col]][1:index_80]

  # Mark countries to highlight
  self$data$Highlight <- self$data[[self$country_col]] %in% top80_countries

  # Base plot with reordered country factor
  p <- ggplot(self$data, aes_string(
    x = self$value_col,
    y = paste0("reorder(", self$country_col, ", ", self$value_col, ")")
  ))


  # Add bars
  p <- p + geom_bar(stat = "identity", fill = fill_color)

  # Add labels to bars
  p <- p + geom_text(
    aes_string(label = self$value_col),
    hjust = -0.2, size = 3.5, family = "serif"
  )



  p <- p + super$getTheme()

  p <- p + theme(
    axis.title.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    axis.text.y = element_text(margin = margin(r = 3))
  )

  # Add labels and theme
  p <- p + labs( 
    x = "", #self$x_label,
    y = ""  # <- remove y label
  )


  # Adjust x-axis limit
  max_val <- max(self$data[[self$value_col]], na.rm = TRUE)
  p <- p + xlim(0, max_val * 1.1)

  # Add horizontal line and label at 80% cutoff
  country_80 <- df_sorted[[self$country_col]][index_80]
  threshold_df <- data.frame(
    x = max_val * 0.95,
    y = country_80,
    label = "80%"
  )

  p <- p +
    geom_hline(
      yintercept = index_80,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5
    ) +
    geom_text(
      data = threshold_df,
      aes(x = x, y = y, label = label),
      vjust = -0.3,
      hjust = 1,
      family = "serif",
      size = 3
    )

    if (requireNamespace("ggbrace", quietly = TRUE)) {
    brace_df <- data.frame(
        y = c(index_80 + 0.4, 1 - 0.4),
        x = -max_val * 0.02
    )

    p <- p + ggbrace::geom_brace(
        data = brace_df,
        aes(x = x, y = y),
        inherit.aes = FALSE,
        direction = "left",
        label = "",
        rotate = TRUE,
        linewidth = 0.7
    ) + annotate("text",
        x = -max_val * 0.04,
        y = (index_80 + 1) / 2,
        label = "Top 80%",
        angle = 90,
        family = "serif",
        size = 3.5
    )
    }

    # Gini inset plot
    if (!is.null(self$gini)) {
        gini_df <- data.frame(x = 0.5, y = 0.5, label = paste0("Gini = ", round(self$gini, 3)))

        gini_plot <- ggplot(gini_df, aes(x = x, y = y, label = label)) +
        geom_text(
            size = 3.5,
            fontface = "bold",
            family = "serif"
        ) +
        theme_void() +
        theme(
            plot.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
        )

        gini_grob <- ggplotGrob(gini_plot)

        p <- p + annotation_custom(
        grob = gini_grob,
        xmin = max_val * 0.6, xmax = max_val * 1.05,
        ymin = -Inf, ymax = Inf
        )
    }

    self$plot <- p
}
,


    # Save plot to path with filename based on metric and type
    save = function(output_path) {
      fname <- paste0("m0_eda_", tolower(self$type), "10_", tolower(self$metric), ".png")
      fig <- self$getOneColumnPlot()
      ggsave(file.path(output_path, fname), plot = fig$plot, width = fig$width, height = fig$height, dpi = fig$dpi)
    }
  )
)
