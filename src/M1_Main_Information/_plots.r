# ---------------------------------------------------------------------------- #
# File: _plots.r
# Description: Functions to generate and save various types of plots
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Function: Generate Bar Plot with Customizable Parameters
# ---------------------------------------------------------------------------- #
generate_bar_plot <- function(  data, output_dir, title, x_label, y_label, file_name, x_var, y_var, threshold_var = NULL) {
  if (!is.null(threshold_var)) {
    data$cumulative_percentage <- cumsum(data[[y_var]]) / sum(data[[y_var]])
    threshold_row <- which(data$cumulative_percentage >= 0.8)[1]
    max_x_value <- max(data[[y_var]], na.rm = TRUE)
  } else {
    threshold_row <- NULL
    max_x_value <- NULL
  }

  bar_plot <- ggplot(data, aes(x = reorder(.data[[x_var]], .data[[y_var]]), y = .data[[y_var]])) +
    geom_bar(stat = "identity", fill = THEME_COLORS["Blue"]) +
    coord_flip() +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ieee_theme +
    theme(
      plot.title = element_text(size = 14, margin = margin(b = 15), hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, margin = margin(r = 15)),
      axis.text = element_text(size = 10),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  if (!is.null(threshold_row)) {
    bar_plot <- bar_plot +
      geom_vline(xintercept = threshold_row, linetype = "dashed", color = "black", linewidth = 1) +
      annotate(
        "text",
        x = threshold_row + 0.25,
        y = max_x_value - 0.75,
        label = "80%",
        color = "black",
        size = 4
      )
  }

  save_plot(bar_plot, file_name, width = 8, height = 6)
}

# ---------------------------------------------------------------------------- #
# Function: Generate Lorenz Curve
# ---------------------------------------------------------------------------- #
generate_inequality_curve <- function(data, output_dir) {
    # Ensure Articles is numeric and valid
    if (!is.numeric(data$Articles) || any(is.na(data$Articles))) {
        stop("[ERROR] 'Articles' column contains non-numeric or missing values.")
    }

    # Sort data by Articles in descending order
    data <- data[order(-data$Articles), ]

    # Calculate cumulative percentages
    cumulative_articles <- cumsum(data$Articles) / sum(data$Articles)
    cumulative_countries <- cumsum(rep(1, nrow(data))) / nrow(data)

    # Calculate Gini coefficient
    area_trapezoids <- (cumulative_articles[-1] + cumulative_articles[-length(cumulative_articles)]) * diff(cumulative_countries) / 2
    gini <- 2 * sum(area_trapezoids) - 1

    # Prepare Lorenz plot data
    lorenz_data <- data.frame(
        cumulative_articles = c(0, cumulative_articles, 1),
        cumulative_countries = c(0, cumulative_countries, 1)
    )

    # Generate Lorenz plot
    lorenz_plot <- ggplot(lorenz_data, aes(x = cumulative_articles, y = cumulative_countries)) +
        geom_line(color = THEME_COLORS["Green"], linewidth = 1.2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = THEME_COLORS["Darkgrey"]) +
        labs(
            title = "Lorenz Curve: Article Production by Countries",
            x = "Cumulative Percentage of Articles",
            y = "Cumulative Percentage of Countries"
        ) +
        coord_fixed(ratio = 1) +
        xlim(0, 1) + ylim(0, 1) +
        annotate("text", x = 0.2, y = 0.8, label = paste0("Gini Coefficient: ", round(gini, 3)), color = "black", size = 4, hjust = 0) +
        ieee_theme +
        theme(
            plot.title = element_text(size = 14, margin = margin(b = 15, t = 15)),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            axis.title.x = element_text(size = 10, margin = margin(t = 15, b = 5)),
            axis.title.y = element_text(size = 10, margin = margin(r = 15, l = 0)),
            axis.text = element_text(size = 8)
        )

    save_plot(lorenz_plot, "M1_MOST_PROD_COUNTRIES_LORENZ_PLOT", width = 5, height = 5, dpi = 600)
}

# ---------------------------------------------------------------------------- #
# Function: Generic Pie Chart Plotting
# ---------------------------------------------------------------------------- #
plot_pie_chart <- function(df, title, fill_var, count_var) {
  ggplot(df, aes(x = "", y = .data[[count_var]], fill = .data[[fill_var]])) +
    geom_bar(width = 1, stat = "identity", color = "black", size = 0.25) +
    coord_polar("y", start = 0) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "right"
    ) +
    scale_fill_manual(values = c(
      "#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc949"
    ))
}
