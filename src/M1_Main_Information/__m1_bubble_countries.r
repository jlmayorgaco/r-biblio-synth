# ---------------------------------------------------------------------------- #
# Function: Prepare Snapshot Data for Country Trend Analysis
# ---------------------------------------------------------------------------- #
prepare_country_trend_data <- function(data) {
  # Ensure required columns exist
  required_cols <- c("Year", "Country", "Total_Citations", "Total_Papers")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Divide the data into two periods: first half and second half
  years <- unique(data$Year)
  mid_year <- median(years)
  
  # Calculate metrics for each period
  first_half <- data %>% filter(Year <= mid_year) %>%
    group_by(Country) %>%
    summarise(
      Total_Citations_1 = sum(Total_Citations, na.rm = TRUE),
      Total_Papers_1 = sum(Total_Papers, na.rm = TRUE),
      Avg_Citations_1 = Total_Citations_1 / Total_Papers_1
    )
  
  second_half <- data %>% filter(Year > mid_year) %>%
    group_by(Country) %>%
    summarise(
      Total_Citations_2 = sum(Total_Citations, na.rm = TRUE),
      Total_Papers_2 = sum(Total_Papers, na.rm = TRUE),
      Avg_Citations_2 = Total_Citations_2 / Total_Papers_2
    )
  
  # Merge the two datasets
  trend_data <- full_join(first_half, second_half, by = "Country")
  
  return(trend_data)
}

# ---------------------------------------------------------------------------- #
# Function: Generate and Plot Country Trends with Movement Arrows
# ---------------------------------------------------------------------------- #
plot_country_trends <- function(trend_data, title = "Country Trends: Total Papers vs Total Citations") {
  # Prepare the plot
  p <- ggplot(trend_data, aes(x = Total_Citations_1, y = Total_Papers_1, size = Avg_Citations_1)) +
    geom_point(color = THEME_COLORS$Main[1], alpha = 0.7) +
    
    # Add arrows to show movement between the two periods
    geom_segment(
      aes(xend = Total_Citations_2, yend = Total_Papers_2),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "darkgray", alpha = 0.8
    ) +
    
    # Add points for the second snapshot (end point of the arrows)
    geom_point(aes(x = Total_Citations_2, y = Total_Papers_2, size = Avg_Citations_2), 
               color = THEME_COLORS$Main[3], alpha = 0.7) +
    
    labs(
      title = title,
      x = "Total Citations",
      y = "Total Papers",
      size = "Average Citations per Paper"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray70"),
      panel.grid.minor = element_line(color = "gray90")
    )
  
  # Return the plot
  return(p)
}

