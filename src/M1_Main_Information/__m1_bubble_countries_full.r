# ---------------------------------------------------------------------------- #
# Function: bubble_countries_process_half_data
# ---------------------------------------------------------------------------- #
bubble_countries_process_half_data <- function(converted_data, N = 5) {
  
  # Column mapping
  column_mapping <- c(
    "AU" = "Authors", "DE" = "Keywords", "ID" = "Index Terms", "C1" = "Author Affiliations",
    "JI" = "Journal Abbreviation", "AB" = "Abstract", "RP" = "Corresponding Author", "DI" = "DOI",
    "SN" = "ISSN", "SO" = "Source Title", "LA" = "Language", "TC" = "Times Cited", 
    "PN" = "Page Number", "PU" = "Publisher", "DB" = "Database", "TI" = "Title", 
    "DT" = "Document Type", "VL" = "Volume", "PY" = "Publication Year", "AF" = "Full Author Names", 
    "J9" = "Journal Name", "AU_UN" = "Author University", "SR_FULL" = "Full Reference", 
    "AU_CO" = "Countries"
  )

  # Extract metadata
  data <- bibliometrix::metaTagExtraction(converted_data, "AU_CO")
  names(data) <- ifelse(names(data) %in% names(column_mapping), column_mapping[names(data)], names(data))
  
  # Split fields
  data$Authors_Array <- strsplit(data$Authors, ";")
  data$Countries_Array <- strsplit(data$Countries, ";")
  
  # Select necessary columns
  new_df <- data %>% select(`Publication Year`, Countries_Array, `Times Cited`)

  # Split the data by years
  split_result <- bubble_countries_split_data_by_years(new_df, N)
  df_total <- split_result$df_total
  df_first_half <- split_result$df_first_half
  df_second_half <- split_result$df_second_half

  # Summarize data
  summarize_data <- function(df) {
    df %>%
      mutate(SCP = if_else(lengths(Countries_Array) == 1, 1, 0),
             MCP = if_else(lengths(Countries_Array) > 1, 1, 0)) %>%
      unnest(Countries_Array) %>%
      group_by(Countries_Array) %>%
      summarise(
        Total_Paper_Count = n(),
        Total_Citations = sum(`Times Cited`, na.rm = TRUE),
        SCP_Count = sum(SCP, na.rm = TRUE),
        MCP_Count = sum(MCP, na.rm = TRUE)
      ) %>% ungroup()
  }

  list(
    df_total = df_total,
    df_first_half_summary = summarize_data(df_first_half),
    df_second_half_summary = summarize_data(df_second_half),
    df_half_ranges = split_result$half_ranges
  )
}

# ---------------------------------------------------------------------------- #
# Function: bubble_countries_split_data_by_years (Dynamic Split)
# ---------------------------------------------------------------------------- #
bubble_countries_split_data_by_years <- function(data, N = 5) {
  
  # Get the last year
  LastYear <- max(data$`Publication Year`, na.rm = TRUE)
  
  # Define ranges
  start_first_half <- LastYear - (2 * N)
  end_first_half <- LastYear - N
  start_second_half <- end_first_half + 1
  end_second_half <- LastYear
  
  # Split data
  df_total <- data
  df_first_half <- data %>% filter(`Publication Year` >= start_first_half & `Publication Year` <= end_first_half)
  df_second_half <- data %>% filter(`Publication Year` >= start_second_half & `Publication Year` <= end_second_half)

  list(
    df_total = df_total,
    df_first_half = df_first_half,
    df_second_half = df_second_half,
    half_ranges = data.frame(
      First_Half_Range = paste(start_first_half, end_first_half, sep = "-"),
      Second_Half_Range = paste(start_second_half, end_second_half, sep = "-")
    )
  )
}

# ---------------------------------------------------------------------------- #
# Function: bubble_countries_validate_and_prepare_country_data
# ---------------------------------------------------------------------------- #
bubble_countries_validate_and_prepare_country_data <- function(df_first_half_summary, df_second_half_summary) {
  
  country_data <- dplyr::full_join(
    df_first_half_summary %>% rename_with(~ paste0(., "_first_half")),
    df_second_half_summary %>% rename_with(~ paste0(., "_second_half")),
    by = c("Countries_Array_first_half" = "Countries_Array_second_half")
  )
  
  country_data <- country_data %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    mutate(across(where(is.character), ~ replace_na(., ""))) %>%
    mutate(
      delta_x = Total_Citations_second_half - Total_Citations_first_half,
      delta_y = Total_Paper_Count_second_half - Total_Paper_Count_first_half
    )
  
  return(country_data)
}

# ---------------------------------------------------------------------------- #
# Function: bubble_countries_filter_top_countries (Fixed)
# ---------------------------------------------------------------------------- #
bubble_countries_filter_top_countries <- function(data) {
  
  # Ensure Total_Citations and Total_Paper_Count exist
  if (!("Total_Citations" %in% colnames(data)) | !("Total_Paper_Count" %in% colnames(data))) {
    stop("[ERROR] Total_Citations or Total_Paper_Count columns are missing in the dataset.")
  }

  # Select top countries by citations and papers
  top_citations <- data %>% arrange(desc(Total_Citations)) %>% slice_head(n = 10)
  top_papers <- data %>% arrange(desc(Total_Paper_Count)) %>% slice_head(n = 10)
  #top_positive_trending
  #top_negative_trending

  # Merge both rankings, ensuring unique countries
  filtered_data <- bind_rows(top_citations, top_papers) %>%
    distinct(Countries_Array_first_half, .keep_all = TRUE)

  return(filtered_data)
}

# ---------------------------------------------------------------------------- #
# Function: bubble_countries_create_country_region_data
# ---------------------------------------------------------------------------- #
bubble_countries_create_country_region_data <- function(df_total) {

  # Ensure no infinite values
  df_total <- df_total %>%
    mutate(
      Total_Citations = ifelse(is.infinite(Total_Citations), NA, Total_Citations),
      Total_Paper_Count = ifelse(is.infinite(Total_Paper_Count), NA, Total_Paper_Count)
    )

  # Compute median values
  median_x <- median(df_total$Total_Citations, na.rm = TRUE)
  median_y <- median(df_total$Total_Paper_Count, na.rm = TRUE)

  # Define axis limits with padding
  delta_padding <- 0.25
  min_x <- max(1, floor(min(df_total$Total_Citations, na.rm = TRUE) * (1 - delta_padding)))
  max_x <- max(1, ceiling(max(df_total$Total_Citations, na.rm = TRUE) * (1 + delta_padding)))
  min_y <- max(1, floor(min(df_total$Total_Paper_Count, na.rm = TRUE) * (1 - delta_padding)))
  max_y <- max(1, ceiling(max(df_total$Total_Paper_Count, na.rm = TRUE) * (1 + delta_padding)))

  data.frame(
    median_x = median_x,
    median_y = median_y,
    xmin = c(median_x, min_x, median_x, min_x),
    xmax = c(max_x, median_x, max_x, median_x),
    ymin = c(median_y, median_y, min_y, min_y),
    ymax = c(max_y, max_y, median_y, median_y),
    region = factor(c("I: High Citations, High Papers", "II: High Citations, Low Papers", 
                      "III: Low Citations, High Papers", "IV: Low Citations, Low Papers"))
  )
}





# ---------------------------------------------------------------------------- #
# Function: bubble_countries_plot_country_bubble_chart (Final with Log Scale)
# ---------------------------------------------------------------------------- #
bubble_countries_plot_country_bubble_chart <- function(df_total, title) {
  
  # Compute total values if not already present
  if (!("Total_Citations" %in% colnames(df_total))) {
    df_total <- df_total %>%
      mutate(
        Total_Citations = Total_Citations_first_half + Total_Citations_second_half,
        Total_Paper_Count = Total_Paper_Count_first_half + Total_Paper_Count_second_half
      )
  }

  # Apply filtering
  filtered_data <- bubble_countries_filter_top_countries(df_total) %>%
    filter(Countries_Array_first_half != "")

  # Ensure filtered_data is not empty
  if (nrow(filtered_data) == 0) {
    stop("[ERROR] No valid data available after filtering. Check dataset content.")
  }

  # Ensure values are non-zero for log transformation
  filtered_data <- filtered_data %>%
    mutate(
      Total_Citations = ifelse(Total_Citations <= 0, 1, Total_Citations),
      Total_Paper_Count = ifelse(Total_Paper_Count <= 0, 1, Total_Paper_Count),
      delta_x = ifelse(delta_x == 0, 1, delta_x),
      delta_y = ifelse(delta_y == 0, 1, delta_y)
    )

  # Create quadrant regions
  region_data <- bubble_countries_create_country_region_data(filtered_data)

  # Apply log transformation to region_data
  region_data <- region_data %>%
    mutate(
      xmin = log10(pmax(xmin, 1)),
      xmax = log10(pmax(xmax, 1)),
      ymin = log10(pmax(ymin, 1)),
      ymax = log10(pmax(ymax, 1))
    )

  # Normalize arrow lengths
  max_delta_x <- max(abs(filtered_data$delta_x), na.rm = TRUE)
  max_delta_y <- max(abs(filtered_data$delta_y), na.rm = TRUE)

  # Avoid division by zero
  max_delta_x <- ifelse(max_delta_x == 0, 1, max_delta_x)
  max_delta_y <- ifelse(max_delta_y == 0, 1, max_delta_y)

  # Scale deltas
  filtered_data <- filtered_data %>%
    mutate(
      scaled_delta_x = delta_x / max_delta_x * (0.1 * max(Total_Citations, na.rm = TRUE)),  
      scaled_delta_y = delta_y / max_delta_y * (0.1 * max(Total_Paper_Count, na.rm = TRUE))
    )

  # Extract transformed limits
  min_x <- min(region_data$xmin, na.rm = TRUE)
  max_x <- max(region_data$xmax, na.rm = TRUE)
  min_y <- min(region_data$ymin, na.rm = TRUE)
  max_y <- max(region_data$ymax, na.rm = TRUE)

  message("\n[DEBUG] Region Data")
  print(region_data)

  message("\n[DEBUG] Plot Limits")
  message("min_x:", min_x, " max_x:", max_x)
  message("min_y:", min_y, " max_y:", max_y)

  ggplot(filtered_data, aes(x = log10(Total_Citations), y = log10(Total_Paper_Count), label = Countries_Array_first_half)) +

    # Adjusted Quadrant Regions (Log Transformed)
    geom_rect(data = region_data, aes(
        xmin = xmin, 
        xmax = xmax, 
        ymin = ymin, 
        ymax = ymax, 
        fill = region), 
        alpha = 0.12, inherit.aes = FALSE) +

    # Bubble Points
    geom_point(alpha = 0.7, color = "steelblue", size = 2) +

    # Arrows showing trends
    geom_segment(aes(
        x = log10(Total_Citations), 
        y = log10(Total_Paper_Count),
        xend = log10(Total_Citations + scaled_delta_x), 
        yend = log10(Total_Paper_Count + scaled_delta_y)),
        arrow = grid::arrow(length = grid::unit(0.2, "cm")),
        color = "darkred", size = 0.5) +

    # Labels
    geom_text_repel(size = 4, fontface = "bold") +

    # Logarithmic Scale
    # Keep continuous scale but format axis labels manually
    scale_x_continuous(expand = c(0, 0), labels = function(x) scales::comma(10^x)) +
    scale_y_continuous(expand = c(0, 0), labels = function(y) scales::comma(10^y)) +

    # Quadrant Colors
    scale_fill_manual(values = c("darkgreen", "gold", "darkblue", "purple"), name = "Quadrants") +

    # Labels & Theme
    labs(
      title = title,
      x = "Total Citations (Log Scale)",
      y = "Total Papers Published (Log Scale)"
    ) +

    # Theme
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "right",

        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_line(color = "gray90"),
        
        # Add a black border around the plot
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 1),
        
        # Ensure axis lines are visible
        axis.line = element_line(color = "gray70")
    )
}