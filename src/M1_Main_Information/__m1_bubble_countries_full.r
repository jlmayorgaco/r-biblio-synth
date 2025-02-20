library(R6)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(ggrepel)
library(bibliometrix)

BubbleCountryAnalysis <- R6Class("BubbleCountryAnalysis",
  public = list(
    data = NULL,
    df = NULL,
    N_years = 5,
    num_countries = 10,
    show_arrows = TRUE,
    show_scale_arrows = TRUE,
    processed_data = NULL,
    country_data = NULL,

    # Constructor
    initialize = function(data, N_years = 5, num_countries = 10, show_arrows = TRUE, show_scale_arrows = TRUE) {
      self$data <- data
      self$N_years <- N_years
      self$num_countries <- num_countries
      self$show_arrows <- show_arrows
      self$show_scale_arrows <- show_scale_arrows
      self$process_data()
    },

    # Step 1: Process Data
    process_data = function() {
      countries_per_year_scp_mcp <- self$extract_country_data(self$data, self$N_years)
      df_first_half_summary <- countries_per_year_scp_mcp$df_first_half_summary
      df_second_half_summary <- countries_per_year_scp_mcp$df_second_half_summary
      self$country_data <- self$validate_and_prepare_data(df_first_half_summary, df_second_half_summary)
    },

    # Step 4: Summarize Data
    summarize_data = function(df) {
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
        ) %>%
        ungroup()
    },


    # Step 2: Extract and Process Data
    extract_country_data = function(converted_data, N) {
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

      self$df <- data

      # Select necessary columns
      new_df <- data %>% select(`Publication Year`, Countries_Array, `Times Cited`)
      # Split the data by years
      split_result <- self$split_data_by_years(new_df, N)
      df_total <- split_result$df_total
      df_first_half_summary <- self$summarize_data(split_result$df_first_half)
      df_second_half_summary <- self$summarize_data(split_result$df_second_half)
      list(
        df_total = df_total, 
        df_first_half_summary = df_first_half_summary, 
        df_second_half_summary = df_second_half_summary
      )
    },

    # ---------------------------------------------------------------------------- #
    # Function: split_data_by_years (Dynamic Split)
    # ---------------------------------------------------------------------------- #
    # Step 3: Split Data by Year
    split_data_by_years = function(data, N) {
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
    },

    # Step 5: Validate and Prepare Data
    validate_and_prepare_data = function(df_first_half_summary, df_second_half_summary) {
      country_data <- dplyr::full_join(
        df_first_half_summary %>% rename_with(~ paste0(., "_first_half")),
        df_second_half_summary %>% rename_with(~ paste0(., "_second_half")),
        by = c("Countries_Array_first_half" = "Countries_Array_second_half")
      ) %>%
        mutate(across(where(is.numeric), ~ replace_na(., 0)),
               across(where(is.character), ~ replace_na(., "")),
               delta_x = Total_Citations_second_half - Total_Citations_first_half,
               delta_y = Total_Paper_Count_second_half - Total_Paper_Count_first_half)
    },

    # Step 6: Filter Top Countries
    filter_top_countries = function(data) {
      top_citations <- data %>% arrange(desc(Total_Citations)) %>% slice_head(n = self$num_countries)
      top_papers <- data %>% arrange(desc(Total_Paper_Count)) %>% slice_head(n = self$num_countries)
      output <- bind_rows(top_citations, top_papers) %>%
        distinct(Countries_Array_first_half, .keep_all = TRUE)
      return(output)
    },

    # Step 6: Filter Top Countries based on SCP and MCP
    filter_top_countries_scp_mcp = function(data) {
      # Select top countries based on highest SCP and MCP counts
      top_scp <- data %>%
        arrange(desc(Total_SCP)) %>%
        slice_head(n = self$num_countries)

      top_mcp <- data %>%
        arrange(desc(Total_MCP)) %>%
        slice_head(n = self$num_countries)

      # Combine both lists and ensure unique countries
      output <- bind_rows(top_scp, top_mcp) %>%
        distinct(Countries_Array_first_half, .keep_all = TRUE)

      return(output)
    },

    # Step 7: Create Region Data
    create_region_data = function(df_total) {
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
    },

    # Step 7: Create Region Data based on SCP and MCP
    create_region_data_scp_mcp = function(df_total) {
      # Ensure no infinite values in SCP and MCP
      df_total <- df_total %>%
        mutate(
          Total_SCP = ifelse(is.infinite(Total_SCP), NA, Total_SCP),
          Total_MCP = ifelse(is.infinite(Total_MCP), NA, Total_MCP)
        )

      # Compute median values for Total_SCP and Total_MCP
      median_x <- median(df_total$Total_SCP, na.rm = TRUE)
      median_y <- median(df_total$Total_MCP, na.rm = TRUE)

      # Define axis limits with padding
      delta_padding <- 0.25
      min_x <- max(1, floor(min(df_total$Total_SCP, na.rm = TRUE) * (1 - delta_padding)))
      max_x <- max(1, ceiling(max(df_total$Total_SCP, na.rm = TRUE) * (1 + delta_padding)))
      min_y <- max(1, floor(min(df_total$Total_MCP, na.rm = TRUE) * (1 - delta_padding)))
      max_y <- max(1, ceiling(max(df_total$Total_MCP, na.rm = TRUE) * (1 + delta_padding)))

      # Return data frame for quadrants based on SCP and MCP
      data.frame(
        median_x = median_x,
        median_y = median_y,
        xmin = c(median_x, min_x, median_x, min_x),
        xmax = c(max_x, median_x, max_x, median_x),
        ymin = c(median_y, median_y, min_y, min_y),
        ymax = c(max_y, max_y, median_y, median_y),
        region = factor(c("I: High SCP, High MCP", "II: High SCP, Low MCP", 
                          "III: Low SCP, High MCP", "IV: Low SCP, Low MCP"))
      )
    },




    get_top_countries_dataset = function (){
      return(self$country_data)
    },

    # ---------------------------------------------------------------------------- #
    # Function: generate_plot
    # ---------------------------------------------------------------------------- #
    # This function generates a bubble plot visualizing the total citations,
    # total papers published, and trends over the last N years for different countries.
    # ---------------------------------------------------------------------------- #
    generate_bubble_tp_vs_tc_plot = function() {
      
      # Step 1: Retrieve Processed Country Data
      df_total <- self$country_data
      
      # -------------------------------------------------------------------------- #
      # Step 2: Compute & Validate Total Citation and Paper Counts
      # -------------------------------------------------------------------------- #
      df_total <- df_total %>%
        mutate(
          Total_Citations = Total_Citations_first_half + Total_Citations_second_half,
          Total_Paper_Count = Total_Paper_Count_first_half + Total_Paper_Count_second_half
        )

      # -------------------------------------------------------------------------- #
      # Step 3: Filter Data (Selecting Top Countries)
      # -------------------------------------------------------------------------- #
      filtered_data <- self$filter_top_countries(df_total) %>%
        filter(Countries_Array_first_half != "")

      # Validate filtered data
      if (nrow(filtered_data) == 0) {
        stop("[ERROR] No valid data available after filtering. Check dataset content.")
      }

      # -------------------------------------------------------------------------- #
      # Step 4: Handle Log Transformations (Avoiding Negative or Zero Values)
      # -------------------------------------------------------------------------- #
      filtered_data <- filtered_data %>%
        mutate(
          Total_Citations = ifelse(Total_Citations <= 0, 1, Total_Citations),
          Total_Paper_Count = ifelse(Total_Paper_Count <= 0, 1, Total_Paper_Count),
          delta_x = ifelse(delta_x == 0, 1, delta_x),
          delta_y = ifelse(delta_y == 0, 1, delta_y)
        )

      # -------------------------------------------------------------------------- #
      # Step 5: Categorize Trends (Used for Coloring)
      # -------------------------------------------------------------------------- #
      filtered_data <- filtered_data %>%
        mutate(
          trend_category = case_when(
            delta_x > 0 & delta_y > 0 ~ "Positive",  # 🟢 Both metrics increased
            delta_x < 0 & delta_y < 0 ~ "Negative",  # 🔴 Both declined
            TRUE ~ "Mixed"  # 🟡 One increased, one decreased
          )
        )

      # -------------------------------------------------------------------------- #
      # Step 6: Define Quadrant Regions
      # -------------------------------------------------------------------------- #
      region_data <- self$create_region_data(filtered_data) %>%
        mutate(
          xmin = log10(pmax(xmin, 1)),
          xmax = log10(pmax(xmax, 1)),
          ymin = log10(pmax(ymin, 1)),
          ymax = log10(pmax(ymax, 1))
        )

      # -------------------------------------------------------------------------- #
      # Step 7: Normalize Arrows (Avoiding Large Variations)
      # -------------------------------------------------------------------------- #
      max_delta_x <- max(abs(filtered_data$delta_x), na.rm = TRUE)
      max_delta_y <- max(abs(filtered_data$delta_y), na.rm = TRUE)

      filtered_data <- filtered_data %>%
        mutate(
          scaled_delta_x = delta_x / max_delta_x * (0.1 * max(Total_Citations, na.rm = TRUE)),  
          scaled_delta_y = delta_y / max_delta_y * (0.1 * max(Total_Paper_Count, na.rm = TRUE))
        )

      # -------------------------------------------------------------------------- #
      # Step 8: Compute Axis Breaks (Log Scale)
      # -------------------------------------------------------------------------- #
      min_x <- min(region_data$xmin, na.rm = TRUE)
      max_x <- max(region_data$xmax, na.rm = TRUE)
      min_y <- min(region_data$ymin, na.rm = TRUE)
      max_y <- max(region_data$ymax, na.rm = TRUE)

      # Debugging output
      message("\n[DEBUG] Generating Log Breaks for X and Y axes")
      x_breaks <- self$generate_log_breaks(min_x, max_x)
      y_breaks <- self$generate_log_breaks(min_y, max_y)

      message("\n[DEBUG] X-Axis Major Breaks: ", paste(10^x_breaks$major, collapse = ", "))
      message("[DEBUG] X-Axis Minor Breaks: ", paste(10^x_breaks$minor, collapse = ", "))
      message("\n[DEBUG] Y-Axis Major Breaks: ", paste(10^y_breaks$major, collapse = ", "))
      message("[DEBUG] Y-Axis Minor Breaks: ", paste(10^y_breaks$minor, collapse = ", "))

      # Define Plot Title
      title <- paste0('Total Production, Total Citations and Trend ', 
                      2 * self$N_years, ' Last Years By Countries')

      # -------------------------------------------------------------------------- #
      # Step 9: Generate Plot
      # -------------------------------------------------------------------------- #
      ggplot(filtered_data, aes(
        x = log10(Total_Citations), 
        y = log10(Total_Paper_Count),
        label = Countries_Array_first_half,
        color = trend_category  # Assign colors based on trend category
      )) +
      
      # Quadrant Regions (Colored Backgrounds)
      geom_rect(
        data = region_data, 
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region), 
        alpha = 0.075, inherit.aes = FALSE
      ) +
      
      # Bubble Points
      geom_point(size = 2, alpha = 0.7) +
      
      # Arrows Showing Trends
      geom_segment(
        aes(
          xend = log10(Total_Citations + scaled_delta_x), 
          yend = log10(Total_Paper_Count + scaled_delta_y)
        ),
        arrow = grid::arrow(length = grid::unit(0.2, "cm")),
        show.legend = FALSE, size = 0.5
      ) +
      
      # Labels
      geom_text_repel(size = 4, fontface = "bold") +
      
      # Custom Log-Formatted X & Y Axis
      scale_x_continuous(
        expand = c(0, 0),
        breaks = x_breaks$minor,  
        minor_breaks = x_breaks$minor,
        labels = custom_log_formatter
      ) +
      
      scale_y_continuous(
        expand = c(0, 0),
        breaks = y_breaks$minor,  
        minor_breaks = y_breaks$minor,
        labels = custom_log_formatter
      ) +
      
      # Trend Colors and Legend Titles
      scale_color_manual(
        values = c(
          "Positive" = THEME_COLORS$Main[5],  # 🟢 Increasing Trend
          "Mixed" = THEME_COLORS$Grayscale$DarkGray,  # 🟡 Mixed Trend
          "Negative" = THEME_COLORS$Main[3]  # 🔴 Declining Trend
        ),
        name = "Trend Direction",  
        labels = c(
          "Positive" = "Positive Trend",  
          "Mixed" = "Mixed Trend",  
          "Negative" = "Negative Trend"  
        )
      ) +
      
      # X-Axis Growth Arrow
      geom_segment(
        aes(x = min_x, xend = max_x, y = (min_y + 0.001), yend = (min_y + 0.001)),
        arrow = grid::arrow(angle = 30, type = "closed", length = grid::unit(0.25, "cm")), 
        size = 0.8, color = "gray70", inherit.aes = FALSE
      ) +
      
      # Y-Axis Growth Arrow
      geom_segment(
        aes(x = (min_x + 0.001), xend = (min_x + 0.001), y = min_y, yend = max_y ),
        arrow = grid::arrow(angle = 30, type = "closed", length = grid::unit(0.25, "cm")), 
        size = 0.8, color = "gray70", inherit.aes = FALSE
      ) +
      
      # Quadrant Colors
      scale_fill_manual(
        values = c(THEME_COLORS$Main[5], THEME_COLORS$Main[1], THEME_COLORS$Main[2], "purple"), 
        name = "Quadrants"
      ) +
      
      # Labels & Theme
      labs(
        title = title,
        x = "Total Citations (Log Scale)",
        y = "Total Papers Published (Log Scale)"
      ) +
      
      # Theme Customization
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "right",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_line(color = "gray90"),
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 1),
        axis.line = element_line(color = "gray70")
      )
    },

    generate_bubble_scp_vs_mcp_plot = function() {
      
      # Step 1: Retrieve Processed Country Data
      df_total <- self$country_data
      
      # Step 2: Compute & Validate SCP and MCP Counts
      df_total <- df_total %>%
        mutate(
          Total_SCP = SCP_Count_first_half + SCP_Count_second_half,
          Total_MCP = MCP_Count_first_half + MCP_Count_second_half
        )

      # Step 3: Filter Data (Selecting Top Countries)
      filtered_data <- self$filter_top_countries_scp_mcp(df_total) %>%
        filter(Countries_Array_first_half != "")

      # Validate filtered data
      if (nrow(filtered_data) == 0) {
        stop("[ERROR] No valid data available after filtering. Check dataset content.")
      }

      # Step 4: Handle Log Transformations (Avoiding Negative or Zero Values)
      filtered_data <- filtered_data %>%
        mutate(
          Total_SCP = ifelse(Total_SCP <= 0, 1, Total_SCP),
          Total_MCP = ifelse(Total_MCP <= 0, 1, Total_MCP),
          delta_scp = ifelse(SCP_Count_second_half - SCP_Count_first_half == 0, 1, SCP_Count_second_half - SCP_Count_first_half),
          delta_mcp = ifelse(MCP_Count_second_half - MCP_Count_first_half == 0, 1, MCP_Count_second_half - MCP_Count_first_half)
        )

      # Step 5: Categorize Trends (Used for Coloring)
      filtered_data <- filtered_data %>%
        mutate(
          trend_category = case_when(
            delta_scp > 0 & delta_mcp > 0 ~ "Positive",  # 🟢 Both increased
            delta_scp < 0 & delta_mcp < 0 ~ "Negative",  # 🔴 Both declined
            TRUE ~ "Mixed"  # 🟡 One increased, one decreased
          )
        )

      # Step 6: Define Quadrant Regions
      region_data <- self$create_region_data_scp_mcp(filtered_data) %>%
        mutate(
          xmin = log10(pmax(xmin, 1)),
          xmax = log10(pmax(xmax, 1)),
          ymin = log10(pmax(ymin, 1)),
          ymax = log10(pmax(ymax, 1))
        )

      # Step 7: Normalize Arrows (Avoiding Large Variations)
      max_delta_scp <- max(abs(filtered_data$delta_scp), na.rm = TRUE)
      max_delta_mcp <- max(abs(filtered_data$delta_mcp), na.rm = TRUE)

      filtered_data <- filtered_data %>%
        mutate(
          scaled_delta_scp = delta_scp / max_delta_scp * (0.1 * max(Total_SCP, na.rm = TRUE)),  
          scaled_delta_mcp = delta_mcp / max_delta_mcp * (0.1 * max(Total_MCP, na.rm = TRUE))
        )

      # Step 8: Compute Axis Breaks (Log Scale)
      min_x <- min(region_data$xmin, na.rm = TRUE)
      max_x <- max(region_data$xmax, na.rm = TRUE)
      min_y <- min(region_data$ymin, na.rm = TRUE)
      max_y <- max(region_data$ymax, na.rm = TRUE)

      x_breaks <- self$generate_log_breaks(min_x, max_x)
      y_breaks <- self$generate_log_breaks(min_y, max_y)

      # Define Plot Title
      title <- paste0('Single-Country vs. Multi-Country Publications ', 
                      2 * self$N_years, ' Last Years By Countries')

      # Step 9: Generate Plot
      ggplot(filtered_data, aes(
        x = log10(Total_SCP), 
        y = log10(Total_MCP),
        label = Countries_Array_first_half,
        color = trend_category  # Assign colors based on trend category
      )) +
      
      # Quadrant Regions (Colored Backgrounds)
      geom_rect(
        data = region_data, 
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region), 
        alpha = 0.075, inherit.aes = FALSE
      ) +
      
      # Bubble Points
      geom_point(size = 2, alpha = 0.7) +
      
      # Arrows Showing Trends
      geom_segment(
        aes(
          xend = log10(Total_SCP + scaled_delta_scp), 
          yend = log10(Total_MCP + scaled_delta_mcp)
        ),
        arrow = grid::arrow(length = grid::unit(0.2, "cm")),
        show.legend = FALSE, size = 0.5
      ) +
      
      # Labels
      geom_text_repel(size = 4, fontface = "bold") +
      
      # Custom Log-Formatted X & Y Axis
      scale_x_continuous(
        expand = c(0, 0),
        breaks = x_breaks$minor,  
        minor_breaks = x_breaks$minor,
        labels = custom_log_formatter
      ) +
      
      scale_y_continuous(
        expand = c(0, 0),
        breaks = y_breaks$minor,  
        minor_breaks = y_breaks$minor,
        labels = custom_log_formatter
      ) +
      
      # Trend Colors and Legend Titles
      scale_color_manual(
        values = c(
          "Positive" = THEME_COLORS$Main[5],  # 🟢 Increasing Trend
          "Mixed" = THEME_COLORS$Grayscale$DarkGray,  # 🟡 Mixed Trend
          "Negative" = THEME_COLORS$Main[3]  # 🔴 Declining Trend
        ),
        name = "Trend Direction",  
        labels = c(
          "Positive" = "Positive Trend",  
          "Mixed" = "Mixed Trend",  
          "Negative" = "Negative Trend"  
        )
      ) +
      
      # Labels & Theme
      labs(
        title = title,
        x = "Single-Country Publications (SCP, Log Scale)",
        y = "Multi-Country Publications (MCP, Log Scale)"
      ) +
      
      # Theme Customization
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "right",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_line(color = "gray90"),
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 1),
        axis.line = element_line(color = "gray70")
      )
    },





    # Function to generate log-scale breaks for a given range
    generate_log_breaks = function(min_val, max_val) {
      message("\n[DEBUG] Generating Log Breaks")
      message("Original min_val: ", min_val, " | max_val: ", max_val)

      # Convert from log10 scale back to linear
      min_val <- 10^min_val
      max_val <- 10^max_val
      message("Converted to Linear Scale: min_val = ", min_val, " | max_val = ", max_val)

      # Compute power of 10 ranges
      log_min <- floor(log10(min_val))
      log_max <- ceiling(log10(max_val))
      message("Log Scale Range: 10^", log_min, " to 10^", log_max)

      # Generate major breaks at powers of 10
      major_breaks <- 10^(seq(log_min, log_max))
      message("Major Breaks: ", paste(major_breaks, collapse = ", "))

      # Generate minor breaks at 2x and 5x intervals
      minor_breaks <- sort(unique(c(major_breaks, 
                                    2 * major_breaks, 
                                    4 * major_breaks, 
                                    6 * major_breaks,
                                    8 * major_breaks 
                                    )))
      message("All Minor Breaks Before Filtering: ", paste(minor_breaks, collapse = ", "))

      # Keep breaks within valid range
      major_breaks <- major_breaks[major_breaks >= min_val & major_breaks <= max_val]
      minor_breaks <- minor_breaks[minor_breaks >= min_val & minor_breaks <= max_val]

      message("Final Major Breaks: ", paste(major_breaks, collapse = ", "))
      message("Final Minor Breaks: ", paste(minor_breaks, collapse = ", "))

      return(list(major = log10(major_breaks), minor = log10(minor_breaks)))
    },



    do_run_by_countries = function(){

      df <- self$df

      # Aggregate Data by Country and Year with Correct SCP and MCP Calculation
      country_year_summary <- df %>%
        # Compute SCP and MCP before unnesting
        mutate(
          SCP_Count = ifelse(lengths(Countries_Array) == 1, 1, 0),  # SCP: Single country
          MCP_Count = ifelse(lengths(Countries_Array) > 1, 1, 0)    # MCP: Multiple countries
        ) %>%
        unnest(Countries_Array) %>%  # Expand country lists into multiple rows
        group_by(Countries_Array, `Publication Year`) %>%
        summarise(
          Total_Paper_Count = n(),  # Count total papers per country per year
          Total_Citations = sum(`Times Cited`, na.rm = TRUE),  # Sum citations
          SCP_Count = sum(SCP_Count),  # Correctly sum SCP
          MCP_Count = sum(MCP_Count),  # Correctly sum MCP
          Density = ifelse(Total_Paper_Count > 0, Total_Citations / Total_Paper_Count, 0)  # Citations per paper
        ) %>%
        ungroup()



        # Sort by MCP_Count in descending order and get the top 10
        top_mcp_countries <- country_year_summary %>%
          arrange(desc(MCP_Count)) %>%
          slice_head(n = 10)

        # Print Debugging Output
        message("\n\n\n do_run_by_countries \n")
        print(head(top_mcp_countries, 10))
        message("\n\n\n")

        # Example usage
        plot_country_trends(country_year_summary, "results/M4_Countries/figures/byCountry/")


    }

)
)



# Function to format log labels
custom_log_formatter <- function(x) {
  formatted_values <- round(10^x)
  return(format(formatted_values, big.mark = ","))  # Adds thousand separators
}





# Function to plot and save each country's trends
plot_country_trends <- function(df, save_path = "plots/") {
  library(ggplot2)
  library(dplyr)

  # Ensure the save path is deleted and recreated
  if (dir.exists(save_path)) {
    unlink(save_path, recursive = TRUE)  # Deletes the directory and all its contents
  }
  dir.create(save_path, recursive = TRUE)  # Recreate the directory

  # Get unique country names
  unique_countries <- unique(df$Countries_Array)

  # Loop through each country and generate four plots
  for (country in unique_countries) {
    country_data <- df %>% filter(Countries_Array == country)

    # Ensure all subdirectories exist
    dir.create(paste0(save_path, "Total_Papers/"), recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0(save_path, "Total_Citations/"), recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0(save_path, "SCP_MCP/"), recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0(save_path, "Density/"), recursive = TRUE, showWarnings = FALSE)

    # === 1. Plot Total Papers Published ===
    p1 <- ggplot(country_data, aes(x = `Publication Year`, y = Total_Paper_Count)) +
      geom_line(color = "black", size = 1.2) +
      geom_point(color = "black", fill = "white", size = 3, shape = 21, stroke = 1.2) +
      labs(title = paste("Publication Trend in", country), x = "Year", y = "Total Papers Published") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line = element_line(color = "black")
      )
    ggsave(paste0(save_path, "Total_Papers/", gsub(" ", "_", country), "_publication_trend.png"), 
           plot = p1, width = 6.5, height = 3.5, dpi = 600)

    # === 2. Plot Total Citations ===
    p2 <- ggplot(country_data, aes(x = `Publication Year`, y = Total_Citations)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "blue", fill = "white", size = 3, shape = 21, stroke = 1.2) +
      labs(title = paste("Total Citations in", country), x = "Year", y = "Total Citations") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line = element_line(color = "black")
      )
    ggsave(paste0(save_path, "Total_Citations/", gsub(" ", "_", country), "_citation_trend.png"), 
           plot = p2, width = 6.5, height = 3.5, dpi = 600)

    # === 3. Plot SCP & MCP on the Same Plot ===
    p3 <- ggplot(country_data, aes(x = `Publication Year`)) +
      geom_line(aes(y = SCP_Count, color = "SCP"), size = 1.2) +
      geom_line(aes(y = MCP_Count, color = "MCP"), size = 1.2, linetype = "dashed") +
      geom_point(aes(y = SCP_Count), color = "red", fill = "white", size = 3, shape = 21, stroke = 1.2) +
      geom_point(aes(y = MCP_Count), color = "darkgreen", fill = "white", size = 3, shape = 21, stroke = 1.2) +
      labs(title = paste("SCP & MCP Trend in", country), x = "Year", y = "Count") +
      scale_color_manual(values = c("SCP" = "red", "MCP" = "darkgreen")) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line = element_line(color = "black")
      )
    ggsave(paste0(save_path, "SCP_MCP/", gsub(" ", "_", country), "_scp_mcp_trend.png"), 
           plot = p3, width = 6.5, height = 3.5, dpi = 600)

    # === 4. Plot Citations per Paper (Density) ===
    p4 <- ggplot(country_data, aes(x = `Publication Year`, y = Density)) +
      geom_line(color = "purple", size = 1.2) +
      geom_point(color = "purple", fill = "white", size = 3, shape = 21, stroke = 1.2) +
      labs(title = paste("Citations per Paper in", country), x = "Year", y = "Citations per Paper") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line = element_line(color = "black")
      )
    ggsave(paste0(save_path, "Density/", gsub(" ", "_", country), "_density_trend.png"), 
           plot = p4, width = 6.5, height = 3.5, dpi = 600)

    # Print messages
    message("Saved IEEE-styled plots for: ", country)
  }
}
