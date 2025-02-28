# ---------------------------------------------------------------------------- #
# M2 Main Information Analysis Script
# ---------------------------------------------------------------------------- #

# Libraries
library(ggwordcloud)
library(ggplot2)
library(jsonlite)
library(bibliometrix)
library(wordcloud2)
library(htmlwidgets)
library(cluster)
library(dplyr)
library(scales)
library(webshot)
library(stopwords)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(scales)

# Load Helper Functions and Additional Scripts
source('../../src/M1_Main_Information/_helpers.r')
source('../../src/M1_Main_Information/_plots.r')
source('../../src/M1_Main_Information/_report.r')
source('../../src/M1_Main_Information/__m1_keywords_clouds_charts.r')
source('../../src/M1_Main_Information/__m1_scp_mcp_stacked_bar_chart.r')
source('../../src/M1_Main_Information/__m1_bubble_countries.r')
source('../../src/M1_Main_Information/__m1_bubble_countries_full.r')

source('../../src/M1_Main_Information/__m1_report.r')

# ---------------------------------------------------------------------------- #
# Function: fn_m1_main_information
# Description: Main information analysis using bibliometrix data.
# ---------------------------------------------------------------------------- #
fn_m1_main_information <- function(bib_data) {
  
  # Step 1: Bibliometric Analysis
  res1 <- biblioAnalysis(bib_data, sep = ";")
  s1 <- summary(res1, pause = FALSE, verbose = FALSE)

  message("======= Summary of Bibliometric Analysis =======")
  print(s1)
  
  # Step 2: Extract and Clean Data
  extracted_data <- extract_bibliographic_data(bib_data, res1)
  
  summary_df <- clean_whitespace(s1$MainInformationDF)
  most_prod_authors <- clean_whitespace(s1$MostProdAuthors)
  annual_production <- clean_whitespace(s1$AnnualProduction)
  most_cited_papers <- clean_whitespace(s1$MostCitedPapers)
  most_prod_countries <- clean_whitespace(s1$MostProdCountries)
  tc_per_countries <- clean_whitespace(s1$TCperCountries)
  most_rel_sources <- clean_whitespace(s1$MostRelSources)
  most_rel_keywords <- clean_whitespace(s1$MostRelKeywords)

  # Step 3: Assign Unique IDs to Most Cited Papers
  most_cited_papers <- most_cited_papers %>%
    dplyr::mutate(
      PaperID = dplyr::case_when(
        DOI %in% bib_data$DI ~ paste0("[", match(DOI, bib_data$DI), "]"),
        TRUE ~ NA_character_
      )
    )

  # Check for Missing IDs
  if (any(is.na(most_cited_papers$PaperID))) {
    warning("[WARNING] Some papers could not be assigned unique IDs.")
  }

  if (!"PaperID" %in% colnames(most_cited_papers)) {
    stop("[ERROR] Unable to assign unique IDs. Check DOI consistency.")
  }

  # Step 4: Compute Bradford's Law
  bradford_law <- bradford(bib_data)$table

  # Step 5: Organize Results into a Structured List
  main_information_data <- list(
    main_information = extract_main_information(summary_df),
    most_cited_papers = most_cited_papers,
    most_prod_countries = most_prod_countries,
    tc_per_countries = tc_per_countries,
    most_rel_sources = most_rel_sources,
    most_rel_keywords = most_rel_keywords,
    most_prod_authors = most_prod_authors,
    author_prod_over_time = annual_production,
    bradford_law = bradford_law,
    extracted_data = extracted_data
  )
  
  return(main_information_data)
}

# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc1 :: Save Document Types Pie Chart
# Description: Creates and saves a pie chart showing the distribution of document types.
# ---------------------------------------------------------------------------- #
fn_m1_mtrc1_articles_types_pie <- function(v_document_types) {
  
  # Step 1: Prepare Data Frame
  df <- data.frame(
    Document_Type = names(v_document_types),
    Count = sapply(v_document_types, function(x) ifelse(length(x) == 0, 0, x))
  )
  
  # Step 2: Filter Out Zero Count Types
  df <- df[df$Count > 0, ]
  
  # Step 3: Apply Label Mapping
  df$Document_Type <- LABEL_MAPPING[df$Document_Type]

  # Step 4: Generate the Pie Chart using ggplot2
  pie_chart <- ggplot(df, aes(x = "", y = Count, fill = Document_Type))
  pie_chart <- pie_chart + geom_bar(
    width = 1, 
    stat = "identity", 
    color = THEME_COLORS$Grayscale$Black, 
    size = 0.25
  )
  pie_chart <- pie_chart + coord_polar("y", start = 0)
  pie_chart <- pie_chart + labs(title = "Document Types Distribution")
  pie_chart <- pie_chart + theme_void()
  pie_chart <- pie_chart + theme(
    plot.title = element_text(
      size = 16, 
      face = "bold", 
      hjust = 0.5, 
      margin = margin(t = 10, b = 10), 
      color = THEME_COLORS$Text$Title
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = THEME_COLORS$Text$Body),
    legend.position = "right"
  )
  pie_chart <- pie_chart + scale_fill_manual(values = THEME_COLORS$Main)
  
  # Step 5: Save the Pie Chart
  save_plot(pie_chart, "M1_G2_DOCUMENT_TYPES_PIE_PLOT", width = 6, height = 4, dpi = 600)
  
  # Log success message
  message("[INFO] Pie chart for document types saved successfully.")
}

# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc3 :: Analyze and Plot Most Productive Authors
# Description: Validates data, calculates author productivity metrics, and generates a bar plot.
# ---------------------------------------------------------------------------- #
fn_m1_mtrc3_analyze_and_plot_most_prod_authors <- function(data) {
  
  # Step 1: Validate Data
  # Ensure the input data contains the required columns: "Authors" and "Articles".
  data <- validate_data(data, c("Authors", "Articles"))
  
  # Step 2: Calculate Metrics for Top Authors
  metrics <- calculate_metrics_top_authors(data)

  # Step 3: Generate Horizontal Bar Plot for Top Authors
  plot <- ggplot(metrics, aes(x = reorder(Authors, Articles), y = Articles))
  plot <- plot + geom_bar(stat = "identity", fill = THEME_COLORS$Main[1], color = "black", size = 0.3)
  plot <- plot + coord_flip()
  plot <- plot + labs(
    title = "Top 10 Most Productive Authors",
    x = "Authors",
    y = "Number of Articles"
  )
  plot <- plot + theme_minimal()
  plot <- plot + theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(color = THEME_COLORS$Grayscale$LightGray),
    panel.grid.minor = element_blank()
  )

  # Step 4: Save Metrics as JSON
  # Export the calculated metrics to a JSON file.
  save_json(metrics, "most_prod_authors.json")
  
  # Step 5: Save the Plot as PNG
  save_plot(plot, "M1_G3_MOST_PROD_AUTHORS_BAR_PLOT", width = 8, height = 6, dpi = 600)
  
  # Step 6: Log Success Message
  message("[INFO] Most Productive Authors analysis completed successfully.")
}

# ---------------------------------------------------------------------------- #
# Function: Generate Lorenz Curve for Author Contributions with Debug Logs
# ---------------------------------------------------------------------------- #
fn_m1_mtrc3_generate_lorenz_curve <- function(data, output_dir) {
  tryCatch({
    # Validate input data
    validate_data(data, c("Authors", "Articles"))

    # Check for NA in 'Authors' column
    if (any(is.na(data$Authors))) {
      warning("[DEBUG] The following rows have NA in 'Authors':")
      print(data[is.na(data$Authors), ])
    }

    # Check for NA or non-numeric values in 'Articles' column
    data$Articles <- suppressWarnings(as.numeric(data$Articles))
    if (any(is.na(data$Articles))) {
      warning("[DEBUG] The following rows have NA or non-numeric values in 'Articles':")
      print(data[is.na(data$Articles), ])
    }

    # Stop execution if invalid data exists
    if (any(is.na(data$Authors)) || any(is.na(data$Articles))) {
      stop("[ERROR] 'Authors' or 'Articles' column contains NA or non-numeric values. Please fix the data.")
    }

    # Ensure that Articles are sorted in descending order
    data <- data[order(-data$Articles), ]

    # Call the generic Lorenz curve generation function
    gini <- generate_lorenz_curve(
      data = data, 
      value_col = "Articles",
      entity_col = "Authors",
      plot_title = "Lorenz Curve: Author Contributions",
      x_label = "Cumulative Percentage of Articles",
      y_label = "Cumulative Percentage of Authors",
      file_name = "M1_G3_AUTHOR_CONTRIBUTIONS_LORENZ_PLOT",
      theme_colors = THEME_COLORS
    )

    # Log the Gini coefficient for reporting
    message("[INFO] Gini Coefficient for Author Contributions: ", round(gini, 3))
    message("[INFO] Lorenz Curve for Author Contributions generated and saved successfully.")
  }, error = function(e) {
    message("[ERROR] Failed to generate Lorenz Curve: ", e$message)
  })
}

# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Cited Papers
# ---------------------------------------------------------------------------- #
fn_m1_mtrc4_analyze_and_plot_most_cited_papers <- function(data) {
  tryCatch({
    # Step 1: Validate required columns
    colnames(data) <- make.unique(colnames(data)) # Ensure unique column names

    citation_col <- if ("Citations" %in% colnames(data)) {
      "Citations"
    } else if ("TC" %in% colnames(data)) {
      "TC"  # Total Citations (common abbreviation)
    } else {
      stop("[ERROR] Missing column for citations (e.g., 'Citations' or 'TC').")
    }

    if (!all(c("Paper", "PaperID") %in% colnames(data))) {
      stop("[ERROR] Missing 'Paper' or 'PaperID' column in the dataset.")
    }

    # Step 2: Clean and preprocess data
    data[[citation_col]] <- suppressWarnings(as.numeric(data[[citation_col]]))
    if ("TCperYear" %in% colnames(data)) {
      data$TCperYear <- suppressWarnings(as.numeric(data$TCperYear))
    }
    if (any(is.na(data[[citation_col]]))) {
      message("[INFO] Removing rows with invalid citation values.")
      data <- data[!is.na(data[[citation_col]]), ]
    }

    if (nrow(data) == 0) {
      stop("[ERROR] No valid rows in the dataset after cleaning citation values.")
    }

    # Step 3: Sort and select top 10 most cited papers
    top_cited_papers <- data[order(-data[[citation_col]]), ]
    top_cited_papers <- head(top_cited_papers, 10)

    # Step 4: Generate the dual-axis plot
    dual_plot <- generate_bar_plot_with_line(
      data = top_cited_papers,
      title = "Top 10 Most Cited Papers with TC per Year",
      x_label = "Papers",
      y_label = "Number of Citations",
      secondary_y_label = "Citations per Year",
      x_var = "PaperID",
      y_var = citation_col,
      line_var = "TCperYear" # Include the line for TCperYear
    )

    # Debug: Check the generated plot object
    if (!inherits(dual_plot, "ggplot")) {
      stop("[ERROR] Generated plot is not a valid ggplot object.")
    }

    # Step 5: Save JSON and plot
    save_json(top_cited_papers, "most_cited_papers.json")
    save_plot(
      dual_plot,
      "M1_G4_MOST_CITED_PAPERS_DUAL_PLOT",
      width = 8,
      height = 6,
      dpi = 600
    )

    # Log completion
    message("[INFO] Most Cited Papers analysis completed successfully.")
  }, error = function(e) {
    message("[ERROR] An error occurred in fn_m1_mtrc4_analyze_and_plot_most_cited_papers: ", e$message)
  })
}


# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Citations per Year
# ---------------------------------------------------------------------------- #
fn_m1_mtrc4_analyze_and_plot_citations_per_year <- function(data) {
  # Step 1: Validate required columns
  colnames(data) <- make.unique(colnames(data)) # Ensure unique column names

  # Check for required columns
  if (!("TCperYear" %in% colnames(data))) {
    message("[INFO] Available columns: ", paste(colnames(data), collapse = ", "))
    stop("[ERROR] 'TCperYear' column is missing in the dataset.")
  }
  if (!all(c("Paper", "PaperID") %in% colnames(data))) {
    message("[INFO] Available columns: ", paste(colnames(data), collapse = ", "))
    stop("[ERROR] 'Paper' or 'PaperID' column is missing in the dataset.")
  }

  # Step 2: Preprocess the 'TCperYear' column
  data$TCperYear <- suppressWarnings(as.numeric(data$TCperYear))
  if (any(is.na(data$TCperYear))) {
    num_invalid <- sum(is.na(data$TCperYear))
    message("[WARNING] Removing ", num_invalid, " rows with invalid 'Citations Per Year' values.")
    data <- data[!is.na(data$TCperYear), ]
  }

  # Ensure valid rows exist
  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning 'Citations Per Year' values.")
  }

  # Step 3: Sort and select top 10 papers by 'TCperYear'
  top_cited_papers <- data[order(-data$TCperYear), ]
  top_cited_papers <- head(top_cited_papers, 10)

  # Verify if there are enough papers to plot
  if (nrow(top_cited_papers) < 10) {
    warning("[WARNING] Less than 10 papers available for plotting. Adjusting to available rows.")
  }

  # Step 4: Plot the data using PaperID for the y-axis
  plot <- generate_bar_plot_vertical(
    data = top_cited_papers,
    title = "Top 10 Papers by Citations Per Year",
    x_label = "Citations Per Year",
    y_label = "Papers",
    x_var = "TCperYear",
    y_var = "PaperID"
  )

  # Step 5: Save JSON and plot
  save_json(top_cited_papers, "citations_per_year.json")
  save_plot(
    plot,
    "M1_G4_CITATIONS_PER_YEAR_BAR_PLOT",
    width = 8,
    height = 6,
    dpi = 600
  )

  # Log completion
  message("[INFO] Citations Per Year analysis completed successfully.")
}


# ---------------------------------------------------------------------------- #
# Function: Generate and Save Bubble Charts for Most Cited Papers (Refactored)
# ---------------------------------------------------------------------------- #
fn_m1_mtrc4_generate_bubble_chart <- function(data) {
  tryCatch({

    source('../../src/M1_Main_Information/__m1_bubble_paper_tc_chart.r')

    # ------------------------------------------- #
    # -- Plotting Settings Config --------------- #
    # ------------------------------------------- #
    dpi <- 600              # Set DPI
    text_size <- 4          # Text size in points
    base_dpi <- 300         # Reference DPI for scaling
    char_scale_factor <- 100  # Reference scale factor for 300 DPI at text size 4
    # ------------------------------------------- #



    data <- validate_and_prepare_data(data)
    
    # Plot Chart 1: Median-Based Quadrants
    message("[INFO] Generating bubble chart for Median-Based Quadrants...")
    region_data <- create_region_data(data)

    message("[INFO]  geom_median <- create_median_quadrant_layers(region_data, data)...")
    geom_median <- create_median_quadrant_layers(data = data, text_size = text_size, alpha = 0.1)

    bubble_chart <- generate_and_save_plot(
      df = data, 
      region_data = region_data,
      title = "Bubble Chart: Median-Based Quadrants", 
      custom_geom = geom_median
    )

    kmeans_plot <- generate_and_save_kmeans_plot(data, "Bubble Chart: K-Means Clustering")
        
    save_plot(
      plot = bubble_chart,
      filename_prefix = "M1_G4_BUBBLE_PAPERS_MEDIAN_QUADRANTS", 
      width = 8,
      height = 6,
      dpi = dpi
    )

    save_plot(
      plot = kmeans_plot,
      filename_prefix = "M1_G4_BUBBLE_PAPERS_K_MEANS", 
      width = 8,
      height = 6,
      dpi = dpi
    )



    # Log completion
    message("[INFO] Bubble chart with median-based quadrants generated successfully.")

  }, error = function(e) {
    message("[ERROR] An error occurred in fn_m1_mtrc4_generate_bubble_chart: ", e$message)
    stop(' ======================================================================================================================== ')
  })
}

# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc5 :: Analyze and Plot Most Productive Countries
# Description: Validates and preprocesses data, then generates multiple plots, 
# including bar plots, Lorenz curves, world maps, and treemaps for country-based analysis.
# ---------------------------------------------------------------------------- #
fn_m1_mtrc5_analyze_and_plot_most_prod_countries <- function(data) {
  
  # Step 1: Validate and Preprocess Data
  message("[INFO] Validating and preprocessing data for country analysis...")
  data <- preprocess_data(data, required_columns = c("Country", "Articles", "SCP", "MCP"))

  # Step 2: Generate Bar Plot for Top Productive Countries
  message("[INFO] Generating bar plot for the most productive countries...")
  top_countries <- get_top_countries(data, top_n = 15)
  
  bar_plot <- ggplot(top_countries, aes(x = reorder(Country, Articles), y = Articles))
  bar_plot <- bar_plot + geom_bar(stat = "identity", fill = THEME_COLORS$Main[1], color = "black", size = 0.3)
  bar_plot <- bar_plot + coord_flip()
  bar_plot <- bar_plot + labs(
    title = "Top 15 Most Productive Countries",
    x = "Country",
    y = "Number of Articles"
  )
  bar_plot <- bar_plot + theme_minimal() + theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = THEME_COLORS$Grayscale$LightGray)
  )

  # Save Bar Plot
  save_plot(bar_plot, "M1_G5_MOST_PROD_COUNTRIES_BAR_PLOT", width = 8, height = 6, dpi = 600)

  # Ensure SCP and MCP columns are numeric
  top_countries$SCP <- as.numeric(top_countries$SCP)
  top_countries$MCP <- as.numeric(top_countries$MCP)

  # Step 3: Generate Dual Bar Plot for SCP and MCP
  message("[INFO] Generating dual bar plot for SCP and MCP...")
  generate_dual_bar_plot_horizontal(
    data = top_countries,
    title = "Top Countries by SCP and MCP",
    x_label = "Countries",
    y_label = "N. of Documents",
    x_var = "Country",
    y_var_scp = "SCP",
    y_var_mcp = "MCP",
    file_name = "M1_G5_MOST_PROD_COUNTRIES_STACKED_BAR_PLOT"
  )

  # Step 4: Generate Lorenz Curve for Inequality Analysis
  message("[INFO] Generating Lorenz curve for inequality analysis...")
  generate_lorenz_curve(
    data = data,
    value_col = "Articles",
    entity_col = "Country",
    plot_title = "Lorenz Curve: Article Production by Countries",
    x_label = "Cumulative Percentage of Articles",
    y_label = "Cumulative Percentage of Countries",
    file_name = "M1_G5_MOST_PROD_COUNTRIES_LORENZ_CURVE",
    theme_colors = THEME_COLORS
  )

  # Step 5: Generate World Map for Article Distribution
  message("[INFO] Generating world map for article production...")
  tryCatch({
    generate_world_map(
      map_data = data,
      output_dir = "results/M1_Main_Information/figures",
      value_col = "Articles",
      map_title = "Global Distribution of Articles",
      file_name = "M1_G5_MOST_PROD_COUNTRIES_MAP",
      color_scheme = "greens"
    )
  }, error = function(e) {
    message("[ERROR] Failed to generate world map: ", e$message)
  })

  # Step 6: Generate Treemap for Article Distribution by Country
  message("[INFO] Generating treemap for article distribution...")
  data <- data %>%
    mutate(
      Country = as.character(Country),
      Freq = as.numeric(Freq),
      SCP = as.numeric(SCP),
      MCP = as.numeric(MCP),
      MCP_Ratio = as.numeric(MCP_Ratio)
    )
  generate_treemap(
    data = data,
    value_col = "Articles",
    label_col = "Country",
    title = "Article Distribution by Country",
    file_name = "M1_G5_MOST_PROD_COUNTRIES_TREEMAP"
  )

  # Step 7: Generate Additional Bar Plots for SCP and MCP
  message("[INFO] Generating bar plots for SCP and MCP...")
  
  # Bar Plot for SCP (Single Country Publications)
  bar_plot_scp <- ggplot(data, aes(x = reorder(Country, SCP), y = SCP))
  bar_plot_scp <- bar_plot_scp + geom_bar(stat = "identity", fill = THEME_COLORS$Main[2], color = "black", size = 0.3)
  bar_plot_scp <- bar_plot_scp + coord_flip()
  bar_plot_scp <- bar_plot_scp + labs(
    title = "Single Country Publications (SCP)",
    x = "Country",
    y = "Number of SCP Articles"
  )
  bar_plot_scp <- bar_plot_scp + theme_minimal()
  save_plot(bar_plot_scp, "M1_G5_MOST_PROD_COUNTRIES_SCP_BAR_PLOT", width = 8, height = 6, dpi = 600)

  # Bar Plot for MCP (Multiple Country Publications)
  bar_plot_mcp <- ggplot(data, aes(x = reorder(Country, MCP), y = MCP))
  bar_plot_mcp <- bar_plot_mcp + geom_bar(stat = "identity", fill = THEME_COLORS$Main[3], color = "black", size = 0.3)
  bar_plot_mcp <- bar_plot_mcp + coord_flip()
  bar_plot_mcp <- bar_plot_mcp + labs(
    title = "Multiple Country Publications (MCP)",
    x = "Country",
    y = "Number of MCP Articles"
  )
  bar_plot_mcp <- bar_plot_mcp + theme_minimal()
  save_plot(bar_plot_mcp, "M1_G5_MOST_PROD_COUNTRIES_MCP_BAR_PLOT", width = 8, height = 6, dpi = 600)

  # Step 8: Final Logging and Completion
  message("[INFO] Most productive countries analysis completed successfully.")
}







# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Total Citations Per Country
# ---------------------------------------------------------------------------- #

analyze_and_plot_tc_per_country <- function(data ) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Convert relevant columns to numeric
  data$`Total Citations` <- as.numeric(data$`Total Citations`)
  data$`Average Article Citations` <- as.numeric(data$`Average Article Citations`)

  # Sort by Total Citations and get the top 10
  top_countries <- data[order(-data$`Total Citations`), ]
  top_countries <- head(top_countries, 10)

  # Create bar plot
  p <- ggplot(top_countries, aes(x = reorder(Country, `Total Citations`), y = `Total Citations`)) +
    geom_bar(stat = "identity", fill = THEME_COLORS["Green"]) +
    coord_flip() +
    labs(
      title = "Top 10 Countries by Total Citations",
      x = "Country",
      y = "Total Citations"
    ) +
    ieee_theme

  # Save plot
  save_plot(p, "tc_per_country")
  save_json(top_countries, "tc_per_country.json")
}
fn_m1_mtrc6_analyze_and_plot_most_rel_sources <- function(data) {
  tryCatch({
    # Ensure unique column names
    colnames(data) <- make.unique(colnames(data))

    # Convert Articles to numeric
    data$Articles <- suppressWarnings(as.numeric(data$Articles))

    # Log data structure
    message("[DEBUG] Data structure before processing:")
    print(head(data))
    str(data)

    # Validate Sources and Articles columns
    if (!"Sources" %in% colnames(data)) {
      stop("[ERROR] 'Sources' column is missing in the data.")
    }
    if (!"Articles" %in% colnames(data)) {
      stop("[ERROR] 'Articles' column is missing in the data.")
    }
    if (any(is.na(data$Articles))) {
      stop("[ERROR] 'Articles' column contains non-numeric or NA values.")
    }

    # Sort by Articles and get the top 10
    top_sources <- data[order(-data$Articles), ]
    top_sources <- head(top_sources, 10)

    # Log top sources
    message("[DEBUG] Top 10 sources:")
    print(top_sources)

    # Define colors using THEME_COLORS object
    if (!"Main" %in% names(THEME_COLORS)) {
      stop("[ERROR] 'Main' colors are not defined in THEME_COLORS.")
    }
    fill_color <- THEME_COLORS$Main[1]

    # Determine the maximum x-axis limit
    max_articles <- max(top_sources$Articles, na.rm = TRUE)
    x_max <- max_articles * 2

    # Create bar plot
    p <- ggplot(top_sources, aes(x = reorder(Sources, Articles), y = Articles)) +
      geom_bar(stat = "identity", fill = fill_color, color = "black", size = 0.5) + # Thin black border
      coord_flip() +
      scale_y_continuous(
        limits = c(0, x_max),
        breaks = seq(0, x_max, by = max_articles / 5)
      ) +
      labs(
        title = "Top 10 Most Relevant Sources",
        subtitle = "A quantitative analysis of article distribution by source",
        x = "Source",
        y = "Number of Articles"
      ) +
      ieee_theme +
      theme(
        text = element_text(size = 10), # Adjust text size for readability
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
        plot.margin = margin(10, 10, 20, 10), # Add space around the plot
        legend.position = "none" # Remove unnecessary legend
      )

    # Log plot object
    if (!inherits(p, "ggplot")) {
      stop("[ERROR] Plot object is not a valid ggplot object.")
    } else {
      message("[DEBUG] Bar plot object created successfully.")
    }

    # Save bar plot
    save_plot(p, "M1_G6_MOST_RELEVANT_SOURCES", width = 9, height = 3.5, dpi = 600)
    save_json(top_sources, "M1_G6_MOST_RELEVANT_SOURCES")

    # Generate Lorenz curve
    message("[DEBUG] Generating Lorenz curve for most relevant sources...")
    gini_coefficient <- generate_lorenz_curve(
      data = top_sources,
      value_col = "Articles",
      entity_col = "Sources",
      plot_title = "Lorenz Curve: Article Distribution by Source",
      x_label = "Cumulative Percentage of Articles",
      y_label = "Cumulative Percentage of Sources",
      file_name = "M1_G6_MOST_RELEVANT_SOURCES_LORENZT"
    )

    message("[INFO] Lorenz curve generated successfully with Gini coefficient: ", round(gini_coefficient, 3))
    message("[INFO] Analysis and plotting of most relevant sources completed successfully.")

  }, error = function(e) {
    message("[ERROR] Failed to analyze and plot most relevant sources: ", e$message)
  })
}


# ---------------------------------------------------------------------------- #
# Function: Analyze and Visualize Bradford's Law
# ---------------------------------------------------------------------------- #
analyze_and_plot_bradford_law <- function(data, output_dir) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Create bar plot
  p <- ggplot(data, aes(x = reorder(SO, -Freq), y = Freq, fill = Zone)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = "Bradford's Law Distribution",
      x = "Source",
      y = "Frequency",
      fill = "Zone"
    ) +
    ieee_theme

  # Save plot
  save_plot(p, "bradford_law")
  save_json(data, "bradford_law.json")
}




# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc5 :: Analyze and Plot Total Citations Per Country
# Description: Validates and preprocesses data, then generates multiple plots
# to visualize total and average citations by country.
# ---------------------------------------------------------------------------- #
fn_m1_mtrc5_analyze_and_plot_tc_per_country <- function(data) {
  
  # Step 1: Validate and Preprocess Data
  message("[INFO] Validating and preprocessing data for citation analysis...")
  data <- preprocess_data(data, required_columns = c("Country", "Total Citations", "Average Article Citations"))

  data <- data %>%
    mutate(
      `Total Citations` = suppressWarnings(as.numeric(`Total Citations`)),
      `Average Article Citations` = suppressWarnings(as.numeric(`Average Article Citations`))
    )

  # Step 2: Generate Bar Plot for Top Countries by Total Citations
  message("[INFO] Generating bar plot for the countries with the most citations...")
  top_countries <- get_top_countries(data, column = "Total Citations", top_n = 15)

  bar_plot <- ggplot(top_countries, aes(x = reorder(Country, `Total Citations`), y = `Total Citations`))
  bar_plot <- bar_plot + geom_bar(stat = "identity", fill = THEME_COLORS$Main[1], color = "black", size = 0.3)
  bar_plot <- bar_plot + coord_flip()
  bar_plot <- bar_plot + labs(
    title = "Top 15 Countries by Total Citations",
    x = "Country",
    y = "Number of Citations"
  )
  bar_plot <- bar_plot + theme_minimal() + theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = THEME_COLORS$Grayscale$LightGray)
  )

  # Save Bar Plot
  save_plot(bar_plot, "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_BAR_PLOT", width = 8, height = 6, dpi = 600)

  # Step 3: Generate Lorenz Curve for Citation Inequality Analysis
  message("[INFO] Generating Lorenz curve for citation inequality analysis...")
  generate_lorenz_curve(
    data = data,
    value_col = "Total Citations",
    entity_col = "Country",
    plot_title = "Lorenz Curve: Citation Distribution by Countries",
    x_label = "Cumulative Percentage of Citations",
    y_label = "Cumulative Percentage of Countries",
    file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_LORENZ_CURVE",
    theme_colors = THEME_COLORS
  )

  # Step 4: Generate World Map for Citation Distribution
  message("[INFO] Generating world map for citation distribution...")
  tryCatch({
    generate_world_map(
      map_data = data,
      output_dir = "results/M1_Main_Information/figures",
      value_col = "Total Citations",
      map_title = "Global Distribution of Citations",
      file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_MAP",
      color_scheme = "blues"
    )
  }, error = function(e) {
    message("[ERROR] Failed to generate world map: ", e$message)
  })

  # Step 5: Generate Treemap for Citation Distribution
  message("[INFO] Generating treemap for citation distribution...")
  generate_treemap(
    data = data,
    value_col = "Total Citations",
    label_col = "Country",
    title = "Citation Distribution by Country",
    file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_TREEMAP"
  )

  # Step 6: Generate Stacked Bar Plot for Most Cited Countries
  message("[INFO] Generating stacked bar plot for the most cited countries...")
  generate_bar_stacked(
    data = data,
    title = "Most Cited Countries",
    x_label = "Number of Citations",
    y_label = "Countries",
    file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_STACKED_BAR_PLOT",
    categorical_var_col = "Country",
    col_a = "Total Citations",
    col_b = "Average Article Citations",
    col_a_label = "Total Citations",
    col_b_label = "Average Citations"
  )

  # Step 7: Generate Bar Plot for Average Citations per Article
  message("[INFO] Generating bar plot for average citations per article...")
  avg_citations_plot <- ggplot(data, aes(x = reorder(Country, `Average Article Citations`), y = `Average Article Citations`))
  avg_citations_plot <- avg_citations_plot + geom_bar(stat = "identity", fill = THEME_COLORS$Main[2], color = "black", size = 0.3)
  avg_citations_plot <- avg_citations_plot + coord_flip()
  avg_citations_plot <- avg_citations_plot + labs(
    title = "Average Citations per Article",
    x = "Country",
    y = "Average Citations"
  )
  avg_citations_plot <- avg_citations_plot + theme_minimal() + theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

  # Save Average Citations Plot
  save_plot(avg_citations_plot, "M1_G5_TOP_COUNTRIES_BY_AVERAGE_CITATIONS_BAR_PLOT", width = 8, height = 6, dpi = 600)

  # Final Log Message
  message("[INFO] Citation analysis (bar plot, Lorenz curve, world map, stacked bar plot) completed successfully.")
}



fn_m1_mtrc5_countries_tp_vs_tc_plot_bubble_chart <- function(most_prod_countries, tc_per_country) {
  tryCatch({
    # Combine data
    combined_data <- merge(most_prod_countries, tc_per_country, by = "Country") %>%
      dplyr::mutate(
        `Total Citations` = as.numeric(`Total Citations`), # Convert to numeric
        Articles = as.numeric(Articles),                 # Convert to numeric
        AverageCitations = `Total Citations` / Articles  # Compute average citations
      )

    # Initial linear fit
    initial_fit <- lm(`Total Citations` ~ Articles, data = combined_data)
    residuals <- abs(resid(initial_fit))
    threshold <- mean(residuals) + 2 * sd(residuals)  # Threshold to identify outliers

    # Exclude outliers
    non_outlier_data <- combined_data[residuals <= threshold, ]

    # Fit linear regression on non-outlier data
    refined_fit <- lm(`Total Citations` ~ Articles, data = non_outlier_data)
    slope <- coef(refined_fit)["Articles"]
    intercept <- coef(refined_fit)["(Intercept)"]

    # Adjust intercept for upper and lower bounds (+50% and -50%)
    upper_intercept <- intercept * 1.5
    lower_intercept <- intercept * 0.5

    # Add y-values for the upper and lower bounds and classify points
    combined_data <- combined_data %>%
      dplyr::mutate(
        UpperBound = slope * Articles + upper_intercept,
        LowerBound = slope * Articles + lower_intercept,
        PointColor = case_when(
          `Total Citations` > UpperBound ~ "High TC (Green)",  # Green for above upper bound
          `Total Citations` < LowerBound ~ "Low TC (Red)",     # Red for below lower bound
          TRUE ~ "Average TC (Blue)"                           # Blue for in-between
        )
      )

    # Calculate midpoints for the dashed lines
    mid_x <- median(combined_data$Articles, na.rm = TRUE)
    mid_y <- median(combined_data$`Total Citations`, na.rm = TRUE)

    # Determine annotation position dynamically
    max_x <- max(combined_data$Articles, na.rm = TRUE)
    max_y <- max(combined_data$`Total Citations`, na.rm = TRUE)
    annotate_x <- max_x * 0.5
    annotate_y <- max_y * 0.5

    # Create bubble chart
    bubble_chart <- ggplot(combined_data, aes(
      x = Articles,
      y = `Total Citations`,
      size = AverageCitations,
      label = Country,
      color = PointColor
    )) +
      geom_point(alpha = 1) + # Use PointColor for bubbles
      geom_text_repel(size = 4, fontface = "bold") +
      geom_vline(xintercept = mid_x, linetype = "longdash", color = THEME_COLORS$Grayscale$MediumGray, linewidth = 0.8) +
      geom_hline(yintercept = mid_y, linetype = "longdash", color = THEME_COLORS$Grayscale$MediumGray, linewidth = 0.8) +
      geom_ribbon(
        aes(x = Articles, ymin = LowerBound, ymax = UpperBound),
        fill = THEME_COLORS$Main[2], alpha = 0.5, inherit.aes = FALSE
      ) +
      geom_abline(
        slope = slope, intercept = intercept,
        linetype = "dashed",  # Trend line
        color = THEME_COLORS$Main[4],
        linewidth = 0.85
      ) +
      annotate(
        "text", x = annotate_x, y = annotate_y,
        label = paste0("Trend line: y = ", round(intercept, 2), " + ", round(slope, 2), "x"),
        color = THEME_COLORS$Text$Caption, size = 6, hjust = 0
      ) +
      coord_cartesian(clip = "off") + # Ensure no clipping
      labs(
        title = "Productivity vs. Impact by Country",
        x = "Number of Articles",
        y = "Total Citations",
        color = "Citation Levels"  # Legend title for color
      ) +
      scale_color_manual(
        values = c(
          "High TC (Green)" = THEME_COLORS$Main[5],
          "Low TC (Red)" = THEME_COLORS$Main[3],
          "Average TC (Blue)" = THEME_COLORS$Main[1]
        )
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(fill = "white", color = NA), # White background
        plot.background = element_rect(fill = "transparent", color = NA), # Transparent plot background
        legend.position = "right",
        legend.box.background = element_rect(fill = "white", color = THEME_COLORS$Grayscale$DarkGray), # White background for legend
        panel.grid.major = element_line(color = THEME_COLORS$Grayscale$LightGray),  # Major gridlines in light gray
        panel.grid.minor = element_line(color = THEME_COLORS$Grayscale$VeryLightGray)   # Minor gridlines in very light gray
      )

    # Save plot
    save_plot(bubble_chart, "M1_G5_TOP_COUNTRIES_Productivity_vs_Impact_Bubble_Chart_With_Color_Legend", width = 12, height = 8, dpi = 400)

    # Save insights
    insight <- paste(
      "Dashed lines divide the chart into quadrants, highlighting countries with high/low productivity and high/low impact.",
      "The trend line (dashed) represents the refined line of best fit (y = ", 
      round(intercept, 2), " + ", round(slope, 2), "x).",
      "Points are colored based on citation levels: green (above upper bound), red (below lower bound), and blue (average)."
    )
    write(insight, file = "results/Productivity_vs_Impact_Color_Legend_Insights.txt")

    return(bubble_chart)
  }, error = function(e) {
    message("[ERROR] Failed to create bubble chart: ", e$message)
    return(NULL)
  })
}




analyze_bradford_law <- function(data, output_dir = "results") {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Prepare Data
  data <- data %>%
    mutate(
      Percent = Freq / sum(Freq) * 100,
      cumPercent = cumsum(Percent)
    )
  
  # Calculate Gini Coefficient
  gini_coeff <- {
    freq_sorted <- sort(data$Freq, decreasing = TRUE)
    lorenz_curve <- cumsum(rep(1, length(freq_sorted))) / length(freq_sorted)
    cumulative_freq <- cumsum(freq_sorted) / sum(freq_sorted)
    1 - 2 * sum(lorenz_curve * cumulative_freq) / sum(cumulative_freq)
  }
  
  # Metrics to Save
  metrics <- list(
    total_sources = nrow(data),
    total_frequency = sum(data$Freq),
    gini_coefficient = round(gini_coeff, 3),
    top_5_sources = data %>% arrange(desc(Freq)) %>% head(5) %>% select(SO, Freq)
  )
  
  # Save Metrics
  save_json(metrics, file.path(output_dir, "bradford_metrics.json"))
  
  # Generate Plots
  plot_bradford_zones <- ggplot(data, aes(x = Rank, y = Freq, fill = Zone)) +
    geom_bar(stat = "identity") +
    labs(title = "Bradford's Law Zones", x = "Rank", y = "Frequency", fill = "Zone") +
    ieee_theme
  
  plot_cumulative_frequency <- ggplot(data, aes(x = Rank, y = cumPercent)) +
    geom_line(color = "blue") +
    labs(title = "Cumulative Frequency Plot", x = "Rank", y = "Cumulative Frequency (%)") +
    ieee_theme
  
  # Save Plots
  save_plot(plot_bradford_zones, file.path(output_dir, "bradford_zones"))
  save_plot(plot_cumulative_frequency, file.path(output_dir, "cumulative_frequency"))
  
  message("[INFO] Analysis and plots for Bradford's Law completed.")
}