# ---------------------------------------------------------------------------- #
# -- M2 Main Information Analysis ------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Load necessary libraries
library(ggwordcloud)
library(ggplot2)
library(jsonlite)
library(bibliometrix)
library(wordcloud2)
library(htmlwidgets)


source('../../src/M1_Main_Information/_helpers.r')
source('../../src/M1_Main_Information/_plots.r')

# ---------------------------------------------------------------------------- #
# Function: Main Information Analysis
# ---------------------------------------------------------------------------- #
fn_m1_main_information <- function(bib_data) {
  res1 <- biblioAnalysis(bib_data, sep = ";")
  s1 <- summary(res1, pause = FALSE, verbose = FALSE)

  # Extract and clean additional summary information
  summary_df <- clean_whitespace(s1$MainInformationDF)
  most_prod_authors <- clean_whitespace(s1$MostProdAuthors)
  annual_production <- clean_whitespace(s1$AnnualProduction)
  most_cited_papers <- clean_whitespace(s1$MostCitedPapers)
  most_prod_countries <- clean_whitespace(s1$MostProdCountries)
  tc_per_countries <- clean_whitespace(s1$TCperCountries)
  most_rel_sources <- clean_whitespace(s1$MostRelSources)
  most_rel_keywords <- clean_whitespace(s1$MostRelKeywords)

  # Compute Bradford's Law
  bradford_law <- bradford(bib_data)$table

  # Organize main information into a structured list
  main_information_data <- list(
    main_information = extract_main_information(summary_df),
    most_cited_papers = most_cited_papers,
    most_prod_countries = most_prod_countries,
    tc_per_countries = tc_per_countries,
    most_rel_sources = most_rel_sources,
    most_rel_keywords = most_rel_keywords,
    most_prod_authors = most_prod_authors,
    author_prod_over_time = annual_production,
    bradford_law = bradford_law
  )

  # Save document type pie chart
  fn_save_m1_articles_pie(main_information_data$main_information$document_types)

  return(main_information_data)
}

# ---------------------------------------------------------------------------- #
# Function: Save Document Types Pie Chart
# ---------------------------------------------------------------------------- #
fn_save_m1_articles_pie <- function(v_document_types) {
  df <- data.frame(
    Document_Type = names(v_document_types),
    Count = sapply(v_document_types, function(x) ifelse(length(x) == 0, 0, x))
  )

  df <- df[df$Count > 0, ]
  df$Document_Type <- LABEL_MAPPING[df$Document_Type]

  # Call the generic pie chart plotting function
  pie_chart <- plot_pie_chart(
    df = df,
    title = "Document Types Distribution",
    fill_var = "Document_Type",
    count_var = "Count"
  )

  save_plot(pie_chart, "M1_DOCUMENT_TYPES_PIE_PLOT", width = 6, height = 4, dpi = 600)
}

# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Productive Authors
# ---------------------------------------------------------------------------- #
analyze_and_plot_most_prod_authors <- function(data, output_dir) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Check for required columns
  if (!("Authors" %in% colnames(data))) {
    stop("[ERROR] The 'Authors' column is missing in the dataset.")
  }

  if (!("Articles" %in% colnames(data))) {
    stop("[ERROR] The 'Articles' column is missing in the dataset.")
  }

  # Convert 'Articles' to numeric
  data$Articles <- suppressWarnings(as.numeric(data$Articles))
  if (any(is.na(data$Articles))) {
    stop("[ERROR] 'Articles' column contains non-numeric or missing values.")
  }

  # Sort by Articles in descending order and select the top 10
  top_authors <- data[order(-data$Articles), ]
  top_authors <- head(top_authors, 10)

  # Create bar plot
  p <- ggplot(top_authors, aes(x = reorder(Authors, Articles), y = Articles)) +
    geom_bar(stat = "identity", fill = THEME_COLORS["Green"]) +
    coord_flip() +
    labs(
      title = "Top 10 Most Productive Authors",
      x = "Authors",
      y = "Number of Articles"
    ) +
    ieee_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )

  # Save results and plot
  save_json(top_authors, file.path(output_dir, "jsons", "most_prod_authors.json"))
  save_plot(p, "M1_MOST_PROD_AUTHORS_BAR_PLOT")
  message("[INFO] Most Productive Authors analysis completed successfully.")
}


# ---------------------------------------------------------------------------- #
# Function: Generate Lorenz Curve for Author Contributions
# ---------------------------------------------------------------------------- #
generate_lorenz_curve <- function(data, output_path) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Validate input data
  if (!all(c("Authors", "Articles") %in% colnames(data))) {
    stop("[ERROR] Missing required columns: 'Authors' and 'Articles'.")
  }

  # Convert Articles column to numeric
  data$Articles <- suppressWarnings(as.numeric(data$Articles))
  if (any(is.na(data$Articles))) {
    stop("[ERROR] 'Articles' column contains non-numeric or missing values.")
  }

  # Sort data by Articles in descending order
  data <- data[order(-data$Articles), ]

  # Calculate cumulative percentages for Lorenz Curve
  cumulative_authors <- cumsum(rep(1, nrow(data))) / nrow(data)
  cumulative_articles <- cumsum(data$Articles) / sum(data$Articles)

  # Calculate the Gini coefficient
  area_trapezoids <- (cumulative_authors[-1] + cumulative_authors[-length(cumulative_authors)]) * diff(cumulative_articles) / 2
  area_under_curve <- sum(area_trapezoids)
  gini <- 1 - 2 * area_under_curve

  # Prepare data for plotting (ensure only necessary columns are included)
  plot_data <- data.frame(
    cumulative_authors = c(0, cumulative_authors, 1),
    cumulative_articles = c(0, cumulative_articles, 1)
  )

  # Define symmetrical axis limits
  axis_limits <- c(0, 1)

  # Define plot labels and annotations
  plot_label_titles <- labs(
    title = "Lorenz Curve: Author Contributions",
    x = "Cumulative Percentage of Articles",
    y = "Cumulative Percentage of Authors"
  )
  plot_label_annotation <- annotate(
    "text", x = 0.2, y = 0.8, label = paste0("Gini Coefficient: ", round(gini, 3)),
    color = "black", size = 4, hjust = 0
  )

  # Create the Lorenz Curve plot
  p <- ggplot(plot_data, aes(x = cumulative_articles, y = cumulative_authors))
  p <- p + geom_line(color = THEME_COLORS["Blue"], linewidth = 1)
  p <- p + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = THEME_COLORS["Darkgrey"])
  p <- p + coord_fixed(ratio = 1)
  p <- p + xlim(axis_limits)
  p <- p + ylim(axis_limits)
  p <- p + plot_label_titles
  p <- p + plot_label_annotation
  p <- p + ieee_theme +
    theme(
      plot.title = element_text(size = 14, margin = margin(b = 15, t = 15)),
      plot.background = element_rect(fill = "transparent", color = NA),  # Transparent plot background
      panel.background = element_rect(fill = "white", color = NA),      # White plot area
      axis.title.x = element_text(size = 10, margin = margin(t = 15, b = 5)), # X-axis title adjustments
      axis.title.y = element_text(size = 10, margin = margin(r = 15, l = 0)),  # Y-axis title adjustments
      axis.text = element_text(size = 8)                       
    )

  # Save the plot
  save_plot(p, "M1_MOST_PROD_AUTHORS_LORENZ_PLOT", width = 5, height = 5, dpi=600,  aspect_ratio = 1)
  message("[INFO] Lorenz Curve generated and saved successfully.")
}




# ---------------------------------------------------------------------------- #
# Function: Generate Most Relevant Keywords Wordcloud2
# ---------------------------------------------------------------------------- #
fn_most_rel_keywords_wordcloud2 <- function(most_rel_keywords) {
  if (is.null(most_rel_keywords) || nrow(most_rel_keywords) == 0) {
    stop("[ERROR] The input `most_rel_keywords` is NULL or empty.")
  }

  # Format column names
  colnames(most_rel_keywords)[1] <- "Author Keywords (DE)"
  colnames(most_rel_keywords)[2] <- "Article-Author-Keywords"

  # Convert counts to numeric
  most_rel_keywords$`Article-Author-Keywords` <- as.numeric(most_rel_keywords$`Article-Author-Keywords`)

  # Prepare data for wordcloud2
  wordcloud_data <- most_rel_keywords %>%
    select(`Author Keywords (DE)`, `Article-Author-Keywords`) %>%
    rename(word = `Author Keywords (DE)`, freq = `Article-Author-Keywords`)

  # Create the word cloud
  library(wordcloud2)
  wordcloud <- wordcloud2(wordcloud_data, size = 1, shape = "circle")

  # Save the word cloud as HTML
  output_html <- file.path("results/M1_Main_Information/figures", "M1_Most_Rel_Keywords_Wordcloud.html")
  htmlwidgets::saveWidget(wordcloud, output_html, selfcontained = FALSE)
  message("[INFO] Wordcloud saved at: ", output_html)
}

 
# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Cited Papers
# ---------------------------------------------------------------------------- #
analyze_and_plot_most_cited_papers <- function(data, output_dir) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Check for alternative column names for Citations
  citation_col <- if ("Citations" %in% colnames(data)) {
    "Citations"
  } else if ("TC" %in% colnames(data)) {
    "TC"  # Common abbreviation for Total Citations
  } else {
    stop("[ERROR] No column for Citations (e.g., 'Citations' or 'TC') in the dataset.")
  }

  if (!("Paper" %in% colnames(data))) {
    stop("[ERROR] The 'Paper' column is missing in the dataset.")
  }

  # Convert the citation column to numeric
  data[[citation_col]] <- suppressWarnings(as.numeric(data[[citation_col]]))
  if (any(is.na(data[[citation_col]]))) {
    message("[WARNING] Removing rows with invalid citation values.")
    data <- data[!is.na(data[[citation_col]]), ]
  }

  # Ensure there are valid rows remaining
  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning citation values.")
  }

  # Sort by citations and get the top 10
  top_cited_papers <- data[order(-data[[citation_col]]), ]
  top_cited_papers <- head(top_cited_papers, 10)

  # Create the plot
  p <- ggplot(top_cited_papers, aes(x = reorder(Paper, data[[citation_col]]), y = data[[citation_col]])) +
    geom_bar(stat = "identity", fill = THEME_COLORS["Orange"]) +
    coord_flip() +
    labs(
      title = "Top 10 Most Cited Papers",
      x = "Papers",
      y = "Number of Citations"
    ) +
    ieee_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Save JSON and plot
  save_json(top_cited_papers, file.path(output_dir, "jsons", "most_cited_papers.json"))
  save_plot(p, "most_cited_papers")
  message("[INFO] Most Cited Papers analysis completed successfully.")
}







# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Citations per Year
# ---------------------------------------------------------------------------- #
analyze_and_plot_citations_per_year <- function(data, output_dir) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Check for the appropriate column for CitationsPerYear
  citation_col <- if ("TCperYear" %in% colnames(data)) {
    "TCperYear"
  } else {
    message("[INFO] Available columns: ", paste(colnames(data), collapse = ", "))
    stop("[ERROR] No column for CitationsPerYear (e.g., 'TCperYear') in the dataset.")
  }

  # Ensure the 'Paper' column exists
  if (!("Paper" %in% colnames(data))) {
    message("[INFO] Available columns: ", paste(colnames(data), collapse = ", "))
    stop("[ERROR] The 'Paper' column is missing in the dataset.")
  }

  # Convert the citation column to numeric
  data[[citation_col]] <- suppressWarnings(as.numeric(data[[citation_col]]))
  if (any(is.na(data[[citation_col]]))) {
    num_invalid <- sum(is.na(data[[citation_col]]))
    message("[WARNING] Removing ", num_invalid, " rows with invalid citation per year values.")
    data <- data[!is.na(data[[citation_col]]), ]
  }

  # Ensure there are valid rows remaining
  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning citation per year values.")
  }

  # Sort by citations per year and get the top 10
  top_cited_papers <- data[order(-data[[citation_col]]), ]
  top_cited_papers <- head(top_cited_papers, 10)

  # Verify if there are enough papers to plot
  if (nrow(top_cited_papers) < 10) {
    warning("[WARNING] Less than 10 papers available for plotting. Adjusting to available rows.")
  }

  # Create the plot
  p <- ggplot(top_cited_papers, aes(x = reorder(Paper, -data[[citation_col]]), y = data[[citation_col]])) +
    geom_bar(stat = "identity", fill = THEME_COLORS["Green"]) +
    coord_flip() +
    labs(
      title = "Top 10 Papers by Citations Per Year",
      x = "Papers",
      y = "Citations Per Year"
    ) +
    ieee_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Save JSON and plot
  save_json(top_cited_papers, file.path(output_dir, "jsons", "citations_per_year.json"))
  save_plot(p, "citations_per_year")
  message("[INFO] Citations Per Year analysis completed successfully.")
}




# ---------------------------------------------------------------------------- #
# Function: Generate Bubble Chart for Most Cited Papers
# ---------------------------------------------------------------------------- #

generate_bubble_chart_analysis <- function(data, output_path) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Validate the required columns
  citation_col <- "TC"  # Total Citations
  per_year_col <- "TCperYear"  # Citations per Year
  normalized_col <- "NTC"  # Normalized Citations
  paper_col <- "Paper"  # Paper Titles

  # Ensure necessary columns exist
  missing_cols <- setdiff(c(citation_col, per_year_col, normalized_col, paper_col), colnames(data))
  if (length(missing_cols) > 0) {
    stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Convert relevant columns to numeric
  data[[citation_col]] <- suppressWarnings(as.numeric(data[[citation_col]]))
  data[[per_year_col]] <- suppressWarnings(as.numeric(data[[per_year_col]]))
  data[[normalized_col]] <- suppressWarnings(as.numeric(data[[normalized_col]]))

  # Remove rows with invalid or missing values
  data <- data[complete.cases(data[c(citation_col, per_year_col, normalized_col)]), ]

  # Ensure there are valid rows remaining
  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning values.")
  }

  # Sort by Citations and select the top 10 papers
  top_papers <- head(data[order(-data[[citation_col]]), ], 10)

  # Generate the bubble chart
  p <- ggplot(top_papers, aes(
    x = .data[[per_year_col]],
    y = .data[[citation_col]],
    size = .data[[normalized_col]],
    label = .data[[paper_col]]
  )) +
    geom_point(alpha = 0.7, color = THEME_COLORS["DarkBlue"]) +
    geom_text(hjust = 0.5, vjust = -0.5, size = 3) +
    labs(
      title = "Bubble Chart: Most Cited Papers",
      x = "Citations per Year",
      y = "Total Citations",
      size = "Normalized Citations"
    ) +
    ieee_theme

  # Save the plot
  save_plot(p, "most_cited_papers_bubble_chart")

  # Generate the analysis report
# Generate a dynamic report based on the bubble chart analysis
report <- paste0(
  "------------------------------------------------------------\n",
  "Automated Bubble Chart Analysis Report\n",
  "------------------------------------------------------------\n\n",
  "### Summary\n",
  "A total of ", nrow(top_papers), " papers were analyzed to identify the most influential, emerging, and impactful research contributions in the dataset.\n\n",
  "---\n\n",
  "### Highlights\n\n",
  "**1. Top-Cited Paper:**\n",
  "   - Title: '", top_papers[[paper_col]][1], "'\n",
  "   - Total Citations: ", top_papers[[citation_col]][1], "\n",
  "   - Annual Citation Rate: ", top_papers[[per_year_col]][1], " citations per year\n",
  "   - Insights: This paper has the highest total citations, making it a cornerstone reference in its field.\n\n",
  "**2. Emerging Paper:**\n",
  {
    # Identify the paper with the highest annual citation rate
    emerging_paper <- top_papers[which.max(top_papers[[per_year_col]]), ]
    paste0(
      "   - Title: '", emerging_paper[[paper_col]], "'\n",
      "   - Annual Citation Rate: ", emerging_paper[[per_year_col]], " citations per year\n",
      "   - Insights: This paper shows the highest annual citation rate in the dataset. Its recent publication date likely contributes to its growing visibility.\n\n"
    )
  },
  "**3. Sustained Impact Paper:**\n",
  {
    # Identify the paper with the highest normalized citations
    sustained_impact_paper <- top_papers[which.max(top_papers[[normalized_col]]), ]
    paste0(
      "   - Title: '", sustained_impact_paper[[paper_col]], "'\n",
      "   - Total Citations: ", sustained_impact_paper[[citation_col]], "\n",
      "   - Annual Citation Rate: ", sustained_impact_paper[[per_year_col]], " citations per year\n",
      "   - Insights: This paper combines consistent relevance and high impact, making it a sustained influence in its field.\n\n"
    )
  },
  "---\n\n",
  "### Observations\n",
  {
    # Generate observations dynamically based on the data
    observations <- list()
    if (any(top_papers[[per_year_col]] == 0)) {
      observations <- c(observations, "Some papers have a zero annual citation rate, which might indicate limited recent impact or data inaccuracies.")
    }
    if (any(top_papers[[normalized_col]] > 20)) {
      observations <- c(observations, "Several papers have high normalized citation scores, highlighting their consistent and widespread influence.")
    }
    if (length(observations) == 0) {
      observations <- c(observations, "Most papers show balanced citation metrics, indicating a mix of recent and long-term impact.")
    }
    paste0(
      paste(seq_along(observations), ". ", observations, collapse = "\n"),
      "\n\n"
    )
  },
  "### Recommendations\n",
  {
    # Generate recommendations dynamically based on the data
    recommendations <- list()
    if (any(top_papers[[per_year_col]] > 50)) {
      recommendations <- c(recommendations, "Highlight emerging papers with high annual citation rates in academic reviews and proposals.")
    }
    if (any(top_papers[[normalized_col]] > 25)) {
      recommendations <- c(recommendations, "Investigate papers with high normalized scores for interdisciplinary insights.")
    }
    if (length(recommendations) == 0) {
      recommendations <- c(recommendations, "Ensure the dataset is comprehensive to capture emerging and impactful papers.")
    }
    paste0(
      paste(seq_along(recommendations), ". ", recommendations, collapse = "\n"),
      "\n\n"
    )
  },
  "------------------------------------------------------------\n"
)

  # Save the report as a .txt file
  report_path <- file.path(output_path, "bubble_chart_analysis_report.txt")
  writeLines(report, report_path)
  message("[INFO] Analysis report saved at: ", report_path)

  message("[INFO] Bubble chart and analysis report generated successfully.")
}

generate_bubble_chart <- function(data, output_path) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Debugging information
 

  # Validate the required columns
  citation_col <- "TC"  # Total Citations
  per_year_col <- "TCperYear"  # Citations per Year
  normalized_col <- "NTC"  # Normalized Citations
  paper_col <- "Paper"  # Paper Titles

  # Ensure necessary columns exist
  missing_cols <- setdiff(c(citation_col, per_year_col, normalized_col, paper_col), colnames(data))
  if (length(missing_cols) > 0) {
    stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Convert relevant columns to numeric
  data[[citation_col]] <- suppressWarnings(as.numeric(data[[citation_col]]))
  data[[per_year_col]] <- suppressWarnings(as.numeric(data[[per_year_col]]))
  data[[normalized_col]] <- suppressWarnings(as.numeric(data[[normalized_col]]))

  # Remove rows with invalid or missing values
  data <- data[complete.cases(data[c(citation_col, per_year_col, normalized_col)]), ]

  # Ensure there are valid rows remaining
  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning values.")
  }

  # Sort by Citations and select the top 10 papers
  top_papers <- head(data[order(-data[[citation_col]]), ], 10)

  # Generate the bubble chart
  p <- ggplot(top_papers, aes(
    x = .data[[per_year_col]],
    y = .data[[citation_col]],
    size = .data[[normalized_col]],
    label = .data[[paper_col]]
  )) +
    geom_point(alpha = 0.7, color = THEME_COLORS["DarkBlue"]) +
    geom_text(hjust = 0.5, vjust = -0.5, size = 3) +
    labs(
      title = "Bubble Chart: Most Cited Papers",
      x = "Citations per Year",
      y = "Total Citations",
      size = "Normalized Citations"
    ) +
    ieee_theme

  # Save the plot
  save_plot(p, "most_cited_papers_bubble_chart")
  message("[INFO] Bubble chart for Most Cited Papers generated and saved.")
}





# ---------------------------------------------------------------------------- #
# Function: Generate Most Relevant Keywords Wordcloud using ggplot2
# ---------------------------------------------------------------------------- #
fn_most_rel_keywords_wordcloud <- function(most_rel_keywords, output_path) {
  if (is.null(most_rel_keywords) || nrow(most_rel_keywords) == 0) {
    stop("[ERROR] The input `most_rel_keywords` is NULL or empty.")
  }

  # Format column names
  colnames(most_rel_keywords)[1] <- "Author Keywords (DE)"
  colnames(most_rel_keywords)[2] <- "Article-Author-Keywords"

  # Convert counts to numeric
  most_rel_keywords$`Article-Author-Keywords` <- as.numeric(most_rel_keywords$`Article-Author-Keywords`)

  # Generate the word cloud using ggplot2
  p <- ggplot(
      most_rel_keywords, 
      aes(
        label = `Author Keywords (DE)`, 
        size = `Article-Author-Keywords`
      )
   )
    p <- p + geom_text_wordcloud(area_corr = TRUE, color = THEME_COLORS["Blue"])
    p <- p + scale_size_area(max_size = 65)
    p <- p + labs(
      title = "Most Relevant Author Keywords",
      subtitle = "Based on Article Contributions",
      caption = "Generated using ggplot2"
    )
    p <- p + theme_minimal()
    p <- p + theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0.5)
    )

  # Save plot
  save_plot(p, "most_rel_keywords_wordcloud")
  message("[INFO] Wordcloud generated and saved using ggplot2.")
}



# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Productive Countries
# ---------------------------------------------------------------------------- #
analyze_and_plot_most_prod_countries <- function(data, output_dir) {
  # Validate and preprocess input data
  data <- preprocess_data(data)

  # Generate bar plot
  top_countries <- get_top_countries(data, top_n = 15) # Get top 15 countries
  generate_bar_plot(
    data = top_countries,
    output_dir = output_dir,
    title = "Top 10 Most Productive Countries",
    x_label = "Country",
    y_label = "Number of Articles",
    file_name = "M1_MOST_PROD_COUNTRIES_BAR_PLOT",
    x_var = "Country",
    y_var = "Articles",
    threshold_var = "cumulative_percentage"  # Enables the 80% threshold functionality
  )

  # Generate Lorenz curve
  generate_inequality_curve(data, output_dir) # Generate Lorenz curve

  # Generate world map
  map_data <- prepare_map_data(data)  # Prepare map data
  generate_world_map(map_data, output_dir) # Generate world map

  message("[INFO] Country analysis visualizations (bar, Lorenz, map) generated successfully.")
}

# ---------------------------------------------------------------------------- #
# Helper Functions
# ---------------------------------------------------------------------------- #


#

# Prepare map data
prepare_map_data <- function(data) {
  map_data <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
  map_data <- map_data[!is.na(map_data@data$NAME) & map_data@data$NAME != "Antarctica", ]
  return(map_data)
}

# Generate world map
generate_world_map <- function(map_data, output_dir) {
  png_filename <- file.path(output_dir, "figures", "M1_MOST_PROD_COUNTRIES_MAP_GRAYSCALE.png")

  png(png_filename, width = 1200, height = 800, units = "px")
  par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)

  mapCountryData(
    map_data,
    nameColumnToPlot = "Articles",
    mapTitle = "Global Distribution of Scientific Articles by Country (Grayscale)",
    catMethod = "fixedWidth",
    colourPalette = grey.colors(5, start = 0.9, end = 0.1),
    addLegend = TRUE,
    borderCol = "#000000"
  )

  par(cex.main = 1, cex.axis = 1, cex.lab = 1)  # Reset parameters
  dev.off()
}


# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Total Citations Per Country
# ---------------------------------------------------------------------------- #

analyze_and_plot_tc_per_country <- function(data, output_dir) {
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
  save_json(top_countries, file.path(output_dir, "jsons", "tc_per_country.json"))
}

# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Relevant Sources
# ---------------------------------------------------------------------------- #
analyze_and_plot_most_rel_sources <- function(data, output_dir) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Convert Articles to numeric
  data$Articles <- as.numeric(data$Articles)

  # Sort by Articles and get the top 10
  top_sources <- data[order(-data$Articles), ]
  top_sources <- head(top_sources, 10)

  # Create bar plot
  p <- ggplot(top_sources, aes(x = reorder(Sources, Articles), y = Articles)) +
    geom_bar(stat = "identity", fill = THEME_COLORS["Orange"]) +
    coord_flip() +
    labs(
      title = "Top 10 Most Relevant Sources",
      x = "Source",
      y = "Number of Articles"
    ) +
    ieee_theme

  # Save plot
  save_plot(p, "most_rel_sources")
  save_json(top_sources, file.path(output_dir, "jsons", "most_rel_sources.json"))
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
  save_json(data, file.path(output_dir, "jsons", "bradford_law.json"))
}

