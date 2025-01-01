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

  extracted_data <- extract_bibliographic_data(bib_data, res1)

  # Extract and clean additional summary information
  summary_df <- clean_whitespace(s1$MainInformationDF)
  most_prod_authors <- clean_whitespace(s1$MostProdAuthors)
  annual_production <- clean_whitespace(s1$AnnualProduction)
  most_cited_papers <- clean_whitespace(s1$MostCitedPapers)
  most_prod_countries <- clean_whitespace(s1$MostProdCountries)
  tc_per_countries <- clean_whitespace(s1$TCperCountries)
  most_rel_sources <- clean_whitespace(s1$MostRelSources)
  most_rel_keywords <- clean_whitespace(s1$MostRelKeywords)


  # Assign unique IDs based on DOI match
  most_cited_papers <- most_cited_papers %>%
    dplyr::mutate(
      PaperID = dplyr::case_when(
        DOI %in% bib_data$DI ~ paste0("[", match(DOI, bib_data$DI), "]"),
        TRUE ~ NA_character_ # If no match, set to NA
      )
    )


  # Ensure there are no missing IDs
  if (any(is.na(most_cited_papers$PaperID))) {
    warning("[WARNING] Some papers could not be assigned unique IDs.")
  }

  # Ensure PaperID column is present
  if (!"PaperID" %in% colnames(most_cited_papers)) {
    stop("[ERROR] Unable to assign unique IDs. Check DOI consistency between datasets.")
  }
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
    bradford_law = bradford_law,
    extracted_data = extracted_data
  )

  return(main_information_data)
}

# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc1 :: Save Document Types Pie Chart
# ---------------------------------------------------------------------------- #
fn_m1_mtrc1_articles_types_pie <- function(v_document_types) {
  # Create data frame from document types
  df <- data.frame(
    Document_Type = names(v_document_types),
    Count = sapply(v_document_types, function(x) ifelse(length(x) == 0, 0, x))
  )

  # Filter out types with zero count and apply label mapping
  df <- df[df$Count > 0, ]
  df$Document_Type <- LABEL_MAPPING[df$Document_Type]

  # Create the pie chart
  pie_chart <- ggplot(df, aes(x = "", y = Count, fill = Document_Type)) +
    geom_bar(
      width = 1, 
      stat = "identity", 
      color = THEME_COLORS$Grayscale$Black, 
      size = 0.25
    ) +
    coord_polar("y", start = 0) +
    labs(
      title = "Document Types Distribution"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 16, 
        face = "bold", 
        hjust = 0.5, 
        margin = margin(t = 10, b = 10), 
        color = THEME_COLORS$Text$Title
      ),
      legend.title = element_blank(),
      legend.text = element_text(
        size = 12, 
        color = THEME_COLORS$Text$Body
      ),
      legend.position = "right"
    ) +
    scale_fill_manual(values = THEME_COLORS$Main)

  # Save the plot using the save_plot function
  save_plot(pie_chart, "M1_G2_DOCUMENT_TYPES_PIE_PLOT", width = 6, height = 4, dpi = 600)
}

# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc1 :: Generate Most Relevant Keywords Wordcloud using ggplot2
# ---------------------------------------------------------------------------- #
fn_m1_mtrc2_most_rel_keywords_wordcloud <- function(most_rel_keywords, output_path = "results/M1_Main_Information/figures") {
  
  # Check if input data is null or empty
  if (is.null(most_rel_keywords) || nrow(most_rel_keywords) == 0) {
    stop("[ERROR] The input `most_rel_keywords` is NULL or empty.")
  }

  # Ensure the columns contain the correct data
  colnames(most_rel_keywords)[1] <- "Author Keywords (DE)"
  colnames(most_rel_keywords)[2] <- "Article-Author-Keywords"
  colnames(most_rel_keywords)[3] <- "Keywords-Plus (ID)"
  colnames(most_rel_keywords)[4] <- "Keywords-Plus-Articles"

  # Function to split keywords and repeat counts appropriately
  split_keywords <- function(keywords, counts) {
    split_words <- unlist(strsplit(keywords, " "))
    counts_repeated <- rep(counts, times = lengths(strsplit(keywords, " ")))
    data.frame(Keyword = split_words, Count = counts_repeated, stringsAsFactors = FALSE)
  }

  # Process both "Author Keywords (DE)" and "Keywords-Plus (ID)"
  author_keywords <- split_keywords(
    most_rel_keywords$`Author Keywords (DE)`, 
    as.numeric(most_rel_keywords$`Article-Author-Keywords`)
  )
  keywords_plus <- split_keywords(
    most_rel_keywords$`Keywords-Plus (ID)`, 
    as.numeric(most_rel_keywords$`Keywords-Plus-Articles`)
  )

  # Combine both into a single data frame
  combined_keywords <- rbind(author_keywords, keywords_plus)

  # Remove NA or empty keywords
  combined_keywords <- combined_keywords[!is.na(combined_keywords$Keyword) & combined_keywords$Keyword != "", ]

  # Aggregate duplicate keywords and sum their counts
  combined_keywords <- combined_keywords %>%
    group_by(Keyword) %>%
    summarise(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup()

  # Sort keywords by frequency
  combined_keywords <- combined_keywords %>%
    arrange(desc(Count))

  # Generate the word cloud
  p <- ggplot(
    combined_keywords, 
    aes(
      label = Keyword,
      size = Count,
      color = Count
    )
  ) +
    geom_text_wordcloud(
      area_corr = TRUE,
      family = "Helvetica",
      rotate_ratio = 0.5,
      shape = "circle",
      grid_size = 10,
      rm_outside = FALSE
    ) +
    scale_size_area(max_size = 100) +
    scale_color_gradientn(
      colors = c(
        THEME_COLORS$Categorical["Lower"], 
        THEME_COLORS$Categorical["Medium"], 
        THEME_COLORS$Categorical["Bigger"]
      ),
      values = scales::rescale(c(1, 10, 50, max(combined_keywords$Count))),
      name = "Word Frequency",
      guide = guide_colorbar(
        barwidth = 10, barheight = 0.8, 
        title.position = "top", title.hjust = 0.5
      )
    ) +
    labs(
      title = "Most Relevant Keywords"
      #subtitle = "Word frequency representation",
      #caption = "Generated using ggplot2"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 16, face = "bold", hjust = 0.5, 
        color = THEME_COLORS$Text$Title,
        margin = margin(t = 10, b = 10)
      ),
      plot.subtitle = element_text(
        size = 12, hjust = 0.5, 
        color = THEME_COLORS$Text$Subtitle,
        margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_text(
        size = 10, hjust = 0.5, 
        color = THEME_COLORS$Text$Body,
        margin = margin(t = 5)
      ),
      legend.position = "bottom",
      legend.title = element_text(
        size = 10, face = "bold", color = THEME_COLORS$Text$Title
      ),
      legend.text = element_text(
        size = 8, color = THEME_COLORS$Text$Body
      )
    )

  # Save the plot
  save_plot(
    p, 
    "M1_G1_MOST_RELEVANT_KEYWORDS_WORDCLOUD", 
    width = 8, height = 6, dpi = 600
  )

  # Log a success message
  message("[INFO] Wordcloud generated and saved successfully.")
}

# ---------------------------------------------------------------------------- #
# Function: Generate Most Relevant Keywords Wordcloud2
# ---------------------------------------------------------------------------- #
fn_m1_mtrc2_most_rel_keywords_wordcloud2 <- function(most_rel_keywords) {
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
create_wordcloud_from_text <- function(extracted_data, output_dir) {
  # Validate extracted_data input
  if (is.null(extracted_data$keywords) || is.null(extracted_data$titles) || is.null(extracted_data$descriptions)) {
    stop("[ERROR] Extracted data must include `keywords`, `titles`, and `descriptions`.")
  }
  
  # Combine keywords, titles, and descriptions into a single text vector
  combined_text <- c(
    unlist(extracted_data$keywords),
    unlist(extracted_data$titles),
    unlist(extracted_data$descriptions)
  )
  
  # Clean the text
  cleaned_text <- tolower(combined_text) # Convert to lowercase
  cleaned_text <- gsub("[[:punct:]]", " ", cleaned_text) # Remove punctuation
  cleaned_text <- gsub("[[:digit:]]", "", cleaned_text) # Remove numbers
  cleaned_text <- gsub("\\s+", " ", cleaned_text) # Remove extra whitespace
  cleaned_text <- trimws(cleaned_text) # Trim leading/trailing spaces
  
  # Tokenize the text into words
  words <- unlist(strsplit(cleaned_text, split = " "))
  
  # Remove stopwords (common words like 'the', 'and', etc.)
  stopwords <- c("the", "and", "of", "in", "to", "for", "on", "with", "by", "a", "an", "is", "this", "that", "it", "as", "at", "from", "are")
  words <- words[!words %in% stopwords]
  
  # Count word frequencies
  word_counts <- as.data.frame(table(words))
  colnames(word_counts) <- c("word", "freq")
  word_counts <- word_counts[order(-word_counts$freq), ] # Sort by frequency
  
  # Debugging: Print top words
  print(head(word_counts, 10))
  
  # Create a word cloud
  p <- ggplot(word_counts, aes(label = word, size = freq)) +
    geom_text_wordcloud(area_corr = TRUE, color = "darkblue") +
    scale_size_area(max_size = 10) + # Adjust max size for better visualization
    labs(
      title = "Word Cloud from Extracted Data",
      subtitle = "Based on Keywords, Titles, and Descriptions",
      caption = "Generated using ggwordcloud"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5)
    )
  
  # Save the plot
  output_file <- file.path(output_dir, "wordcloud_extracted_data.png")
  ggsave(output_file, plot = p, width = 8, height = 6, dpi = 300)
  message("[INFO] Word cloud saved at: ", output_file)
  
  return(word_counts) # Return word counts for further analysis
}

# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Productive Authors
# ---------------------------------------------------------------------------- #
fn_m1_mtrc3_analyze_and_plot_most_prod_authors <- function(data) {
  # Step 1: Validate data
  data <- validate_data(data, c("Authors", "Articles"))
  
  # Step 2: Calculate metrics
  metrics <- calculate_metrics_top_authors(data)
  
  # Step 3: Generate the plot
  plot <- generate_bar_plot(
    data = metrics,
    title = "Top 10 Most Productive Authors",
    x_label = "Authors",
    y_label = "Number of Articles",
    x_var = "Authors",
    y_var = "Articles"
  )

  message("[INFO] ======>  Most Productive Authors plot generated sucessfuly.")
  print(plot)
  
  # Step 4: Save JSON
  save_json(metrics, "most_prod_authors.json")
  
  # Step 5: Save plot
  save_plot(plot, "M1_G3_MOST_PROD_AUTHORS_BAR_PLOT", width = 8, height = 6, dpi = 600)
  
  # Log success
  message("[INFO] Most Productive Authors analysis completed successfully.")
}
# ---------------------------------------------------------------------------- #
# Function: Generate Lorenz Curve for Author Contributions with Debug Logs
# ---------------------------------------------------------------------------- #
fn_m1_mtrc3_generate_lorenz_curve <- function(data, output_dir) {
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

  # Call the generic Lorenz curve generation function
  gini <- generate_lorenz_curve(
    data = data, 
    value_col = "Articles",
    entity_col = "Authors",
    plot_title = "Lorenz Curve: Author Contributions",
    x_label = "Cumulative Percentage of Articles",
    y_label = "Cumulative Percentage of Authors",
    file_name = "M1_G3_AUTHOR_CONTRIBUTIONS_LORENZ_PLOT"
  )

  # Log the Gini coefficient for reporting
  message("[INFO] Gini Coefficient for Author Contributions: ", round(gini, 3))
  message("[INFO] Lorenz Curve for Author Contributions generated and saved successfully.")
}



 
# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Cited Papers
# ---------------------------------------------------------------------------- #
fn_m1_mtrc4_analyze_and_plot_most_cited_papers <- function(data) {
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
  if (!("Paper" %in% colnames(data))) {
    message("[INFO] Available columns: ", paste(colnames(data), collapse = ", "))
    stop("[ERROR] 'Paper' column is missing in the dataset.")
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

  # Step 4: Plot the data using a helper function
  plot <- generate_bar_plot(
    data = top_cited_papers,
    title = "Top 10 Papers by Citations Per Year",
    x_label = "Citations Per Year",
    y_label = "Papers",
    x_var = "TCperYear",
    y_var = "Paper",
    file_name = NULL # File name handled later
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
# Function: Analyze and Plot Most Productive Countries
# ---------------------------------------------------------------------------- #
analyze_and_plot_most_prod_countries <- function(data, output_dir) {
  # Validate and preprocess input data
  data <- preprocess_data(data)

  # Generate bar plot
  top_countries <- get_top_countries(data, top_n = 15) # Get top 15 countries
  plot <- generate_bar_plot(
    data = top_countries, 
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
  save_json(top_countries, "tc_per_country.json")
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
  save_json(top_sources, "most_rel_sources.json")
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

