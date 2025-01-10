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
source('../../src/M1_Main_Information/_report.r')

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
  plot <- generate_bar_plot_horizontal(
    data = metrics,
    title = "Top 10 Most Productive Authors",
    x_label = "Authors",
    y_label = "Number of Articles",
    x_var = "Authors",
    y_var = "Articles"
  )

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
      file_name = file.path(output_dir, "M1_G3_AUTHOR_CONTRIBUTIONS_LORENZ_PLOT"),
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
# Function: Generate and Save Bubble Chart for Most Cited Papers
# ---------------------------------------------------------------------------- #
fn_m1_mtrc4_generate_bubble_chart <- function(data) {
  tryCatch({
    # Step 1: Ensure unique column names
    colnames(data) <- make.unique(colnames(data))

    # Step 2: Validate the required columns
    required_cols <- c("TC", "TCperYear", "NTC", "PaperID")
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Step 3: Convert relevant columns to numeric
    numeric_cols <- c("TC", "TCperYear", "NTC")
    for (col in numeric_cols) {
      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
    }

    # Step 4: Remove rows with invalid or missing values
    valid_data <- data[complete.cases(data[numeric_cols]), ]
    if (nrow(valid_data) == 0) {
      stop("[ERROR] No valid rows in the dataset after cleaning values.")
    }

    # Step 5: Sort by Total Citations and select the top 10 papers
    top_papers <- head(valid_data[order(-valid_data$TC), ], 10)

    # Debug: Print top papers
    message("[DEBUG] Top papers selected for bubble chart:")
    print(top_papers)

    # Step 6: Generate the bubble chart
    bubble_chart <- ggplot(top_papers, aes(
      x = TCperYear,
      y = TC,
      size = NTC,
      label = PaperID
    )) +
      geom_point(alpha = 0.7, color = THEME_COLORS$Main[1]) + # Single color for bubbles
      geom_text_repel(aes(label = PaperID), size = 3, fontface = "bold") +
      labs(
        title = "Bubble Chart: Most Cited Papers",
        x = "Citations per Year",
        y = "Total Citations",
        size = "Normalized Citations"
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

    # Debug: Check if the plot is a valid ggplot object
    if (!inherits(bubble_chart, "ggplot")) {
      stop("[ERROR] Generated bubble chart is not a valid ggplot object.")
    }

    # Step 7: Save the bubble chart
    save_plot(
      plot = bubble_chart,
      filename_prefix = "M1_G4_MOST_CITED_PAPERS_BUBBLE_CHART",
      width = 8,
      height = 6,
      dpi = 600
    )

    # Log completion
    message("[INFO] Bubble chart for Most Cited Papers generated and saved successfully.")
  }, error = function(e) {
    message("[ERROR] An error occurred in fn_m1_mtrc4_generate_bubble_chart: ", e$message)
  })
}






# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Most Productive Countries
# ---------------------------------------------------------------------------- #
fn_m1_mtrc5_analyze_and_plot_most_prod_countries <- function(data) {
  # Step 1: Validate and preprocess input data
  message("[INFO] Validating and preprocessing data for country analysis...")
  data <- preprocess_data(data, required_columns = c("Country", "Articles", "SCP", "MCP"))

  # Step 2: Generate bar plot for top productive countries
  message("[INFO] Generating bar plot for the most productive countries...")
  top_countries <- get_top_countries(data, top_n = 15)
  generate_bar_plot_horizontal(
    data = top_countries,
    title = "Top 15 Most Productive Countries",
    x_label = "Country",
    y_label = "Number of Articles",
    x_var = "Country",
    y_var = "Articles",
    file_name = "M1_G5_MOST_PROD_COUNTRIES_BAR_PLOT"
  )

  # Step 3: Generate Lorenz curve for inequality analysis
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

  # Step 4: Generate world map for article distribution
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

  data <- data %>%
    mutate(
      Country = as.character(Country),          # Convert Country to character
      Freq = as.numeric(Freq),                  # Convert Freq to numeric
      SCP = as.numeric(SCP),                    # Convert SCP to numeric
      MCP = as.numeric(MCP),                    # Convert MCP to numeric
      MCP_Ratio = as.numeric(MCP_Ratio)         # Convert MCP_Ratio to numeric
    )
  generate_treemap(
    data = data,
    value_col = "Articles",
    label_col = "Country",
    title = "Article Distribution by Country",
    file_name = "M1_G5_MOST_PROD_COUNTRIES_TREEMAP"
  )

  # Step 5: Generate the stacked plot for top 10 countries
  message("[INFO] Generating stacked bar plot for the most productive countries...")
generate_bar_stacked(
  data = data, 
  title = "Most Productive Countries",
  x_label = "N. of Documents",
  y_label = "Countries",
  categorical_var_col = "Country",
  col_a = "SCP",
  col_b = "MCP",
  col_a_label = "SCP",
  col_b_label = "MCP",
  file_name = "M1_G5_MOST_PROD_COUNTRIES_STACKED_BAR_PLOT"
)

  # Step 6: Bar Plot for SCP (Single Country Publications)
  message("[INFO] Generating bar plot for SCP...")
  data <- data %>% mutate(SCP = as.numeric(SCP))
  generate_bar_plot_horizontal(
    data = data,
    title = "Single Country Publications (SCP)",
    x_label = "Country",
    y_label = "Number of SCP Articles",
    x_var = "Country",
    y_var = "SCP",
    add_threshold_line = FALSE,
    file_name = "M1_G5_MOST_PROD_COUNTRIES_SCP_BAR_PLOT"
  )

  # Step 7: Bar Plot for MCP (Multiple Country Publications)
  message("[INFO] Generating bar plot for MCP...")
  data <- data %>% mutate(MCP = as.numeric(MCP))
  generate_bar_plot_horizontal(
    data = data,
    title = "Multiple Country Publications (MCP)",
    x_label = "Country",
    y_label = "Number of MCP Articles",
    x_var = "Country",
    y_var = "MCP",
    add_threshold_line = FALSE,
    file_name = "M1_G5_MOST_PROD_COUNTRIES_MCP_BAR_PLOT"
  )


  # Final log message
  message("[INFO] Most productive countries analysis (bar plot, Lorenz curve, world map, stacked bar plot) completed successfully.")
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
# Function: Analyze and Plot Total Citations Per Country
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Function: Analyze and Plot Total Citations Per Country
# ---------------------------------------------------------------------------- #
fn_m1_mtrc5_analyze_and_plot_tc_per_country <- function(data) {
  # Step 1: Validate and preprocess input data
  message("[INFO] Validating and preprocessing data for citation analysis...")
  data <- preprocess_data(data, required_columns = c("Country", "Total Citations", "Average Article Citations"))


data <- data %>%
  mutate(
    `Total Citations` = suppressWarnings(as.numeric(`Total Citations`)),
    `Average Article Citations` = suppressWarnings(as.numeric(`Average Article Citations`))
  )

  # Step 2: Generate bar plot for top countries by total citations
  message("[INFO] Generating bar plot for the countries with the most citations...")
  top_countries <- get_top_countries(data, column = "Total Citations", top_n = 15)

generate_bar_plot_horizontal(
    data = top_countries,
    title = "Top 15 Countries by Total Citations",
    x_label = "Country",
    y_label = "Number of Citations",
    x_var = "Country",
    y_var = "Total Citations",
    file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_BAR_PLOT"
  )


  # Step 3: Generate Lorenz curve for inequality analysis based on total citations
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

data <- data %>%
  rename(Total_Citations = `Total Citations`) %>%
  mutate(Total_Citations = suppressWarnings(as.numeric(Total_Citations)))

data <- data %>%
  rename(Average_Article_Citations = `Average Article Citations`) %>%
  mutate(Average_Article_Citations = suppressWarnings(as.numeric(Average_Article_Citations)))



  # Step 4: Generate world map for citation distribution
  message("[INFO] Generating world map for citation distribution...")
  tryCatch({
    generate_world_map(
      map_data = data,
      output_dir = "results/M1_Main_Information/figures",
      value_col = "Total_Citations",
      map_title = "Global Distribution of Citations",
      file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_MAP",
      color_scheme = "blues"
    )
  }, error = function(e) {
    message("[ERROR] Failed to generate world map: ", e$message)
  })


  data <- data %>%
    mutate(
      Country = as.character(Country),
      Total_Citations = as.numeric(Total_Citations),
      Average_Article_Citations = as.numeric(Average_Article_Citations)
    )

  # Step 5: Generate treemap for citation distribution
  message("[INFO] Generating treemap for citation distribution...")
  generate_treemap(
    data = data,
    value_col = "Total_Citations",
    label_col = "Country",
    title = "Citation Distribution by Country",
    file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_TREEMAP"
  )



  # Step 6: Generate stacked bar plot for the most cited countries
  message("[INFO] Generating stacked bar plot for the countries with most citations...")
generate_bar_stacked(
    data = data,
    title = "Most Cited Countries",
    x_label = "Number of Citations",
    y_label = "Countries",
    file_name = "M1_G5_TOP_COUNTRIES_BY_TOTAL_CITATIONS_STACKED_BAR_PLOT",
    categorical_var_col = "Country",
    col_a = "Total_Citations",
    col_b = "Average_Article_Citations",
    col_a_label = "Total Citations",
    col_b_label = "Average Citations"
)

  # Step 7: Bar Plot for Average Citations per Article
  message("[INFO] Generating bar plot for average citations per article...")
  generate_bar_plot_horizontal(
    data = data,
    title = "Average Citations per Article",
    x_label = "Country",
    y_label = "Average Citations",
    x_var = "Country",
    y_var = "Average_Article_Citations",
    add_threshold_line = FALSE,
    file_name = "M1_G5_TOP_COUNTRIES_BY_AVERAGE_CITATIONS_BAR_PLOT"
  )


  # Final log message
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