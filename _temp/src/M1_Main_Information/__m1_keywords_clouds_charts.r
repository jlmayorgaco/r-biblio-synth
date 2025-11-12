
# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc2 :: Generate Most Relevant Keywords Wordcloud using ggplot2
# Description: Creates a word cloud from the most relevant keywords using ggplot2.
# ---------------------------------------------------------------------------- #
fn_m1_mtrc2_most_rel_keywords_wordcloud <- function(most_rel_keywords, output_path = "results/M1_Main_Information/figures") {
  
  # Step 1: Validate Input Data
  if (is.null(most_rel_keywords) || nrow(most_rel_keywords) == 0) {
    stop("[ERROR] The input `most_rel_keywords` is NULL or empty.")
  }

  # Step 2: Ensure Correct Column Names
  colnames(most_rel_keywords)[1] <- "Author Keywords (DE)"
  colnames(most_rel_keywords)[2] <- "Article-Author-Keywords"
  colnames(most_rel_keywords)[3] <- "Keywords-Plus (ID)"
  colnames(most_rel_keywords)[4] <- "Keywords-Plus-Articles"

  # Step 3: Function to Split Keywords and Repeat Counts
  split_keywords <- function(keywords, counts) {
    split_words <- unlist(strsplit(keywords, " "))
    counts_repeated <- rep(counts, times = lengths(strsplit(keywords, " ")))
    data.frame(Keyword = split_words, Count = counts_repeated, stringsAsFactors = FALSE)
  }

  # Step 4: Process Author Keywords and Keywords-Plus
  author_keywords <- split_keywords(
    most_rel_keywords$`Author Keywords (DE)`, 
    as.numeric(most_rel_keywords$`Article-Author-Keywords`)
  )
  
  keywords_plus <- split_keywords(
    most_rel_keywords$`Keywords-Plus (ID)`, 
    as.numeric(most_rel_keywords$`Keywords-Plus-Articles`)
  )

  # Step 5: Combine and Clean Data
  combined_keywords <- rbind(author_keywords, keywords_plus)
  combined_keywords <- combined_keywords[!is.na(combined_keywords$Keyword) & combined_keywords$Keyword != "", ]
  
  # Aggregate Keywords and Sort by Frequency
  combined_keywords <- combined_keywords %>%
    group_by(Keyword) %>%
    summarise(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(Count))

  # Step 6: Generate the Word Cloud Plot using ggplot2
  p <- ggplot(combined_keywords, aes(label = Keyword, size = Count, color = Count))
  
  p <- p + geom_text_wordcloud(
    area_corr = TRUE,
    family = "Helvetica",
    rotate_ratio = 0.5,
    shape = "circle",
    grid_size = 10,
    rm_outside = FALSE
  )
  
  p <- p + scale_size_area(max_size = 100)
  
  p <- p + scale_color_gradientn(
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
  )
  
  p <- p + labs(title = "Most Relevant Keywords")
  
  p <- p + theme_void()
  
  p <- p + theme(
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
    legend.title = element_text(size = 10, face = "bold", color = THEME_COLORS$Text$Title),
    legend.text = element_text(size = 8, color = THEME_COLORS$Text$Body)
  )

  # Step 7: Save the Plot
  save_plot(p, "M1_G1_MOST_RELEVANT_KEYWORDS_WORDCLOUD", width = 8, height = 6, dpi = 600)
  
  # Step 8: Log Success Message
  message("[INFO] Wordcloud generated and saved successfully.")
}
# ---------------------------------------------------------------------------- #
# Function: M1 Metric Mtrc2 :: Generate Most Relevant Keywords Wordcloud2
# Description: Creates a word cloud using the wordcloud2 package and saves it as an HTML file.
# ---------------------------------------------------------------------------- #
fn_m1_mtrc2_most_rel_keywords_wordcloud2 <- function(most_rel_keywords) {
  
  # Ensure required packages are loaded
  library(dplyr)
  library(wordcloud2)
  library(htmlwidgets)
  
  # Debugging: Print the input data
  message(' ')
  message(' ')
  message(' ')
  message(' ')
  message(' ')
  message(' =============================================> most_rel_keywords <=============================================' )
  message(' most_rel_keywords ')
  print(most_rel_keywords)
  message(' ')
  message(' ')
  message(' ')
  message(' ')
  
  # Step 1: Validate Input Data
  if (is.null(most_rel_keywords) || nrow(most_rel_keywords) == 0) {
    stop("[ERROR] The input `most_rel_keywords` is NULL or empty.")
  }

  # Step 2: Clean and Fix Duplicate Column Names
  colnames(most_rel_keywords) <- make.unique(trimws(colnames(most_rel_keywords)))
  
  # Print column names for debugging
  message("[INFO] Column names after cleaning and making unique:")
  print(colnames(most_rel_keywords))
  
  # Dynamically detect and rename columns based on actual names
  author_keywords_col <- grep("Author Keywords", colnames(most_rel_keywords), value = TRUE)
  article_keywords_col <- grep("Keywords-Plus", colnames(most_rel_keywords), value = TRUE)
  articles_col <- grep("^Articles(\\.\\d*)?$", colnames(most_rel_keywords), value = TRUE)

  # Print detected column names for debugging
  message("[INFO] Detected columns:")
  message(" - Author Keywords Column: ", author_keywords_col)
  message(" - Keywords-Plus Column: ", article_keywords_col)
  message(" - Articles Column: ", articles_col)

  # Ensure we have the right columns
  if (length(author_keywords_col) > 0 && length(article_keywords_col) > 0 && length(articles_col) >= 2) {
    most_rel_keywords <- most_rel_keywords %>%
      rename(
        `Author Keywords (DE)` = all_of(author_keywords_col),
        `Article-Author-Keywords` = all_of(articles_col[2])  # Use the second "Articles" column for Keywords-Plus
      )
    message("[INFO] Dynamically renamed columns successfully.")
  } else {
    stop("[ERROR] Required columns `Author Keywords (DE)` or `Keywords-Plus (ID)` not found.")
  }

  # Print renamed column names for verification
  message("[INFO] Renamed column names:")
  print(colnames(most_rel_keywords))
  
  # Step 3: Convert Counts to Numeric
  most_rel_keywords$`Article-Author-Keywords` <- as.numeric(most_rel_keywords$`Article-Author-Keywords`)
  
  # Check for NAs after conversion
  if (any(is.na(most_rel_keywords$`Article-Author-Keywords`))) {
    stop("[ERROR] Column `Article-Author-Keywords` contains non-numeric values.")
  }

  # Step 4: Prepare Data for Wordcloud2 with dplyr::filter to avoid namespace conflict
  wordcloud_data <- most_rel_keywords %>%
    dplyr::select(`Author Keywords (DE)`, `Article-Author-Keywords`) %>%
    rename(word = `Author Keywords (DE)`, freq = `Article-Author-Keywords`) %>%
    dplyr::filter(freq > 2)  # Use dplyr::filter to avoid conflict

  # Check if filtered data is empty
  if (nrow(wordcloud_data) == 0) {
    stop("[ERROR] No keywords left after filtering low-frequency terms.")
  }

  # Step 5: Generate the Word Cloud using wordcloud2
  wordcloud <- wordcloud2(
    data = wordcloud_data, 
    size = 1.5,           # Increase size to fit more words
    shape = "circle"
  )
  
  # Step 6: Save the Word Cloud as an HTML File
  output_html <- file.path("results/M1_Main_Information/figures", "M1_Most_Rel_Keywords_Wordcloud.html")
  
  # Create directory if not exists
  if (!dir.exists(dirname(output_html))) {
    dir.create(dirname(output_html), recursive = TRUE)
  }
  
  htmlwidgets::saveWidget(wordcloud, output_html, selfcontained = FALSE)
  
  # Step 7: Log Success Message
  message("[INFO] Wordcloud saved at: ", output_html)
}




fn_m1_mtrc2_all_keywords_wordcloud <- function(extracted_data) {

  library(wordcloud2)
  library(webshot)
  library(htmlwidgets)

  if (is.null(extracted_data$keywords) || is.null(extracted_data$titles) || is.null(extracted_data$descriptions)) {
    stop("[ERROR] Extracted data must include `keywords`, `titles`, and `descriptions`.")
  }

  # Combine text data
  combined_text <- c(
    unlist(extracted_data$keywords),
    unlist(extracted_data$titles),
    unlist(extracted_data$descriptions)
  )
  
  # Clean text data
  cleaned_text <- tolower(combined_text)
  cleaned_text <- gsub("[[:punct:]]", " ", cleaned_text)
  cleaned_text <- gsub("[[:digit:]]", "", cleaned_text)
  cleaned_text <- gsub("\\s+", " ", cleaned_text)
  cleaned_text <- trimws(cleaned_text)

  # Tokenize and filter words
  words <- unlist(strsplit(cleaned_text, split = " "))
  stopwords <- c(stopwords::stopwords("en"), "also", "however", "therefore", "thus", "hence")
  words <- words[!words %in% stopwords]
  words <- words[nchar(words) > 1]

  # Word frequency
  word_counts <- as.data.frame(table(words))
  colnames(word_counts) <- c("word", "freq")
  word_counts <- word_counts[order(-word_counts$freq), ]
  word_counts <- head(word_counts, 100)


  # Custom color function
  color_function <- "function(word, weight) {
      var maxWeight = 4000;  // Replace with approximate maximum frequency
      var intensity = weight / maxWeight;
      intensity = Math.min(1, Math.max(0.1, intensity)); // Clamp intensity
      return `rgba(0, 0, 255, ${intensity})`;  // Blue gradient
  }"

  # Generate word cloud
  wordcloud_plot <- wordcloud2::wordcloud2(
    data = word_counts,
    size = 1.2,
    color = htmlwidgets::JS(color_function),
    shape = "ellipse",
    gridSize = 10,
    rotateRatio = 0.015
  )


  # Save word cloud as HTML
  htmlwidgets::saveWidget(
    wordcloud_plot,
    "results/M1_Main_Information/figures/M1_Most_Rel_Keywords_Wordcloud.html",
    selfcontained = TRUE
  )

  # Save word cloud as PNG
  webshot(
    url = "results/M1_Main_Information/figures/M1_Most_Rel_Keywords_Wordcloud.html",
    file = "results/M1_Main_Information/figures/M1_Most_Rel_Keywords_Wordcloud.png",
    delay = 25,
    vwidth = 2000,
    vheight = 1200
  )

  message("[INFO] Word cloud saved as HTML and PNG.")
  return(word_counts)
}


fn_m1_mtrc2_all_keywords_wordcloud_by_years <- function(extracted_data) {
  preprocessed <- wc_3x3_preprocess_data(extracted_data)
  wordcloud_data <- wc_3x3_create_wordcloud_data(preprocessed$data, preprocessed$word_freq, preprocessed$top_100_words)
  wc_3x3_plot_and_save_results(wordcloud_data$grobs, wordcloud_data$chunks_details)
}

