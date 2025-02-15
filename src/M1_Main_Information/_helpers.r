




# ---------------------------------------------------------------------------- #
# Function: Clean Whitespace
# ---------------------------------------------------------------------------- #
clean_whitespace <- function(df) {
  colnames(df) <- trimws(colnames(df))
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      gsub("\\s+", " ", trimws(x))
    } else {
      x
    }
  })
  return(df)
}




# ---------------------------------------------------------------------------- #
# Function: Save JSON
# ---------------------------------------------------------------------------- #
save_json <- function(data, filename, path = "results/M1_Main_Information/jsons/", source = "Scopus Dataset") {
  # Ensure the directory for the JSON file exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Prepare metadata for JSON
  metadata <- list(
    analysis_date = Sys.Date(),
    source = source,
    data = data
  )

  # Construct the full file path
  filepath <- file.path(path, filename)

  # Write JSON to the specified path
  tryCatch({
    jsonlite::write_json(metadata, filepath, pretty = TRUE, auto_unbox = TRUE)
    message("[INFO] JSON saved successfully at: ", filepath)
  }, error = function(e) {
    stop("[ERROR] Failed to save JSON: ", e$message)
  })
}



# Preprocess input data
preprocess_data <- function(data, required_columns = NULL) {
  if (!is.null(required_columns)) {
    message("[DEBUG] Checking for required columns...")
    print(required_columns)
    print(names(data))

    # Check for missing columns
    missing_cols <- setdiff(required_columns, names(data))
    if (length(missing_cols) > 0) {
      stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
  }

  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Convert 'Articles' column to numeric if it exists
  if ("Articles" %in% colnames(data)) {
    data$Articles <- suppressWarnings(as.numeric(data$Articles))
    if (any(is.na(data$Articles))) {
      stop("[ERROR] 'Articles' column contains non-numeric or missing values.")
    }
  }

  # Normalize country names if 'Country' column exists
  if ("Country" %in% colnames(data)) {
    data$Country <- normalize_country_names(data$Country)
  }

  return(data)
}



# Normalize country names to match map dataset
normalize_country_names <- function(country) {
  dplyr::case_when(
    country == "USA" ~ "UNITED STATES",
    country == "KOREA" ~ "SOUTH KOREA",
    country == "UNITED KINGDOM" ~ "UNITED KINGDOM",
    TRUE ~ country  # Default: keep as is
  )
}

# Get top N countries by a specified column
get_top_countries <- function(data, column = "Articles", top_n = 10) {
  if (!column %in% colnames(data)) {
    stop("[ERROR] Specified column not found in data: ", column)
  }

  if (!is.numeric(data[[column]])) {
    stop("[ERROR] Specified column is not numeric: ", column)
  }

  # Sort data by the specified column in descending order
  top_countries <- data[order(-data[[column]]), ]

  # Select the top N rows
  top_countries <- head(top_countries, top_n)

  # Calculate cumulative percentages for threshold functionality
  top_countries$cumulative_percentage <- cumsum(top_countries[[column]]) / sum(top_countries[[column]])

  return(top_countries)
}


# ---------------------------------------------------------------------------- #
# Function: Extract Main Information
# ---------------------------------------------------------------------------- #
extract_main_information <- function(summary_df) {
  list(
    timespan = as.character(summary_df[summary_df$Description == "Timespan", "Results"]),
    sources = as.integer(summary_df[summary_df$Description == "Sources (Journals, Books, etc)", "Results"]),
    documents = as.integer(summary_df[summary_df$Description == "Documents", "Results"]),
    annual_growth_rate = as.numeric(summary_df[summary_df$Description == "Annual Growth Rate %", "Results"]),
    document_average_age = as.numeric(summary_df[summary_df$Description == "Document Average Age", "Results"]),
    avg_citations_per_doc = as.numeric(summary_df[summary_df$Description == "Average citations per doc", "Results"]),
    avg_citations_per_year_per_doc = as.numeric(summary_df[summary_df$Description == "Average citations per year per doc", "Results"]),
    references = as.integer(summary_df[summary_df$Description == "References", "Results"]),
    document_types = extract_document_types(summary_df),
    authors = as.integer(summary_df[summary_df$Description == "Authors", "Results"]),
    authors_per_doc = as.numeric(summary_df[summary_df$Description == "Authors per Document", "Results"]),
    co_authors_per_doc = as.numeric(summary_df[summary_df$Description == "Co-Authors per Documents", "Results"]),
    international_collaborations = as.numeric(summary_df[summary_df$Description == "International co-authorships %", "Results"]),
    single_author_documents = as.integer(summary_df[summary_df$Description == "Single-authored documents", "Results"]),
    multi_author_documents = as.integer(summary_df[summary_df$Description == "Multi-authored documents", "Results"])
  )
}

# ---------------------------------------------------------------------------- #
# Function: Extract Document Types
# ---------------------------------------------------------------------------- #
extract_document_types <- function(summary_df) {
  list(
    article = as.integer(summary_df[summary_df$Description == "article", "Results"]),
    article_article = as.integer(summary_df[summary_df$Description == "article article", "Results"]),
    article_conference_paper = as.integer(summary_df[summary_df$Description == "article conference paper", "Results"]),
    article_review = as.integer(summary_df[summary_df$Description == "article review", "Results"]),
    conference_paper = as.integer(summary_df[summary_df$Description == "conference paper", "Results"]),
    review = as.integer(summary_df[summary_df$Description == "review", "Results"])
  )
}
extract_bibliographic_data <- function(bib_data, res1) {
  # Extract Author Keywords (DE) and Keywords Plus (ID)
  author_keywords <- bib_data$DE
  keywords_plus <- bib_data$ID
    # Extract Titles and Descriptions/Abstracts
  titles <- bib_data$TI
  descriptions <- bib_data$AB

  
  # Combine and deduplicate all keywords
  all_keywords <- unique(c(author_keywords, keywords_plus))
  
  # Create a dataframe associating DE with publication years
  message("[DEBUG] Splitting DE into individual keywords...")

  keywords_split <- strsplit(bib_data$DE, "; ")
   all_keywords_combined <- mapply(function(de, id, ti, ab) {
    paste(c(de, id, ti, ab), collapse = " ")
  }, bib_data$DE, bib_data$ID, bib_data$TI, bib_data$AB, SIMPLIFY = TRUE)
  
# Create a dataframe associating combined keywords with publication years
  if (length(bib_data$PY) == length(all_keywords_combined)) {
    message("[DEBUG] Lengths match. Creating dataframe...")
    all_keywords_df <- data.frame(
      Keywords = all_keywords_combined,
      Years = bib_data$PY,
      stringsAsFactors = FALSE
    )
  } else {
    warning("[WARN] Length mismatch between years and combined keywords. Creating empty dataframe.")
    all_keywords_df <- data.frame(Keywords = character(), Years = numeric(), stringsAsFactors = FALSE)
  }



 
# Text cleaning function
clean_text <- function(text) {
  text <- gsub("\\b[©|®|™]\\b", "", text) # Remove copyright, registered trademark, and trademark symbols
  text <- gsub("\\[.*?\\]", "", text)     # Remove content within square brackets
  text <- gsub("[[:punct:]]", " ", text)  # Remove punctuation
  text <- gsub("[[:digit:]]", " ", text)  # Remove numbers
  text <- gsub("\\s+", " ", text)         # Remove extra spaces
  text <- tolower(text)                   # Convert to lowercase
  trimws(text)                            # Trim leading/trailing spaces
}

   # Remove stop words
remove_stopwords <- function(text) {
  words <- unlist(strsplit(text, " "))
  paste(words[!words %in% stopwords::stopwords("en")], collapse = " ")
}

  # Combine into a structured list
  extracted_data <- list(
    all_keywords = data.frame(
      Keywords = clean_text(paste(bib_data$DE, bib_data$ID, bib_data$TI, bib_data$AB, sep = " ")),
      Years = bib_data$PY,
      stringsAsFactors = FALSE
    ),
    keywords = unique(clean_text(c(bib_data$DE, bib_data$ID))),
    titles = clean_text(bib_data$TI),
    descriptions = clean_text(bib_data$AB)
  )


# Apply stopword removal
extracted_data$all_keywords$Keywords <- sapply(
  extracted_data$all_keywords$Keywords, 
  remove_stopwords
)
  
  return(extracted_data)
}

# Data validation function
validate_data <- function(data, required_columns) {
  colnames(data) <- make.unique(colnames(data))
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("[ERROR] Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  data
}



# Metric calculation function
calculate_metrics_top_authors <- function(data) {
  data$Articles <- suppressWarnings(as.numeric(data$Articles))
  if (any(is.na(data$Articles))) {
    stop("[ERROR] 'Articles' column contains non-numeric or missing values.")
  }
  # Sort and select top 10 authors
  top_authors <- data[order(-data$Articles), ]
  top_authors <- head(top_authors, 10)
  
  # Add rank information
  top_authors <- transform(top_authors, Rank = seq_len(nrow(top_authors)))
  
  return(top_authors)
}




# ---------------------------------------------------------------------------- #
# Utility Function: Calculate Gini Coefficient
# ---------------------------------------------------------------------------- #
calculate_gini <- function(cumulative_x, cumulative_y) {
  area_trapezoids <- (cumulative_x[-1] + cumulative_x[-length(cumulative_x)]) * diff(cumulative_y) / 2
  area_under_curve <- sum(area_trapezoids)
  return(1 - 2 * area_under_curve)
}





prepare_map_data <- function(data, country_col, value_col, map = world_map) {
  # Normalize country names
  data$Country <- normalize_country_names(data[[country_col]])

  # Merge with map data
  map_data <- merge(map, data, by.x = "name", by.y = "Country", all.x = TRUE)

  # Fill missing values with 0
  map_data[is.na(map_data[[value_col]]), value_col] <- 0

  return(map_data)  
}
