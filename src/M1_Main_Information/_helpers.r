# ---------------------------------------------------------------------------- #
# Function: Save Plot
# ---------------------------------------------------------------------------- #
save_plot <- function(plot, filename_prefix, width = 4, height = 3, dpi = 300, aspect_ratio = NULL) {

  # Define the output directory
  output_dir <- "results/M1_Main_Information/figures"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Adjust height based on aspect ratio if specified
  if (!is.null(aspect_ratio)) {
    height <- width * aspect_ratio
  }

  # Save as PNG
  tryCatch({
    ggsave(
      filename = file.path(output_dir, paste0(filename_prefix, "_PNG.png")),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      device = "png"
    )
    message("[INFO] PNG plot saved: ", file.path(output_dir, paste0(filename_prefix, "_PNG.png")))
  }, error = function(e) {
    warning("[WARNING] Failed to save PNG plot: ", e$message)
  })

  # Save as SVG
  tryCatch({
    ggsave(
      filename = file.path(output_dir, paste0(filename_prefix, "_SVG.svg")),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      device = "svg"
    )
    message("[INFO] SVG plot saved: ", file.path(output_dir, paste0(filename_prefix, "_SVG.svg")))
  }, error = function(e) {
    warning("[WARNING] Failed to save SVG plot: ", e$message)
  })

  # Save as EPS
  tryCatch({
    ggsave(
      filename = file.path(output_dir, paste0(filename_prefix, "_EPS.eps")),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      device = "eps"
    )
    message("[INFO] EPS plot saved: ", file.path(output_dir, paste0(filename_prefix, "_EPS.eps")))
  }, error = function(e) {
    warning("[WARNING] Failed to save EPS plot: ", e$message)
  })
}





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
preprocess_data <- function(data) {
  colnames(data) <- make.unique(colnames(data))
  data$Articles <- suppressWarnings(as.numeric(data$Articles))
  if (any(is.na(data$Articles))) {
    stop("[ERROR] 'Articles' column contains non-numeric or missing values.")
  }
  data$Country <- normalize_country_names(data$Country)
  return(data)
}



# Normalize country names to match map dataset
normalize_country_names <- function(country) {
  dplyr::case_when(
    country == "USA" ~ "United States",
    country == "KOREA" ~ "South Korea",
    country == "UNITED KINGDOM" ~ "United Kingdom",
    TRUE ~ country  # Default: keep as is
  )
}

# Get top N countries by articles
get_top_countries <- function(data, top_n = 10) {
  top_countries <- data[order(-data$Articles), ]
  top_countries <- head(top_countries, top_n)
  top_countries$cumulative_percentage <- cumsum(top_countries$Articles) / sum(top_countries$Articles)
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
  author_keywords <- res1$DE
  keywords_plus <- res1$ID
  all_keywords <- unique(c(author_keywords, keywords_plus))

  # Extract Titles
  titles <- bib_data$TI

  # Extract Descriptions/Abstracts
  descriptions <- bib_data$AB

  # Combine into a structured list
  extracted_data <- list(
    keywords = all_keywords,
    titles = titles,
    descriptions = descriptions
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


preprocess_data <- function(data, required_columns) {
  # Ensure unique column names
  colnames(data) <- make.unique(colnames(data))

  # Validate required columns
  missing_cols <- setdiff(required_columns, colnames(data))
  if (length(missing_cols) > 0) {
    stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Convert necessary columns to numeric
  data$Articles <- suppressWarnings(as.numeric(data$Articles))
  data <- data[complete.cases(data), ]

  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning.")
  }

  return(data)
}


get_top_countries <- function(data, top_n = 10) {
  top_countries <- data[order(-data$Articles), ]
  top_countries <- head(top_countries, top_n)

  # Calculate cumulative percentages for threshold functionality
  top_countries$cumulative_percentage <- cumsum(top_countries$Articles) / sum(top_countries$Articles)
  return(top_countries)
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
