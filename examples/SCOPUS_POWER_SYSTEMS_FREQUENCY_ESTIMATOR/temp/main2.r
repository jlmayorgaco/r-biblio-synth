# ===============================================================
# Title: Scopus BibTeX Loader and Preprocessor
# Author: Jorge Luis Mayorga Taborda
# Date: 2025-02-13
# Description: 
# This script defines an R class `ScopusBibLoader` that loads, cleans,
# and preprocesses a .bib file exported from Scopus. The dataset is
# transformed to ensure high-quality bibliometric analysis, converting
# authors into lists and extracting countries using bibliometrix.
#
# Dependencies:
# - bib2df (for loading BibTeX files)
# - dplyr (for data manipulation)
# - janitor (for cleaning column names)
# - stringr (for string manipulation)
# - bibliometrix (for extracting countries)
#
# ===============================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))


install.packages("bib2df", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)
install.packages("janitor", dependencies = TRUE)
install.packages("purrr", dependencies = TRUE)
install.packages("bibliometrix", dependencies = TRUE)
install.packages("jsonlite")
install.packages("tidyr")



# Load necessary libraries
library(bib2df)       # For loading .bib files into data frames
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(janitor)      # For cleaning column names
library(purrr)      # For cleaning column names
library(bibliometrix) # For extracting countries from affiliations
library(jsonlite)
library(tidyr)

# ===============================================================
# ScopusBibLoader Class
# Description:
# This class handles the loading, cleaning, and processing of Scopus .bib files.
# It provides a method to load the .bib file, clean it, and extract authors and
# countries into structured lists.
# ===============================================================

# Utility function to extract countries from affiliations
extractCountries <- function(affiliations) {
  country_list <- c("China", "USA", "Germany", "France", "UK", "Japan", "Italy", "Spain", "Australia", "India", "Brazil")
  countries <- affiliations[affiliations %in% country_list]
  return(unique(countries))
}


ScopusBibLoader <- setRefClass(
  "ScopusBibLoader",
  fields = list(file_path = "character"),
  
  methods = list(
    
    # -----------------------------------------------------------
    # Constructor Method: initialize
    # Description:
    # Initializes the ScopusBibLoader class with a file path.
    # Parameters:
    # - file_path: The path to the .bib file (default: "./data/scopus.bib")
    # -----------------------------------------------------------
    initialize = function(file_path = "./data/scopus.bib") {
      .self$file_path <- file_path
    },
    
    # -----------------------------------------------------------
    # Method: loadBibFile
    # Description:
    # Loads the .bib file using the bib2df package and returns it as a data frame.
    # Returns:
    # - A data frame containing the parsed BibTeX data.
    # -----------------------------------------------------------
    loadBibFile = function() {
      message("Loading .bib file...")
      bib_data <- bib2df(.self$file_path)
      message("File loaded successfully.")
      return(bib_data)
    },
    
    # -----------------------------------------------------------
    # Method: cleanData
    # Description:
    # Cleans and preprocesses the loaded .bib data.
    # - Converts the authors column into a list of authors.
    # - Extracts a list of countries from affiliations using bibliometrix.
    # Parameters:
    # - bib_data: The data frame loaded from the .bib file.
    # Returns:
    # - A cleaned and processed data frame.
    # -----------------------------------------------------------
    cleanData = function() {
  message("Cleaning and preprocessing data...")
  
  # Convert the .bib file to bibliometrix format
  converted_data <- tryCatch(
    bibliometrix::convert2df(file = .self$file_path, dbsource = "scopus", format = "bibtex"),
    error = function(e) {
      stop("Error converting .bib file: ", e$message)
    }
  )


  # Create a mapping vector
column_mapping <- c(
  "AU" = "Authors",
  "DE" = "Keywords",
  "ID" = "Index Terms",
  "C1" = "Author Affiliations",
  "JI" = "Journal Abbreviation",
  "AB" = "Abstract",
  "RP" = "Corresponding Author",
  "DI" = "DOI",
  "SN" = "ISSN",
  "SO" = "Source Title",
  "LA" = "Language",
  "TC" = "Times Cited",
  "PN" = "Page Number",
  "PU" = "Publisher",
  "DB" = "Database",
  "TI" = "Title",
  "DT" = "Document Type",
  "VL" = "Volume",
  "PY" = "Publication Year",
  "AF" = "Full Author Names",
  "J9" = "Journal Name",
  "AU_UN" = "Author University",
  "SR_FULL" = "Full Reference",
  "AU_CO" = "Countries"
)

  
    # Extract country information using metaTagExtraction
    data <- bibliometrix::metaTagExtraction(converted_data, "AU_CO")

        unmapped_columns <- setdiff(names(data), names(column_mapping))
        if (length(unmapped_columns) > 0) {
        message("Unmapped columns found: ", paste(unmapped_columns, collapse = ", "))
        }

        # Apply the column mapping
        names(data) <- ifelse(names(data) %in% names(column_mapping), column_mapping[names(data)], names(data))


# Split Authors into an array
data$Authors_Array <- strsplit(data$Authors, ";")

# Split Countries into an array
data$Countries_Array <- strsplit(data$Countries, ";")

# Verify the changes
head(data[, c("Authors", "Authors_Array", "Countries", "Countries_Array")])

# Optional: check for any unmapped columns again after transformation
unmapped_columns <- setdiff(names(data), names(column_mapping))
if (length(unmapped_columns) > 0) {
  message("Unmapped columns found: ", paste(unmapped_columns, collapse = ", "))
}
    message("Data cleaned successfully.")

    new_df <- data[, c("Publication Year", "Countries_Array", "Times Cited")]
    # Calculate the half of the year range
    year_range <- range(new_df$`Publication Year`, na.rm = TRUE)
    mid_year <- floor(mean(year_range))

    # Split the DataFrame into two based on the midpoint year
    df_first_half <- new_df[new_df$`Publication Year` <= mid_year, ]
    df_second_half <- new_df[new_df$`Publication Year` > mid_year, ]

    # Group by country and calculate Total Paper Count and Total Citations

        # Step 1: Create SCP and MCP indicators before unnesting
        df_first_half <- df_first_half %>%
        mutate(
            SCP = if_else(lengths(Countries_Array) == 1, 1, 0), # 1 if it's SCP, 0 otherwise
            MCP = if_else(lengths(Countries_Array) > 1, 1, 0)  # 1 if it's MCP, 0 otherwise
        )

        # Step 2: Unnest the Countries_Array
        df_first_half_long <- df_first_half %>%
        unnest(Countries_Array)

        # Step 3: Group by country and calculate total paper count, total citations, SCP count, and MCP count
        df_summary <- df_first_half_long %>%
        group_by(Countries_Array) %>%
        summarise(
            Total_Paper_Count = n(),                       # Total number of papers
            Total_Citations = sum(`Times Cited`, na.rm = TRUE), # Total citations
            SCP_Count = sum(SCP, na.rm = TRUE),            # Total SCP count
            MCP_Count = sum(MCP, na.rm = TRUE)             # Total MCP count
        ) %>%
        ungroup()

    return(df_summary)
},
    
    # -----------------------------------------------------------
    # Method: getProcessedData
    # Description:
    # Main method to load, clean, and return the processed data.
    # Returns:
    # - A cleaned data frame with authors and countries in list format.
    # -----------------------------------------------------------
    getProcessedData = function() {
      bib_data <- .self$loadBibFile()          # Load the .bib file
      processed_data <- .self$cleanData()  # No need to pass bib_data
      return(processed_data)
    }
  )
)

# ===============================================================
# Main Execution
# Instantiate the ScopusBibLoader class and get the processed data
# ===============================================================
scopus_loader <- ScopusBibLoader$new(file_path = "./data/scopus.bib") # Create an instance
scopus_dataset <- scopus_loader$getProcessedData()                    # Load and clean data

# Print the first few rows of the processed dataset
print(head(scopus_dataset))


# Save the first 10 rows as a JSON file
output_file <- "./scopus_dataset.json"
# Extract the first 10 rows and convert to JSON
json_data <- jsonlite::toJSON(scopus_dataset[1:10, ], pretty = TRUE)
# Write the JSON data to a file
write(json_data, file = output_file)
message("Dataset saved as JSON at: ", output_file)

# ===============================================================
# End of Script
# ===============================================================
