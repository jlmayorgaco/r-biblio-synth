extract_country_data <- function(converted_data) {

    library(dplyr)
library(purrr)
library(stringr)

  # Define column mapping for better readability (optional)
  column_mapping <- c(
    "AU" = "Authors", "DE" = "Keywords", "ID" = "Index Terms", "C1" = "Author Affiliations",
    "JI" = "Journal Abbreviation", "AB" = "Abstract", "RP" = "Corresponding Author", "DI" = "DOI",
    "SN" = "ISSN", "SO" = "Source Title", "LA" = "Language", "TC" = "Times Cited", 
    "PN" = "Page Number", "PU" = "Publisher", "DB" = "Database", "TI" = "Title", 
    "DT" = "Document Type", "VL" = "Volume", "PY" = "Year", "AF" = "Full Author Names", 
    "J9" = "Journal Name", "AU_UN" = "Author University", "SR_FULL" = "Full Reference", 
    "AU_CO" = "Country"
  )

  country_acronyms <- c(
  "UNITED STATES" = "USA",
  "UNITED KINGDOM" = "UK",
  "PEOPLE'S REPUBLIC OF CHINA" = "CHINA",
  "SOUTH KOREA" = "KOREA",
  "REPUBLIC OF KOREA" = "KOREA",
  "GERMAN DEMOCRATIC REPUBLIC" = "GERMANY",
  "FEDERAL REPUBLIC OF GERMANY" = "GERMANY",
  "RUSSIAN FEDERATION" = "RUSSIA",
  "IRAN (ISLAMIC REPUBLIC OF)" = "IRAN"
)

  # Extract countries from bibliometrix metadata
  extracted <- bibliometrix::metaTagExtraction(converted_data, Field = "AU_CO")
  # Rename columns using mapping (optional but clean)
  names(extracted) <- ifelse(names(extracted) %in% names(column_mapping),
                             column_mapping[names(extracted)],
                             names(extracted))


    # Limpieza de la columna Country
extracted <- extracted %>%
  mutate(Country = purrr::map_chr(Country, function(countries_str) {
    countries <- stringr::str_split(countries_str, ";")[[1]]
    countries <- trimws(countries)
    countries <- dplyr::recode(countries, !!!country_acronyms)
    paste(countries, collapse = ";")
  }))
  message(' ')
  message(' ')
  message(' ')
  message(' ON extract_country_data ')
  message(' ')
  message(' ')
  message(' ')
  message(' ')

    message(' ')
  message(' ')
  message(' ')
  message(' =====================> extracted <- bibliometrix::metaTagExtraction(converted_data ')
  message(' ')
  print(colnames(extracted))
  message(' ')
    message(' ')
message("Country (first row): ", extracted %>% pull(Country) %>% .[1])
  message(' ')


  # Split authors and countries for multi-author papers
  extracted$Authors_Array <- strsplit(extracted$Authors, ";")
  extracted$Country_Array <- strsplit(extracted$Country, ";")

  message(' END OF EXTRACTED')

  return(extracted)
}
