# ============================================================================
# data/countries/update_reference.R
# Update countries_reference.csv with World Bank data
# ============================================================================

library(wbstats)
library(countrycode)
library(readr)

BASE_DIR <- "data/countries"
ref_file <- file.path(BASE_DIR, "countries_reference.csv")

cat("Updating countries_reference.csv...\n\n")

# Read current reference
ref <- read_csv(ref_file)

cat("Current reference: ", nrow(ref), " rows\n")

# Fetch income groups from World Bank API
cat("\nFetching income groups...\n")
tryCatch({
  url <- "https://api.worldbank.org/v2/country?format=json&per_page=300"
  resp <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  
  # Extract country data
  country_data <- data[[2]]
  
  income_map <- setNames(country_data$incomeLevel$value, country_data$id)
  
  # Update income_group
  ref$income_group <- income_map[ref$iso3]
  
  cat("  Income groups updated\n")
}, error = function(e) cat("  ERROR:", e$message, "\n"))

# Fetch coordinates from World Bank
cat("\nFetching coordinates...\n")
tryCatch({
  url <- "https://api.worldbank.org/v2/country?format=json&per_page=300"
  resp <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  
  country_data <- data[[2]]
  
  lat_map <- setNames(country_data$latitude, country_data$id)
  lon_map <- setNames(country_data$longitude, country_data$id)
  
  ref$latitude <- lat_map[ref$iso3]
  ref$longitude <- lon_map[ref$iso3]
  
  cat("  Coordinates updated\n")
}, error = function(e) cat("  ERROR:", e$message, "\n"))

# Fetch population (most recent)
cat("\nFetching population...\n")
tryCatch({
  pop_data <- wb_data(
    indicator = "SP.POP.TOTL",
    start_date = 2020,
    end_date = 2023,
    return_wide = FALSE
  )
  
  # Get most recent non-NA value per country
  pop_latest <- pop_data %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarize(population = max(SP.POP.TOTL[!is.na(SP.POP.TOTL)], na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  names(pop_latest) <- c("iso3", "population")
  
  ref <- ref %>%
    dplyr::left_join(pop_latest, by = "iso3")
  
  ref$population[is.infinite(ref$population)] <- NA
  
  cat("  Population updated\n")
}, error = function(e) cat("  ERROR:", e$message, "\n"))

# Fetch area from World Bank (if available)
cat("\nFetching area...\n")
tryCatch({
  area_data <- wb_data(
    indicator = "AG.SRF.TOTL.K2",
    start_date = 2000,
    end_date = 2023
  )
  
  area_latest <- area_data %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarize(area_km2 = max(AG.SRF.TOTL.K2[!is.na(AG.SRF.TOTL.K2)], na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  names(area_latest) <- c("iso3", "area_km2")
  
  ref <- ref %>%
    dplyr::left_join(area_latest, by = "iso3")
  
  ref$area_km2[is.infinite(ref$area_km2)] <- NA
  
  cat("  Area updated\n")
}, error = function(e) cat("  ERROR:", e$message, "\n"))

# Clean up
ref <- ref %>%
  dplyr::mutate(
    country_name = trimws(gsub("\\s*\\(.*?\\)\\s*", "", country_name)),
    continent = ifelse(continent == "Unknown", NA, continent),
    region = ifelse(region == "Unknown", NA, region)
  )

# Save
write_csv(ref, ref_file)

cat("\n========================================\n")
cat("Updated countries_reference.csv\n")
cat(nrow(ref), "countries\n")
cat("Columns:", paste(names(ref), collapse = ", "), "\n")
cat("========================================\n")

# Show sample
cat("\nSample data:\n")
print(head(ref, 5))
