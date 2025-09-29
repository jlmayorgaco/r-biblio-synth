suppressPackageStartupMessages({
  library(dplyr)
  library(countrycode)
  library(readr)
})

# Read properly with semicolon separator
countries <- read_delim("countries.csv", 
                        delim = ";", 
                        col_names = TRUE, 
                        trim_ws = TRUE)

# Check the column names
print(names(countries))

# Normalize names
names(countries) <- toupper(names(countries))

# Add metadata
countries_complete <- countries %>%
  mutate(
    ISO3      = countrycode(NAME, "country.name", "iso3c"),
    CONTINENT = countrycode(NAME, "country.name", "continent"),
    REGION    = countrycode(NAME, "country.name", "region")
  )

# Save enriched file
write_delim(countries_complete, "countries_complete.csv", delim = ";")

message("[INFO] countries_complete.csv generated with ", 
        nrow(countries_complete), " rows.")
