# ============================================================================
# data/countries/fetch_oecd_indicators.R
# Fetch OECD data for R&D and researchers
# ============================================================================

library(httr)
library(jsonlite)
library(readr)

INDICATORS_DIR <- "data/countries/countries_indicators"

cat("Fetching OECD data...\n\n")

# OECD has R&D data - let's try their SDMX endpoint
oecd_base <- "https://sdmx.oecd.org/public/rest/data/"

# OECD countries dataset for R&D
# GERD as % of GDP
cat("[1] GERD (% GDP) from OECD...\n")
tryCatch({
  url <- paste0(oecd_base, "OECD.SSTI,NAD_SNA2008,1.0/GDE.GD+GDP.GD.BV.Q/all")
  resp <- GET(url, add_headers(Accept = "application/json"))
  
  if(status_code(resp) == 200) {
    data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    cat("  OECD data structure received\n")
    cat("  Status: OK (OECD API complex - data needs mapping)\n")
  } else {
    cat("  Status:", status_code(resp), "\n")
  }
}, error = function(e) cat("  ERROR:", e$message, "\n"))

# Try to get researchers from OECD
cat("[2] Researchers from OECD...\n")
tryCatch({
  # OECD researchers in R&D
  url <- paste0(oecd_base, "OECD.SSTI,NAD_SNA2008,1.0/HRSTC.RD+PERS.RD+BUS.RD+Gov.RD.all/all")
  resp <- GET(url, add_headers(Accept = "application/json"))
  
  if(status_code(resp) == 200) {
    data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    cat("  OECD data received\n")
  }
}, error = function(e) cat("  ERROR:", e$message, "\n"))

cat("\nNote: OECD data requires complex mapping. Using World Bank data as primary source.\n")
