# ============================================================================
# data/countries/fetch_all_indicators.R
# Script completo para poblar todos los indicadores de países
# ============================================================================

cat("============================================================\n")
cat("RBiblioSynth - Fetch Country Indicators\n")
cat("============================================================\n\n")

# Cargar librerías
library(wbstats)
library(countrycode)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(httr)
library(jsonlite)

# Directorio base
BASE_DIR <- "data/countries"
INDICATORS_DIR <- file.path(BASE_DIR, "countries_indicators")

cat("Directorio base:", BASE_DIR, "\n")

# ============================================================================
# 1. Poblar countries_reference.csv
# ============================================================================
cat("\n[1/8] Generando countries_reference.csv...\n")

# Obtener lista completa de países del World Bank
all_countries <- wb_cachelist$countries

# Crear dataframe de referencia
ref_data <- all_countries %>%
  select(iso2 = iso2c, iso3 = iso3c, country_name = country) %>%
  filter(!is.na(iso3), iso3 != "WLD") %>%  # Excluir aggregados
  mutate(
    continent = case_when(
      iso3 %in% c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM",
                   "CHN", "CYP", "GEO", "IND", "IDN", "IRN", "IRQ", "ISR",
                   "JPN", "JOR", "KAZ", "KWT", "KGZ", "LAO", "LBN", "MYS",
                   "MDV", "MNG", "MMR", "NPL", "PRK", "OMN", "PAK", "PHL",
                   "QAT", "SAU", "SGP", "KOR", "LKA", "SYR", "TWN", "TJK",
                   "THA", "TLS", "TUR", "TKM", "ARE", "UZB", "VNM", "YEM") ~ "Asia",
      iso3 %in% c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BGR", "HRV",
                   "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN",
                   "ISL", "IRL", "ITA", "XKX", "LVA", "LIE", "LTU", "LUX",
                   "MKD", "MLT", "MDA", "MCO", "MNE", "NLD", "NOR", "POL",
                   "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "ESP",
                   "SWE", "CHE", "UKR", "GBR", "VAT") ~ "Europe",
      iso3 %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV",
                   "CAF", "TCD", "COM", "COG", "COD", "CIV", "DJI", "EGY",
                   "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN",
                   "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI",
                   "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA",
                   "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN",
                   "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE") ~ "Africa",
      iso3 %in% c("ATG", "ARG", "BHS", "BRB", "BLZ", "BOL", "BRA", "CAN",
                   "CHL", "COL", "CRI", "CUB", "CUW", "DMA", "DOM", "ECU",
                   "SLV", "GRD", "GTM", "GUY", "HTI", "HND", "JAM", "MEX",
                   "NIC", "PAN", "PRY", "PER", "PRI", "KNA", "LCA", "MAF",
                   "VCT", "SXM", "SUR", "TTO", "TCA", "USA", "URY", "VEN",
                   "VIR", "ABW", "AIA", "BMU", "CYM", "CUB", "GLP", "GUF",
                   "MTQ", "SMS", "VGB") ~ "Americas",
      iso3 %in% c("AUS", "FJI", "PYF", "GUM", "KIR", "MHL", "FSM", "NRU",
                   "NCL", "NZL", "MNP", "PLW", "PNG", "WSM", "SLC", "TLS",
                   "TON", "TUV", "VUT") ~ "Oceania",
      TRUE ~ "Unknown"
    ),
    region = case_when(
      iso3 %in% c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK") ~ "Southern Asia",
      iso3 %in% c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM") ~ "South-Eastern Asia",
      iso3 %in% c("CHN", "HKG", "JPN", "PRK", "KOR", "MNG", "TWN") ~ "Eastern Asia",
      iso3 %in% c("AFG", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "OMN", "PSE", "QAT", "SAU", "SYR", "TUR", "ARE", "YEM") ~ "Western Asia",
      iso3 %in% c("KAZ", "KGZ", "TJK", "TKM", "UZB") ~ "Central Asia",
      iso3 %in% c("USA", "CAN") ~ "Northern America",
      iso3 %in% c("MEX", "GTM", "BLZ", "HND", "SLV", "NIC", "CRI", "PAN") ~ "Central America",
      iso3 %in% c("COL", "VEN", "ECU", "PER", "BOL", "BRA", "CHL", "ARG", "PRY", "URY") ~ "South America",
      iso3 %in% c("JPN", "KOR") ~ "Eastern Asia",
      iso3 %in% c("DEU", "FRA", "GBR", "ITA", "ESP", "NLD", "BEL", "AUT", "CHE", "PRT", "IRL", "NOR", "SWE", "DNK", "FIN", "POL", "CZE", "AUT") ~ "Western Europe",
      iso3 %in% c("RUS", "UKR", "BLR", "MDA", "GEO", "ARM", "AZE", "KAZ") ~ "Eastern Europe",
      TRUE ~ continent
    ),
    income_group = NA_character_
  )

# Obtener income group desde World Bank
cat("   Obteniendo income groups...\n")
income_data <- tryCatch({
  wb_data(indicator = "NY.GDP.PCAP.PP.CD", start_date = 2023, end_date = 2023) %>%
    mutate(income = "Not classified") %>%
    select(iso3 = iso3c, income)
}, error = function(e) NULL)

# Obtener regiones y income desde API directa
cat("   Fetching country metadata from World Bank API...\n")
country_meta <- tryCatch({
  url <- "https://api.worldbank.org/v2/country?format=json&per_page=300"
  resp <- GET(url)
  data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  
  # data[[2]] contains the country records
  records <- lapply(data[[2]], function(x) {
    list(
      iso3 = as.character(x$id),
      income_group = if (!is.null(x$incomeLevel) && !is.na(x$incomeLevel$value[1])) 
        as.character(x$incomeLevel$value) else NA_character_,
      region = if (!is.null(x$region) && !is.na(x$region$value[1])) 
        as.character(x$region$value) else NA_character_
    )
  })
  
  df <- bind_rows(records)
  df
}, error = function(e) {
  cat("API error:", e$message, "\n")
  NULL
})

# Mergear income group
if (!is.null(country_meta) && nrow(country_meta) > 0) {
  names(country_meta) <- c("iso3", "income_group_wb", "region_wb")
  
  ref_data <- ref_data %>%
    left_join(country_meta, by = "iso3") %>%
    mutate(
      income_group = ifelse(is.na(income_group) & !is.na(income_group_wb), income_group_wb, income_group),
      region = ifelse(is.na(region) & !is.na(region_wb), region_wb, region)
    ) %>%
    select(-income_group_wb, -region_wb)
}

# Limpiar nombres
ref_data <- ref_data %>%
  mutate(country_name = trimws(gsub("\\s*\\(.*?\\)\\s*", "", country_name)))

# Guardar
write_csv(ref_data, file.path(BASE_DIR, "countries_reference.csv"))
cat("   OK: countries_reference.csv (", nrow(ref_data), " países)\n")

# ============================================================================
# 2. World Bank WDI Indicators
# ============================================================================
cat("\n[2/8] Fetching World Bank WDI indicators...\n")

wdi_indicators <- list(
  gdp_pc_ppp       = "NY.GDP.PCAP.PP.CD",
  gdp_growth       = "NY.GDP.MKTP.KD.ZG",
  internet_users   = "IT.NET.USER.ZS",
  urban_pop        = "SP.URB.TOTL.IN.ZS",
  high_tech_exports = "TX.VAL.TECH.CD"
)

for (nm in names(wdi_indicators)) {
  cat("   - ", nm, "... ")
  tryCatch({
    data <- wb_data(indicator = wdi_indicators[[nm]], start_date = 1960, end_date = 2024)
    
    if (!is.null(data) && nrow(data) > 0) {
      out_file <- file.path(INDICATORS_DIR, "econ", paste0(nm, ".csv"))
      
      data %>%
        select(iso3 = iso3c, year = date, value = !!sym(wdi_indicators[[nm]])) %>%
        filter(!is.na(value), !is.na(iso3)) %>%
        arrange(iso3, year) %>%
        write_csv(out_file)
      
      cat("OK (", nrow(data), " obs)\n")
    }
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
}

# ============================================================================
# 3. UNESCO UIS Indicators via API
# ============================================================================
cat("\n[3/8] Fetching UNESCO UIS indicators...\n")

# UNESCO API - indicador codes
unesco_indicators <- list(
  rd_gdp          = list(code = "RD_GERD_GDP", subdir = "innovation"),
  researchers_pm   = list(code = "RP_RSRS", subdir = "innovation"),
  articles         = list(code = "BS_SCI_SCIE", subdir = "innovation"),
  tertiary_enroll  = list(code = "SE_TER_ENRR", subdir = "human_capital"),
  education_gdp    = list(code = "SE.XPD.TOTL.GD.ZS", subdir = "human_capital")
)

fetch_unesco <- function(indicator_code, start_year = 1970, end_year = 2023) {
  base_url <- "http://api.uis.unesco.org/rest/data/UNESCO,DF_UNDATA_UIS,1.0"
  
  url <- paste0(base_url, "/", indicator_code, "?startTime=", start_year, "&endTime=", end_year)
  
  tryCatch({
    resp <- GET(url, add_headers("Accept" = "application/json"))
    
    if (status_code(resp) == 200) {
      content <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
      
      if (!is.null(content$data) && length(content$data) > 0) {
        records <- lapply(content$data, function(x) {
          if (!is.null(x$value)) {
            data.frame(
              iso3 = toupper(x$geo[[1]]),
              year = as.integer(x$time),
              value = as.numeric(x$value),
              stringsAsFactors = FALSE
            )
          }
        })
        
        df <- bind_rows(records) %>% filter(!is.na(value))
        return(df)
      }
    }
    return(NULL)
  }, error = function(e) {
    cat("API error:", e$message, "\n")
    return(NULL)
  })
}

for (nm in names(unesco_indicators)) {
  cat("   - ", nm, "... ")
  
  df <- fetch_unesco(unesco_indicators[[nm]]$code)
  
  if (!is.null(df) && nrow(df) > 0) {
    out_file <- file.path(INDICATORS_DIR, unesco_indicators[[nm]]$subdir, paste0(nm, ".csv"))
    df %>% arrange(iso3, year) %>% write_csv(out_file)
    cat("OK (", nrow(df), " obs)\n")
  } else {
    cat("SKIPPED (no data)\n")
  }
}

# ============================================================================
# 4. WIPO Patents
# ============================================================================
cat("\n[4/8] Fetching WIPO patents...\n")

# WIPO Statistics API
wipo_base <- "https://www.wipo.int/ipstats/rest/"

# Patent applications by country
cat("   - patents_res... ")
tryCatch({
  url <- paste0(wipo_base, "countrypatentdata?year_start=1980&year_end=2023")
  resp <- GET(url, add_headers("Accept" = "application/json"))
  
  if (status_code(resp) == 200) {
    content <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    
    if (!is.null(content$data) && length(content$data) > 0) {
      df <- lapply(content$data, function(x) {
        data.frame(
          iso3 = toupper(x$country_code),
          year = as.integer(x$year),
          value = as.numeric(x$patent_count),
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% filter(!is.na(value))
      
      write_csv(df, file.path(INDICATORS_DIR, "innovation", "patents_res.csv"))
      cat("OK (", nrow(df), " obs)\n")
    }
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# PCT applications
cat("   - patents_pct... ")
tryCatch({
  url <- paste0(wipo_base, "pctcountrypctdata?year_start=1990&year_end=2023")
  resp <- GET(url, add_headers("Accept" = "application/json"))
  
  if (status_code(resp) == 200) {
    content <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    
    if (!is.null(content$data) && length(content$data) > 0) {
      df <- lapply(content$data, function(x) {
        data.frame(
          iso3 = toupper(x$country_code),
          year = as.integer(x$year),
          value = as.numeric(x$pct_count),
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% filter(!is.na(value))
      
      write_csv(df, file.path(INDICATORS_DIR, "innovation", "patents_pct.csv"))
      cat("OK (", nrow(df), " obs)\n")
    }
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# ============================================================================
# 5. UNDP HDI
# ============================================================================
cat("\n[5/8] Fetching UNDP HDI...\n")

cat("   - hdi... ")
tryCatch({
  # HDI data from HDR website
  url <- "https://hdr.undp.org/sites/default/files/data/indicators/hdr23-compositeIndices.csv"
  
  hdi_raw <- read_csv(url)
  
  if (!is.null(hdi_raw) && nrow(hdi_raw) > 0) {
    hdi <- hdi_raw %>%
      filter(indicator == "Human Development Index (HDI)") %>%
      select(iso3 = iso3, year = year, value = value) %>%
      mutate(iso3 = toupper(iso3)) %>%
      filter(!is.na(value), !is.na(iso3))
    
    write_csv(hdi, file.path(INDICATORS_DIR, "human_capital", "hdi.csv"))
    cat("OK (", nrow(hdi), " obs)\n")
  }
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("   Creando HDI placeholder...\n")
  # Crear placeholder si falla
  write_csv(data.frame(iso3 = character(), year = integer(), value = double()),
            file.path(INDICATORS_DIR, "human_capital", "hdi.csv"))
})

# ============================================================================
# 6. Additional indicators (gerd, rd_personnel, etc.)
# ============================================================================
cat("\n[6/8] Fetching additional innovation indicators...\n")

# GERD from UNESCO (using alternative approach if API fails)
cat("   - gerd... ")
tryCatch({
  url <- "http://api.uis.unesco.org/rest/data/UNESCO,DF_UNDATA_UIS,1.0/RD_GERD_TOTL?startTime=1970&endTime=2023"
  resp <- GET(url, add_headers("Accept" = "application/json"))
  
  if (status_code(resp) == 200) {
    content <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    
    if (!is.null(content$data) && length(content$data) > 0) {
      df <- lapply(content$data, function(x) {
        data.frame(
          iso3 = toupper(x$geo[[1]]),
          year = as.integer(x$time),
          value = as.numeric(x$value),
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% filter(!is.na(value))
      
      write_csv(df, file.path(INDICATORS_DIR, "innovation", "gerd.csv"))
      cat("OK (", nrow(df), " obs)\n")
    }
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# R&D personnel
cat("   - rd_personnel_pm... ")
tryCatch({
  url <- "http://api.uis.unesco.org/rest/data/UNESCO,DF_UNDATA_UIS,1.0/RP_RSSE?startTime=1990&endTime=2023"
  resp <- GET(url, add_headers("Accept" = "application/json"))
  
  if (status_code(resp) == 200) {
    content <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    
    if (!is.null(content$data) && length(content$data) > 0) {
      df <- lapply(content$data, function(x) {
        data.frame(
          iso3 = toupper(x$geo[[1]]),
          year = as.integer(x$time),
          value = as.numeric(x$value),
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% filter(!is.na(value))
      
      write_csv(df, file.path(INDICATORS_DIR, "innovation", "rd_personnel_pm.csv"))
      cat("OK (", nrow(df), " obs)\n")
    }
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# Business R&D
cat("   - rd_business... ")
tryCatch({
  url <- "http://api.uis.unesco.org/rest/data/UNESCO,DF_UNDATA_UIS,1.0/RD_GERD_BV?startTime=1990&endTime=2023"
  resp <- GET(url, add_headers("Accept" = "application/json"))
  
  if (status_code(resp) == 200) {
    content <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    
    if (!is.null(content$data) && length(content$data) > 0) {
      df <- lapply(content$data, function(x) {
        data.frame(
          iso3 = toupper(x$geo[[1]]),
          year = as.integer(x$time),
          value = as.numeric(x$value),
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% filter(!is.na(value))
      
      write_csv(df, file.path(INDICATORS_DIR, "innovation", "rd_business.csv"))
      cat("OK (", nrow(df), " obs)\n")
    }
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# ============================================================================
# 7. Technology indicators
# ============================================================================
cat("\n[7/8] Fetching technology indicators...\n")

# Gini from World Bank
cat("   - gini... ")
tryCatch({
  gini_data <- wb_data(indicator = "SI.POV.GINI", start_date = 1985, end_date = 2023)
  
  if (!is.null(gini_data) && nrow(gini_data) > 0) {
    gini <- gini_data %>%
      select(iso3 = iso3c, year = date, value = SI.POV.GINI) %>%
      filter(!is.na(value))
    
    write_csv(gini, file.path(INDICATORS_DIR, "technology", "gini.csv"))
    cat("OK (", nrow(gini), " obs)\n")
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# IP royalties
cat("   - ip_receipts... ")
tryCatch({
  ip_data <- wb_data(indicator = "BX.GSR.ROYL.CD", start_date = 1970, end_date = 2023)
  
  if (!is.null(ip_data) && nrow(ip_data) > 0) {
    ip <- ip_data %>%
      select(iso3 = iso3c, year = date, value = BX.GSR.ROYL.CD) %>%
      filter(!is.na(value))
    
    write_csv(ip, file.path(INDICATORS_DIR, "technology", "ip_receipts.csv"))
    cat("OK (", nrow(ip), " obs)\n")
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# Manufacturing exports
cat("   - mfg_exports... ")
tryCatch({
  mfg_data <- wb_data(indicator = "TX.VAL.MANU.ZS", start_date = 1970, end_date = 2023)
  
  if (!is.null(mfg_data) && nrow(mfg_data) > 0) {
    mfg <- mfg_data %>%
      select(iso3 = iso3c, year = date, value = TX.VAL.MANU.ZS) %>%
      filter(!is.na(value))
    
    # Placeholder file
    write_csv(mfg, file.path(INDICATORS_DIR, "technology", "urban_pop.csv"))
    cat("OK (", nrow(mfg), " obs)\n")
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# ============================================================================
# 8. UN Population Prospects (projections)
# ============================================================================
cat("\n[8/8] Fetching UN Population projections...\n")

cat("   - population_forecast... ")
tryCatch({
  # UN Population Prospects
  url <- "https://population.un.org/wpp/Download/Files/1_Population%20Data/CSV/WPP2024_TotalPopulation.csv"
  
  pop_raw <- read_csv(url)
  
  if (!is.null(pop_raw) && nrow(pop_raw) > 0) {
    pop <- pop_raw %>%
      filter(Varient == "Medium") %>%
      select(iso3 = ISO3_code, year = Time, value = PopTotal) %>%
      filter(!is.na(iso3), year >= 2025) %>%
      mutate(value = as.numeric(value) * 1000) %>%
      filter(!is.na(value))
    
    write_csv(pop, file.path(INDICATORS_DIR, "projections", "population_forecast.csv"))
    cat("OK (", nrow(pop), " obs)\n")
  }
}, error = function(e) cat("ERROR:", e$message, "\n"))

# ============================================================================
# Summary
# ============================================================================
cat("\n============================================================\n")
cat("Fetch complete!\n")
cat("============================================================\n\n")

# Listar archivos generados
cat("Archivos generados:\n")
indicator_files <- list.files(INDICATORS_DIR, recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
for (f in indicator_files) {
  df <- read_csv(f, show_col_types = FALSE)
  n <- nrow(df)
  cat(sprintf("  %-60s %6d obs\n", f, n))
}

cat("\n")
