# ============================================================================
# data/countries/update_from_worldbank.R
# Poblar indicadores desde World Bank API
# ============================================================================

library(wbstats)
library(readr)

BASE_DIR <- "data/countries"
INDICATORS_DIR <- file.path(BASE_DIR, "countries_indicators")

cat("============================================================\n")
cat("Fetching World Bank Indicators\n")
cat("============================================================\n\n")

# Función para guardar indicador
save_indicator <- function(indicator_code, output_file) {
  cat(sprintf("  %s... ", indicator_code))
  
  tryCatch({
    data <- wb_data(indicator = indicator_code, 
                    start_date = 1960, 
                    end_date = 2024)
    
    if (!is.null(data) && nrow(data) > 0) {
      # Get value column name
      val_col <- indicator_code
      
      # Create output dataframe
      out_df <- data.frame(
        iso3 = data$iso3c,
        year = data$date,
        value = data[[val_col]]
      )
      
      # Remove NAs
      out_df <- out_df[!is.na(out_df$value) & !is.na(out_df$iso3), ]
      out_df <- out_df[order(out_df$iso3, out_df$year), ]
      rownames(out_df) <- NULL
      
      # Save
      write.csv(out_df, output_file, row.names = FALSE)
      
      n_countries <- length(unique(out_df$iso3))
      cat(sprintf("OK (%d obs, %d countries)\n", nrow(out_df), n_countries))
      return(TRUE)
    } else {
      cat("SKIPPED (no data)\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    return(FALSE)
  })
}

# Economic indicators
cat("[econ]\n")
save_indicator("NY.GDP.PCAP.PP.CD", file.path(INDICATORS_DIR, "econ", "gdp_pc_ppp.csv"))
save_indicator("NY.GDP.MKTP.KD.ZG", file.path(INDICATORS_DIR, "econ", "gdp_growth.csv"))

# Innovation indicators
cat("\n[innovation]\n")
save_indicator("GB.XPD.RSDV.GD.ZS", file.path(INDICATORS_DIR, "innovation", "rd_gdp.csv"))
save_indicator("IP.JRN.ARTC.SC", file.path(INDICATORS_DIR, "innovation", "articles.csv"))

# Human capital indicators
cat("\n[human_capital]\n")
save_indicator("SE.TER.ENRR", file.path(INDICATORS_DIR, "human_capital", "tertiary_enroll.csv"))
save_indicator("SE.XPD.TOTL.GD.ZS", file.path(INDICATORS_DIR, "human_capital", "education_gdp.csv"))

# Technology indicators
cat("\n[technology]\n")
save_indicator("IT.NET.USER.ZS", file.path(INDICATORS_DIR, "technology", "internet_users.csv"))
save_indicator("TX.VAL.TECH.CD", file.path(INDICATORS_DIR, "technology", "high_tech_exports.csv"))
save_indicator("SI.POV.GINI", file.path(INDICATORS_DIR, "technology", "gini.csv"))
save_indicator("BX.GSR.ROYL.CD", file.path(INDICATORS_DIR, "technology", "ip_receipts.csv"))
save_indicator("SP.URB.TOTL.IN.ZS", file.path(INDICATORS_DIR, "technology", "urban_pop.csv"))

cat("\n============================================================\n")
cat("World Bank fetch complete!\n")
cat("============================================================\n")
