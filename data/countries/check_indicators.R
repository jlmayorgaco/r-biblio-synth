library(wbstats)

cat("Checking World Bank indicator coverage:\n\n")

indicators <- list(
  "R&D (% GDP)" = "GB.XPD.RSDV.GD.ZS",
  "Researchers per million" = "SP.POP.SCIE.RD.P6",
  "Journal articles" = "IP.JRN.ARTC.SC",
  "Tertiary enrollment" = "SE.TER.ENRR",
  "Education spending" = "SE.XPD.TOTL.GB.ZS"
)

for(nm in names(indicators)) {
  tryCatch({
    d <- wb_data(indicators[[nm]], start_date=1990, end_date=2023)
    val_col <- names(d)[3]
    non_na <- sum(!is.na(d[[val_col]]))
    n_countries <- length(unique(d$iso3c[!is.na(d[[val_col]])]))
    cat(sprintf("%-25s: %5d obs, %4d countries, %4d non-NA\n", 
                nm, nrow(d), n_countries, non_na))
  }, error = function(e) {
    cat(sprintf("%-25s: ERROR - %s\n", nm, e$message))
  })
}
