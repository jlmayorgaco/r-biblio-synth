library(bibliometrix)

# Source the M3 prepare function
source("C:/Users/walla/Documents/Github/r-biblio-synth/R/module_m3/compute/m3_prepare_country_data.R")

bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)

# Try to run prepare_m3_country_data
writeLines("Running prepare_m3_country_data...")
result <- tryCatch({
  prepare_m3_country_data(bib_data)
}, error = function(e) {
  writeLines(paste("ERROR:", e$message))
  writeLines(paste("Type:", class(e)))
  NULL
})

if (!is.null(result)) {
  writeLines("Success!")
  writeLines(paste("Status:", result$status))
  writeLines(paste("Country summary rows:", nrow(result$country_summary)))
}