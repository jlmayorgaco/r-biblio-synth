library(bibliometrix)

# Source all R files
source("C:/Users/walla/Documents/Github/r-biblio-synth/R/core/bootstrap.R")

bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)

writeLines("Starting test of m3_prepare_country_data...")

# Try the prepare function
result <- tryCatch({
  prepare_m3_country_data(bib_data[1:100, ])  # Just first 100 rows
}, error = function(e) {
  writeLines(paste("ERROR:", e$message))
  writeLines(paste("Call:", deparse(e$call)))
  NULL
})

if (!is.null(result)) {
  writeLines("SUCCESS!")
  writeLines(paste("Status:", result$status))
}