library(bibliometrix)

bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus",
  format = "bibtex"
)

cat("AU_CO column type:", class(bib_data$AU_CO), "\n")
cat("AU_CO first 3 values:\n")
print(bib_data$AU_CO[1:3])
cat("AU_CO is.factor:", is.factor(bib_data$AU_CO), "\n")
cat("AU_CO is.list:", is.list(bib_data$AU_CO), "\n")

# Check for NA values
cat("NA count:", sum(is.na(bib_data$AU_CO)), "\n")