library(bibliometrix)
bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)
writeLines(paste("C1 has NA:", sum(is.na(bib_data$C1))))
writeLines(paste("C1 empty strings:", sum(bib_data$C1 == "", na.rm=TRUE)))
writeLines(paste("Total rows:", nrow(bib_data)))
# Check if any C1 values are NA
if (any(is.na(bib_data$C1))) {
  writeLines("C1 has NA values!")
}
# Check the condition that trips
test_result <- !all(is.na(bib_data$C1)) && any(bib_data$C1 != "", na.rm=TRUE)
writeLines(paste("Test condition result:", test_result))