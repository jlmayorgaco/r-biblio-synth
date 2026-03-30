library(bibliometrix)

bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)

# Check C1 type and behavior
writeLines(paste("C1 class:", class(bib_data$C1)))
writeLines(paste("C1 mode:", mode(bib_data$C1)))
writeLines(paste("C1[1] class:", class(bib_data$C1[1])))
writeLines(paste("C1[[1]] class:", class(bib_data$C1[[1]])))

# Test is.na behavior
writeLines(paste("is.na(bib_data$C1[1]):", is.na(bib_data$C1[1])))

# Test with a row where C1 is NA
na_rows <- which(is.na(bib_data$C1))
writeLines(paste("Number of NA rows:", length(na_rows)))
if (length(na_rows) > 0) {
  writeLines(paste("First NA row:", na_rows[1]))
  writeLines(paste("C1 value at NA row:", is.na(bib_data$C1[na_rows[1]])))
}