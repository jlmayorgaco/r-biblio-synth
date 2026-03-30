library(bibliometrix)
bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)
writeLines("Column names:")
writeLines(paste(names(bib_data), collapse=", "))
writeLines(paste("Has AU_CO:", "AU_CO" %in% names(bib_data)))
writeLines(paste("Has C1:", "C1" %in% names(bib_data)))
writeLines(paste("Has AU_UN:", "AU_UN" %in% names(bib_data)))
if ("C1" %in% names(bib_data)) {
  writeLines("C1 first 3:")
  writeLines(paste(bib_data$C1[1:3], collapse=" | "))
}
if ("AU_UN" %in% names(bib_data)) {
  writeLines("AU_UN first 3:")
  writeLines(paste(bib_data$AU_UN[1:3], collapse=" | "))
}