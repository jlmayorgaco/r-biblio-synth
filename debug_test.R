library(bibliometrix)
bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)

# Test the condition at line 32
test1 <- "C1" %in% names(bib_data)  # TRUE
test2 <- !all(is.na(bib_data$C1))   # TRUE (not all are NA)
comp_result <- bib_data$C1 != ""    # Vector with TRUE/FALSE/NA
writeLines(paste("Comparison result has NA:", any(is.na(comp_result))))
writeLines(paste("Comparison result sample (first 10):"))
print(head(comp_result, 10))

# This is the problematic line:
writeLines("Testing: any(bib_data$C1 != \"\")")
tryCatch({
  result <- any(bib_data$C1 != "")
  writeLines(paste("Result:", result))
}, error = function(e) {
  writeLines(paste("ERROR:", e$message))
})