library(bibliometrix)

bib_data <- convert2df(
  file = "C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib",
  dbsource = "scopus", 
  format = "bibtex"
)

writeLines("Starting manual test of m3_prepare_country_data logic...")

# Test the values
input <- bib_data
has_au_co <- "AU_CO" %in% names(input)
has_c1 <- "C1" %in% names(input)

writeLines(paste("has_au_co:", has_au_co))
writeLines(paste("has_c1:", has_c1))

# Try the for loop from m3_prepare_country_data manually
doc_country_pairs <- list()

for (i in seq_len(min(nrow(input), 10))) {  # Only first 10 rows
  writeLines(paste("Processing row", i))
  
  # Get the raw country string for this document
  if (has_au_co) {
    writeLines("  Using AU_CO")
    raw_country <- if (!is.na(input$AU_CO[i])) {
      as.character(input$AU_CO[i])
    } else {
      NA_character_
    }
  } else if (has_c1) {
    writeLines("  Using C1")
    writeLines(paste("    input$C1[i] class:", class(input$C1[i])))
    writeLines(paste("    input$C1[i] is NA:", is.na(input$C1[i])))
    
    # This is the problematic line
    raw_country <- if (!is.na(input$C1[i])) {  # Error might be here
      as.character(input$C1[i])
    } else {
      NA_character_
    }
    writeLines(paste("    raw_country:", if (is.na(raw_country)) "NA" else substr(raw_country, 1, 50)))
  } else {
    raw_country <- NA_character_
  }
}

writeLines("Test passed without error!")