# main2.r
suppressPackageStartupMessages(library(here))

# Load the main entrypoint (index.r)
source(here::here("src2", "index.r"))

# Create pipeline
pipeline <- AnalysisPipeline$new(
  bib_file = "data/scopus.bib",
  modules  = c("M1_DataIngestion", "M2_Production", "M3_Countries"),
  out_dir  = "results2"
)

# Add metadata
pipeline$setTitle("Power Systems Frequency Estimators from 1960 to 2023")
pipeline$setDate("Wednesday, July 10, 2024 1:50:56 AM")
pipeline$setQuery("TITLE-ABS-KEY ( power AND system AND frequency AND estimator ) from 1960 to 2023")
pipeline$setKeywords(c("power", "system", "frequency", "estimator"))

# Run
pipeline$run()
pipeline$export_results_index()
