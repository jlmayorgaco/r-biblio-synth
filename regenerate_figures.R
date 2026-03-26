# Regenerate figures with fixes
.libPaths('C:/Users/walla/Documents/R/win-library/4.5')
library(bibliometrix)

for (f in list.files('R/', pattern = '\\.R$', recursive = TRUE, full.names = TRUE)) source(f)

raw_bib <- readLines("examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/data/scopus.bib", warn = FALSE)
tmp <- tempfile(fileext = ".bib")
writeLines(raw_bib, tmp, useBytes = TRUE)
bib_data <- convert2df(file = tmp, dbsource = "scopus", format = "bibtex")
unlink(tmp)

cat("Loaded", nrow(bib_data), "documents\n")

unlink("results/m1", recursive = TRUE, force = TRUE)
result <- run_m1(bib_data, config = list(output_dir = "results"), export = TRUE)

cat("Status:", result$status, "\n")
cat("\nPlots generated:\n")
for (nm in names(result$artifacts$plots)) {
  cat(" ", nm, ":", paste(names(result$artifacts$plots[[nm]]$plots), collapse = ", "), "\n")
}

cat("\nFiles:\n")
list.files("results/m1/plots/", pattern = "\\.png$")
