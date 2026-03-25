# ============================================================================
# run_tests.R - Run all testthat tests for RBiblioSynth
# ============================================================================

.libPaths('C:/Users/walla/Documents/R/win-library/4.5')
library(testthat)
library(cli)
library(ggplot2)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
library(bibliometrix)
library(stopwords)
library(scales)
library(stats)

# Source all R files
for (f in list.files('R/', pattern = '\\.R$', recursive = TRUE, full.names = TRUE)) {
  source(f)
}

# Source test fixtures
source('tests/testthat/helper-fixtures.R')

# Run tests
test_dir('tests/testthat/', reporter = 'summary')
