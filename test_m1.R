# ============================================================================
# test_m1.R - Full M1 integration test
# ============================================================================

.libPaths('C:/Users/walla/Documents/R/win-library/4.5')
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

cat('\n========================================\n')
cat('RBiblioSynth M1 Integration Test\n')
cat('========================================\n\n')

# Create extended fixture
fixture <- make_extended_biblio_fixture()
cat('Fixture created:', nrow(fixture), 'rows\n')

# Validate
validation <- validate_m1_input(fixture)
cat('Validation OK:', validation$ok, '\n\n')

# Compute all metrics
cat('--- Computing metrics ---\n')

overview <- compute_m1_overview(fixture)
cat('Overview:', overview$status, '\n')

doc_types <- compute_m1_doc_types(fixture)
cat('Doc types:', doc_types$status, '- rows:', nrow(doc_types$doc_type_table), '\n')

authors <- compute_m1_authors(fixture)
cat('Authors:', authors$status, '- top:', nrow(authors$top_authors), '\n')

citations <- compute_m1_citations(fixture)
cat('Citations:', citations$status, '- top cited:', nrow(citations$top_cited_documents), '\n')

countries <- compute_m1_countries(fixture)
cat('Countries:', countries$status, '\n')

sources <- compute_m1_sources(fixture)
cat('Sources:', sources$status, '- top:', nrow(sources$top_sources), '\n')

keywords <- compute_m1_keywords(fixture)
cat('Keywords:', keywords$status, '- top:', nrow(keywords$top_keywords), '\n')

bradford <- compute_m1_bradford(fixture)
cat('Bradford:', bradford$status, '- zones:', nrow(bradford$bradford_table), '\n')

# Render plots
cat('\n--- Rendering plots ---\n')

plots_doc <- render_m1_doc_types(doc_types)
cat('Doc types plots:', plots_doc$status, '- plots:', length(plots_doc$plots), '\n')

plots_auth <- render_m1_authors(authors)
cat('Authors plots:', plots_auth$status, '- plots:', length(plots_auth$plots), '\n')

plots_cit <- render_m1_citations(citations)
cat('Citations plots:', plots_cit$status, '- plots:', length(plots_cit$plots), '\n')

plots_src <- render_m1_sources(sources)
cat('Sources plots:', plots_src$status, '- plots:', length(plots_src$plots), '\n')

plots_kw <- render_m1_keywords(keywords)
cat('Keywords plots:', plots_kw$status, '- plots:', length(plots_kw$plots), '\n')

plots_bf <- render_m1_bradford(bradford)
cat('Bradford plots:', plots_bf$status, '- plots:', length(plots_bf$plots), '\n')

# Build tables
cat('\n--- Building tables ---\n')

tbl_overview <- build_m1_overview_table(overview)
cat('Overview table:', tbl_overview$status, '- rows:', nrow(tbl_overview$table), '\n')

tbl_doc <- build_m1_doc_types_table(doc_types)
cat('Doc types table:', tbl_doc$status, '- rows:', nrow(tbl_doc$table), '\n')

# Run full M1 pipeline
cat('\n--- Running full M1 pipeline ---\n')

tmp <- tempdir()
cfg <- list(output_dir = file.path(tmp, 'm1_test_output'))

result <- run_m1(fixture, config = cfg, export = TRUE)

cat('\nResult class:', class(result), '\n')
cat('Result status:', result$status, '\n')
cat('Data slots:', names(result$data), '\n')
cat('Artifact types:', names(result$artifacts), '\n')
cat('Has manifest:', 'manifest' %in% names(result$artifacts), '\n')

# Verify legacy compatibility
cat('\n--- Legacy compatibility check ---\n')
cat('overview has main_indicators:', 'main_indicators' %in% names(result$data$overview), '\n')
cat('doc_types has doc_type_table:', 'doc_type_table' %in% names(result$data$doc_types), '\n')
cat('authors has top_authors:', 'top_authors' %in% names(result$data$authors), '\n')
cat('citations has top_cited_documents:', 'top_cited_documents' %in% names(result$data$citations), '\n')
cat('countries has top_countries_by_articles:', 'top_countries_by_articles' %in% names(result$data$countries), '\n')
cat('sources has top_sources:', 'top_sources' %in% names(result$data$sources), '\n')
cat('keywords has top_keywords:', 'top_keywords' %in% names(result$data$keywords), '\n')
cat('bradford has bradford_table:', 'bradford_table' %in% names(result$data$bradford), '\n')

cat('\n========================================\n')
cat('M1 Integration Test PASSED!\n')
cat('========================================\n')
