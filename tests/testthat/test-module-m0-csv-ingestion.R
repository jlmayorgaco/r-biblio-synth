# ============================================================================
# test-module-m0-csv-ingestion.R - Tests for M0 CSV multi-file ingestion
# ============================================================================

test_that("m0_validate_sources accepts multi-file CSV source specs", {
  tmp_dir <- tempfile("m0_csv_sources_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  scopus_a <- file.path(tmp_dir, "scopus_part_1.csv")
  scopus_b <- file.path(tmp_dir, "scopus_part_2.csv")
  writeLines(c("Authors,Title,Year", "\"Smith J\",\"Paper A\",2020"), scopus_a)
  writeLines(c("Authors,Title,Year", "\"Brown A\",\"Paper B\",2021"), scopus_b)

  validated <- m0_validate_sources(list(
    scopus = list(
      files = c(scopus_a, scopus_b),
      db = "scopus",
      format = "csv",
      schema = "scopus_csv",
      chunk_size_rows = 1000
    )
  ))

  expect_true(validated$ok)
  expect_equal(validated$details$scopus$n_files, 2)
  expect_equal(validated$details$scopus$schema, "scopus_csv")
})

test_that("m0_load_single_source loads multi-file Scopus CSV and drops repeated headers", {
  tmp_dir <- tempfile("m0_scopus_csv_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  file_a <- file.path(tmp_dir, "scopus_export_a.csv")
  file_b <- file.path(tmp_dir, "scopus_export_b.csv")
  header <- paste(
    c(
      "Authors", "Title", "Year", "Source title", "Abstract",
      "Author Keywords", "Index Keywords", "Affiliations", "References",
      "Cited by", "DOI", "Document Type", "Language of Original Document"
    ),
    collapse = ","
  )

  writeLines(c(
    header,
    "\"Smith J\",\"Paper A\",2020,\"Journal A\",\"Abstract A\",\"forecasting\",\"power systems\",\"University of Texas, Austin, USA\",\"Doe J, 2018, Energy\",5,\"10.1000/a\",\"Article\",\"English\"",
    header,
    "\"Brown A\",\"Paper B\",2021,\"Journal B\",\"Abstract B\",\"review\",\"biblio\",\"Imperial College London, London, UK\",\"Smith J, 2019, IEEE\",3,\"10.1000/b\",\"Review\",\"English\""
  ), file_a)

  writeLines(c(
    header,
    "\"Garcia M\",\"Paper C\",2022,\"Journal C\",\"Abstract C\",\"grid\",\"stability\",\"Universidad Nacional de Colombia, Bogota, Colombia\",\"Garcia M, 2020, Energies\",7,\"10.1000/c\",\"Article\",\"English\""
  ), file_b)

  result <- m0_load_single_source(list(
    files = c(file_a, file_b),
    db = "scopus",
    format = "csv",
    schema = "scopus_csv",
    chunk_size_rows = 1
  ), config = biblio_config(verbose = FALSE, cache_enabled = FALSE))

  expect_equal(nrow(result), 3)
  expect_true(all(c("AU", "TI", "PY", "SO", "AB", "DE", "ID", "C1", "CR", "DI", "TC") %in% names(result)))
  expect_false(any(result$TI == "Title"))
  expect_true(any(grepl("UNITED STATES", result$AU_CO, fixed = TRUE)))
  expect_true(any(grepl("UNITED KINGDOM", result$AU_CO, fixed = TRUE)))
  expect_equal(sort(unique(result$DI)), c("10.1000/a", "10.1000/b", "10.1000/c"))
  expect_true(all(c("M0_SOURCE_FILE", "M0_SOURCE_PATH", "M0_SOURCE_ROW") %in% names(result)))
  expect_equal(sort(unique(result$M0_SOURCE_FILE)), c("scopus_export_a.csv", "scopus_export_b.csv"))
})

test_that("run_m0 accepts WoS CSV file patterns and merges to canonical data", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))

  tmp_dir <- tempfile("m0_wos_csv_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wos_a <- file.path(tmp_dir, "wos_export_1.csv")
  wos_b <- file.path(tmp_dir, "wos_export_2.csv")

  df_a <- data.frame(
    Authors = "Smith J",
    `Article Title` = "Power System Forecasting",
    `Publication Year` = 2020,
    `Source Title` = "IEEE Transactions on Power Systems",
    Abstract = "Forecasting study.",
    `Author Keywords` = "forecasting; power systems",
    `Keywords Plus` = "electricity",
    Affiliations = "University of Texas, Austin, USA",
    `Cited References` = "Doe J, 2018, Energy",
    `Times Cited` = 10,
    DOI = "10.1000/shared",
    `Document Type` = "Article",
    Language = "English",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  df_b <- data.frame(
    Authors = "Brown A",
    `Article Title` = "Power System Forecasting",
    `Publication Year` = 2020,
    `Source Title` = "IEEE Transactions on Power Systems",
    Abstract = "Same document from another export.",
    `Author Keywords` = "forecasting; stability",
    `Keywords Plus` = "grid",
    Affiliations = "Imperial College London, London, UK",
    `Cited References` = "Smith J, 2019, IEEE",
    `Times Cited` = 12,
    DOI = "10.1000/shared",
    `Document Type` = "Article",
    Language = "English",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  utils::write.csv(df_a, wos_a, row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(df_b, wos_b, row.names = FALSE, fileEncoding = "UTF-8")

  result <- run_m0_ns(
    sources = list(
      wos = list(
        file_pattern = file.path(tmp_dir, "wos_export_*.csv"),
        db = "wos",
        format = "csv",
        schema = "wos_csv",
        chunk_size_rows = 1,
        prisma_role = "database"
      )
    ),
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE),
    export = FALSE
  )

  expect_s3_class(result, "biblio_module_result")
  expect_equal(nrow(result$data$bib_merged), 1)
  expect_true(all(c("M0_DOC_ID", "AU", "TI", "PY", "SO", "DI", "AB") %in% names(result$data$bib_merged)))
  expect_true("documents" %in% names(result$data$organized))
  expect_true(is.data.frame(result$data$source_summary))
})

test_that("run_m0 normalizes CSV headers and DOI keys across Scopus and WoS", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))

  tmp_dir <- tempfile("m0_cross_source_csv_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  scopus_file <- file.path(tmp_dir, "scopus_lower_headers.csv")
  wos_file <- file.path(tmp_dir, "wos_lower_headers.csv")

  writeLines(c(
    "authors,title,year,source title,abstract,author keywords,index keywords,affiliations,references,cited by,doi,document type,language of original document",
    "\"Smith J\",\"Grid Stability Forecasting\",2022,\"Energy Journal\",\"Scopus abstract\",\"forecasting; stability\",\"grid\",\"University of Toronto, Toronto, Canada\",\"Doe J, 2020\",4,\"https://doi.org/10.5555/ABC\",\"Article\",\"English\""
  ), scopus_file)

  writeLines(c(
    "au,ti,py,so,ab,de,id,c1,cr,tc,di,dt,la",
    "\"Smith J; Brown A\",\"Grid Stability Forecasting\",2022,\"Energy Journal\",\"WoS abstract with more methodological details\",\"forecasting; power systems\",\"electric grids\",\"Imperial College London, London, UK\",\"Roe J, 2021\",9,\"doi:10.5555/abc.\",\"Article\",\"English\""
  ), wos_file)

  result <- run_m0_ns(
    sources = list(
      scopus = list(
        file = scopus_file,
        db = "scopus",
        format = "csv",
        schema = "scopus_csv",
        chunk_size_rows = 1,
        prisma_role = "database"
      ),
      wos = list(
        file = wos_file,
        db = "wos",
        format = "csv",
        schema = "wos_csv",
        chunk_size_rows = 1,
        prisma_role = "database"
      )
    ),
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE),
    export = FALSE
  )

  merged <- result$data$bib_merged
  expect_equal(nrow(merged), 1)
  expect_equal(merged$DI, "10.5555/abc")
  expect_equal(merged$M0_DOI_KEY, "10.5555/abc")
  expect_true(grepl("SCOPUS", merged$M0_SOURCE_DBS, fixed = TRUE))
  expect_true(grepl("WOS", merged$M0_SOURCE_DBS, fixed = TRUE))
  expect_true(grepl("scopus_lower_headers.csv", merged$M0_SOURCE_FILE, fixed = TRUE))
  expect_true(grepl("wos_lower_headers.csv", merged$M0_SOURCE_FILE, fixed = TRUE))
  expect_true(any(grepl("CANADA|UNITED KINGDOM", merged$AU_CO)))
  expect_true("field_provenance" %in% names(result$data$organized))
  expect_true("field_conflicts" %in% names(result$data$organized))
})
