# ============================================================================
# test-module-m0-advanced.R - Tests for advanced M0 capabilities
# ============================================================================

test_that("run_m0 records field-level provenance and conflict resolution", {
  merge_ns <- get("m0_merge_sources", envir = asNamespace("RBiblioSynth"))
  build_prov_ns <- get("m0_build_field_provenance_table", envir = asNamespace("RBiblioSynth"))
  build_conflict_ns <- get("m0_build_field_conflict_table", envir = asNamespace("RBiblioSynth"))

  df_wos <- data.frame(
    AU = "Smith J",
    TI = "Power System Forecasting",
    PY = 2020,
    SO = "IEEE Transactions on Power Systems",
    DI = "10.1000/test-doi",
    DT = "Article",
    TC = 10,
    AB = "Short abstract.",
    DE = "forecasting",
    SOURCE_DB = "WOS",
    SOURCE_TAG = "wos",
    stringsAsFactors = FALSE
  )

  df_openalex <- data.frame(
    AU = "Smith J; Brown A",
    TI = "Power System Forecasting with Deep Learning",
    PY = 2020,
    SO = "Open Energy Journal",
    DI = "10.1000/test-doi",
    DT = "Article",
    TC = 12,
    AB = "A much longer abstract with additional detail and methodological context.",
    DE = "forecasting; deep learning",
    SOURCE_DB = "OPENALEX",
    SOURCE_TAG = "openalex",
    stringsAsFactors = FALSE
  )

  merged <- merge_ns(
    list(wos = df_wos, openalex = df_openalex),
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE, source_priority = c("WOS", "OPENALEX", "GENERIC"))
  )

  expect_equal(nrow(merged), 1)
  expect_true("M0_FIELD_PROVENANCE" %in% names(merged))
  expect_true("M0_FIELD_CONFLICTS" %in% names(merged))
  expect_true(merged$M0_CONFLICT_COUNT[1] >= 1)
  expect_match(merged$AB[1], "much longer abstract", ignore.case = TRUE)
  expect_equal(merged$SO[1], "IEEE Transactions on Power Systems")
  prov_table <- build_prov_ns(merged)
  conflict_table <- build_conflict_ns(merged)
  expect_true(any(prov_table$field %in% c("AB", "SO", "DE")))
  expect_true(any(conflict_table$field %in% c("AB", "SO", "DE")))
})

test_that("run_m0 derives PRISMA counts from screening ledger decisions", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))

  df <- data.frame(
    AU = c("Smith J", "Garcia M", "Brown A"),
    TI = c("Paper A", "Paper B", "Paper C"),
    PY = c(2020, 2021, 2022),
    SO = c("Journal A", "Journal B", "Journal C"),
    DI = c("10.1000/a", "10.1000/b", "10.1000/c"),
    DT = c("Article", "Review", "Article"),
    TC = c(5, 7, 2),
    stringsAsFactors = FALSE
  )

  screening_ledger <- data.frame(
    title = c("Paper A", "Paper A", "Paper B", "Paper B", "Paper C", "Paper C", "Paper C", "Paper A", "Paper C"),
    stage = c("screening", "screening", "screening", "screening", "screening", "screening", "screening", "eligibility", "eligibility"),
    reviewer = c("r1", "r2", "r1", "r2", "r1", "r2", "adjudicator", "r1", "r1"),
    decision = c("include", "include", "exclude", "exclude", "include", "exclude", "include", "exclude", "include"),
    reason = c(NA, NA, "Out of scope", "Out of scope", NA, NA, NA, "No full text", NA),
    is_final = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    status = c("submitted", "submitted", "submitted", "submitted", "submitted", "submitted", "final", "final", "final"),
    stringsAsFactors = FALSE
  )

  file_a <- tempfile(fileext = ".csv")
  on.exit(unlink(file_a), add = TRUE)
  utils::write.csv(df, file_a, row.names = FALSE)

  result <- run_m0_ns(
    sources = list(src = list(file = file_a, db = "generic", format = "csv", prisma_role = "database")),
    prisma_spec = "auto",
    screening_ledger = screening_ledger,
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE),
    export = FALSE
  )

  expect_true(isTRUE(result$data$screening_summary$ok))
  expect_equal(result$data$prisma$screening$records_screened, 3)
  expect_equal(result$data$prisma$screening$excluded_screening, 1)
  expect_equal(result$data$prisma$eligibility$fulltext_assessed, 2)
  expect_equal(result$data$prisma$eligibility$excluded_fulltext, 1)
  expect_equal(result$data$prisma$included$studies_included, 1)
  expect_true("No full text" %in% names(result$data$prisma$eligibility$excluded_reasons))
  expect_equal(result$data$screening_summary$reliability$status, "success")
})

test_that("connector mappers standardize mock API payloads", {
  map_openalex <- get("m0_map_openalex_records", envir = asNamespace("RBiblioSynth"))
  map_crossref <- get("m0_map_crossref_records", envir = asNamespace("RBiblioSynth"))
  map_pubmed <- get("m0_map_pubmed_records", envir = asNamespace("RBiblioSynth"))

  openalex_records <- list(list(
    display_name = "Grid-aware forecasting",
    publication_year = 2024,
    cited_by_count = 15,
    doi = "https://doi.org/10.1000/openalex",
    type = "article",
    abstract_inverted_index = list(Grid = list(0), aware = list(1), forecasting = list(2)),
    concepts = list(list(display_name = "Forecasting", score = 0.7)),
    primary_location = list(source = list(display_name = "OpenAlex Journal", issn = c("1234-5678"))),
    authorships = list(
      list(
        author = list(display_name = "Smith J", orcid = "https://orcid.org/0000-0001-0000-0001"),
        institutions = list(list(display_name = "Test University", country_code = "US"))
      )
    )
  ))

  crossref_records <- list(list(
    title = list("Crossref-ready study"),
    DOI = "10.1000/crossref",
    type = "journal-article",
    abstract = "Crossref abstract",
    `container-title` = list("Crossref Journal"),
    `is-referenced-by-count` = 9,
    publisher = "Crossref Publisher",
    subject = list("Energy"),
    author = list(list(
      family = "Garcia",
      given = "Maria",
      ORCID = "https://orcid.org/0000-0002-0000-0002",
      affiliation = list(list(name = "Crossref Institute"))
    )),
    funder = list(list(name = "NSF")),
    reference = list(list(author = "Doe J", year = "2019", `journal-title` = "Energy", DOI = "10.1000/ref")),
    published = list(`date-parts` = list(list(2023, 5, 1)))
  ))

  pubmed_result <- list(
    uids = c("12345"),
    `12345` = list(
      title = "PubMed study",
      pubdate = "2022 Jan",
      fulljournalname = "PubMed Journal",
      authors = list(list(name = "Lee K")),
      articleids = list(list(idtype = "doi", value = "10.1000/pubmed"))
    )
  )

  openalex_df <- map_openalex(openalex_records)
  crossref_df <- map_crossref(crossref_records)
  pubmed_df <- map_pubmed(pubmed_result)

  expect_equal(openalex_df$DI[1], "10.1000/openalex")
  expect_match(openalex_df$AB[1], "Grid aware forecasting")
  expect_match(openalex_df$OI[1], "0000-0001-0000-0001")

  expect_equal(crossref_df$DI[1], "10.1000/crossref")
  expect_match(crossref_df$FU[1], "NSF")
  expect_match(crossref_df$OI[1], "0000-0002-0000-0002")

  expect_equal(pubmed_df$DI[1], "10.1000/pubmed")
  expect_equal(pubmed_df$SO[1], "PubMed Journal")
  expect_equal(pubmed_df$AU[1], "Lee K")
})
