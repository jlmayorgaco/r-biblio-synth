# ============================================================================
# helper-fixtures.R - Test fixtures for RBiblioSynth tests
# ============================================================================

#' Create a minimal bibliographic fixture
#'
#' @return A tibble with AU, PY, TI, SO columns.
#' @export
make_minimal_biblio_fixture <- function() {
  tibble::tibble(
    AU = c("Smith J", "Jones M", "Brown A"),
    PY = c(2020L, 2021L, 2022L),
    TI = c("First title", "Second title", "Third title"),
    SO = c("Journal A", "Journal B", "Journal C")
  )
}

#' Create an extended bibliographic fixture
#'
#' @return A tibble with extended columns for comprehensive testing.
#' @export
make_extended_biblio_fixture <- function() {
  tibble::tibble(
    AU = c("Smith J; Jones M", "Johnson K; Williams P", "Brown A; Davis R", "Wilson T", "Taylor E"),
    PY = c(2020L, 2021L, 2022L, 2020L, 2021L),
    TI = c(
      "Analysis of power systems",
      "Frequency estimation methods",
      "Review of grid stability",
      "New control strategies",
      "Machine learning in power"
    ),
    SO = c("IEEE Trans", "IEEE Trans", "J Power Systems", "IEEE Trans", "Applied Energy"),
    TC = c(10L, 25L, 5L, 15L, 8L),
    DI = c("10.1109/1", "10.1109/2", "10.1016/3", "10.1109/4", "10.1016/5"),
    DT = c("article", "article", "review", "article", "article"),
    DE = c("power systems; frequency; estimation",
           "frequency; estimation; algorithm",
           "grid; stability; control",
           "control; power; systems",
           "machine learning; power"),
    ID = c("POWER; FREQUENCY; GRID",
           "FREQUENCY; ESTIMATION; POWER",
           "GRID; STABILITY; CONTROL",
           "CONTROL; POWER; SYSTEMS",
           "MACHINE LEARNING; POWER; GRID"),
    AB = c("Abstract 1", "Abstract 2", "Abstract 3", "Abstract 4", "Abstract 5"),
    AU_CO = c("USA; UK", "USA; China", "UK; Germany", "USA", "USA; Japan")
  )
}

#' Create a richer bibliographic fixture for advanced M1 testing
#'
#' @return A tibble with enough variety for advanced M1 analyses.
#' @export
make_m1_rich_fixture <- function() {
  n <- 24L
  years <- rep(2014:2025, each = 2L)
  authors_pool <- list(
    c("Smith J", "Jones M"),
    c("Garcia L", "Chen Y"),
    c("Brown A", "Davis R"),
    c("Taylor E", "Wilson T"),
    c("Khan S", "Lopez P"),
    c("Martin F", "Lee H")
  )
  topic_pool <- list(
    c("grid stability", "voltage control", "power systems"),
    c("frequency estimation", "control theory", "signal processing"),
    c("load forecasting", "machine learning", "time series"),
    c("renewable integration", "microgrid", "energy transition")
  )
  country_pool <- list(
    c("USA", "UK"),
    c("China", "USA"),
    c("Germany", "Spain"),
    c("Japan", "Canada")
  )
  source_pool <- c(
    "IEEE Transactions on Smart Grid",
    "Applied Energy",
    "Energy",
    "Electric Power Systems Research"
  )

  rows <- lapply(seq_len(n), function(i) {
    author_pair <- authors_pool[[((i - 1L) %% length(authors_pool)) + 1L]]
    if (i %% 3L == 0L) {
      author_pair <- c(author_pair, "Patel N")
    }
    topic_set <- topic_pool[[((i - 1L) %% length(topic_pool)) + 1L]]
    countries <- country_pool[[((i - 1L) %% length(country_pool)) + 1L]]
    source <- source_pool[((i - 1L) %% length(source_pool)) + 1L]
    year <- years[i]
    citations <- as.integer(5 + (i * 3) + ((i - 1L) %% 4L) * 4L)
    document_type <- if (i %% 7L == 0L) "review" else "article"

    title <- paste("Advanced study", i, "on", topic_set[1], "for", gsub("IEEE Transactions on ", "", source))
    keywords <- paste(c(topic_set, "bibliometrics", "power engineering"), collapse = "; ")
    keywords_plus <- paste(toupper(gsub(" ", "_", c(topic_set, "analysis", "trend"))), collapse = "; ")
    abstract <- paste(
      "This study examines", topic_set[1], "and", topic_set[2],
      "with emphasis on", topic_set[3], "across longitudinal energy datasets."
    )
    refs <- paste(
      c(
        sprintf("%s, %d, %s", author_pair[1], max(2000, year - 4L), source),
        sprintf("Wang H, %d, Applied Energy", max(2001, year - 5L)),
        sprintf("Miller D, %d, Energy", max(2002, year - 6L))
      ),
      collapse = "; "
    )
    c1 <- paste(
      sprintf("University A, Department of Energy Systems, %s", countries[1]),
      sprintf("University B, School of Engineering, %s", countries[2]),
      collapse = "; "
    )

    tibble::tibble(
      AU = paste(author_pair, collapse = "; "),
      PY = as.integer(year),
      TI = title,
      SO = source,
      TC = citations,
      DI = sprintf("10.1000/rbs-%03d", i),
      DT = document_type,
      DE = keywords,
      ID = keywords_plus,
      AB = abstract,
      AU_CO = paste(countries, collapse = "; "),
      C1 = c1,
      CR = refs
    )
  })

  dplyr::bind_rows(rows)
}
