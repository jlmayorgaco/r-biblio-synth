# ============================================================================
# helper-fixtures-m3.R - Test fixtures for M3 Countries Module
# ============================================================================

#' Minimal M3 fixture: enough for country extraction and production
make_m3_minimal_fixture <- function() {
  tibble::tibble(
    AU    = c("Smith J", "Jones M", "Brown A", "Davis K"),
    PY    = c(2020L, 2021L, 2022L, 2020L),
    TI    = c("Title A", "Title B", "Title C", "Title D"),
    TC    = c(5L, 10L, 3L, 8L),
    AU_CO = c("UNITED STATES", "CHINA", "GERMANY", "UNITED STATES")
  )
}

#' Extended M3 fixture: multi-country docs, yearly data, citations
make_m3_extended_fixture <- function() {
  tibble::tibble(
    AU    = c(
      "Smith J; Jones M", "Brown A; Wang X", "Davis K; Mueller H",
      "Lee Y; Garcia R",  "Wilson T",        "Zhang L; Kim J",
      "Patel S",          "Cohen B; Roy A",  "Tanaka M; Chen W",
      "Adams P; Osei K"
    ),
    PY    = c(2018L, 2019L, 2020L, 2021L, 2022L,
              2019L, 2020L, 2021L, 2022L, 2018L),
    TI    = paste("Title", LETTERS[1:10]),
    TC    = c(30L, 45L, 12L, 25L, 8L, 50L, 15L, 22L, 10L, 5L),
    DI    = paste0("10.1234/", seq_len(10)),
    AU_CO = c(
      "UNITED STATES;CHINA",
      "GERMANY;CHINA",
      "UNITED STATES;GERMANY",
      "CHINA;SOUTH KOREA",
      "UNITED STATES",
      "CHINA;JAPAN",
      "INDIA",
      "UNITED KINGDOM;INDIA",
      "JAPAN;CHINA",
      "UNITED STATES;GHANA"
    )
  )
}

#' Fixture with no AU_CO but with C1 for fallback testing
make_m3_c1_fixture <- function() {
  tibble::tibble(
    AU = c("Author A", "Author B"),
    PY = c(2020L, 2021L),
    TC = c(5L, 10L),
    C1 = c(
      "[Author A] Univ. of XYZ, New York, USA",
      "[Author B] Peking Univ., Beijing, CHINA"
    )
  )
}

#' Prepared data helper: wraps prepare_m3_country_data on the minimal fixture
make_m3_prepared_minimal <- function() {
  prepare_m3_country_data(make_m3_minimal_fixture())
}

#' Prepared data helper: wraps prepare_m3_country_data on the extended fixture
make_m3_prepared_extended <- function() {
  prepare_m3_country_data(make_m3_extended_fixture())
}
