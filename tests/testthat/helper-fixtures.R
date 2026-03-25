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
