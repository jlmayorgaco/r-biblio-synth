# ============================================================================
# helper-fixtures.R - Test fixtures for RBiblioSynth tests
# ============================================================================

#' Create a minimal bibliographic fixture
#'
#' Returns a tiny tibble suitable for unit tests.
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
