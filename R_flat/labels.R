# ============================================================================
# labels.R - Label mappings (no globals)
# ============================================================================

#' Get label mapping for document types
#'
#' Returns a named character vector mapping document type codes to labels.
#'
#' @return A named character vector.
#' @export
get_label_mapping <- function() {
  c(
    article                    = "Article",
    review                     = "Review",
    conference_paper           = "Conference Paper",
    article_conference_paper   = "Article & Conference Paper",
    article_review             = "Article Review",
    article_article            = "Article (Revised)",
    book                       = "Book",
    book_chapter               = "Book Chapter",
    editorial                  = "Editorial",
    erratum                    = "Erratum",
    letter                     = "Letter",
    note                       = "Note",
    short_survey               = "Short Survey"
  )
}
