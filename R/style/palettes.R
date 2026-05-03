# ============================================================================
# palettes.R - Color palettes (no globals)
# ============================================================================

#' Get a named color palette
#'
#' Returns a deterministic color palette by name. When \code{n} is supplied,
#' the requested palette is expanded or truncated deterministically.
#'
#' @param name Character. Palette name. One of "main", "sequential", "diverging".
#' @param n Optional integer. Number of colors to return.
#' @return A character vector of hex colors.
#' @export
get_biblio_palette <- function(name = "main", n = NULL) {
  palettes <- list(
    main = c(
      "#4e79a7", "#f28e2b", "#e15759", "#76b7b2",
      "#59a14f", "#edc949", "#b07aa1", "#ff9da7"
    ),
    sequential = c("#deebf7", "#9ecae1", "#3182bd", "#08306b"),
    diverging  = c("#d73027", "#fc8d59", "#fee08b", "#91bfdb", "#4575b4")
  )

  palette <- palettes[[name]]
  if (is.null(palette)) {
    palette <- palettes[["main"]]
  }

  if (is.null(n)) {
    return(palette)
  }

  n <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n) || n <= 0) {
    return(character(0))
  }

  if (length(palette) == n) {
    return(palette)
  }

  if (length(palette) == 1L) {
    return(rep(palette, n))
  }

  grDevices::colorRampPalette(palette)(n)
}
