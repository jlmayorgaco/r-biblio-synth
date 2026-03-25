# ============================================================================
# palettes.R - Color palettes (no globals)
# ============================================================================

#' Get a named color palette
#'
#' Returns a deterministic color palette by name.
#'
#' @param name Character. Palette name. One of "main", "sequential", "diverging".
#' @return A character vector of hex colors.
#' @export
get_biblio_palette <- function(name = "main") {
  palettes <- list(
    main = c(
      "#4e79a7", "#f28e2b", "#e15759", "#76b7b2",
      "#59a14f", "#edc949", "#b07aa1", "#ff9da7"
    ),
    sequential = c("#deebf7", "#9ecae1", "#3182bd", "#08306b"),
    diverging  = c("#d73027", "#fc8d59", "#fee08b", "#91bfdb", "#4575b4")
  )
  palettes[[name]]
}
