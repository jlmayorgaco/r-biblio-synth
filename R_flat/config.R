# ============================================================================
# config.R - Configuration management
# ============================================================================

#' Get default bibliometric configuration
#'
#' Returns a list with default configuration values for the package.
#'
#' @return A named list with configuration keys and default values.
#' @export
#' @examples
#' cfg <- biblio_config()
#' cfg$output_dir
biblio_config <- function() {
  list(
    output_dir      = "results",
    export_plots    = TRUE,
    export_json     = TRUE,
    export_reports  = TRUE,
    theme_name      = "ieee",
    verbose         = TRUE,
    top_n_default   = 10,
    top_n_countries = 15,
    top_n_authors   = 10,
    top_n_sources   = 10,
    top_n_keywords  = 30,
    dpi             = 600
  )
}

#' Merge user configuration with defaults
#'
#' Merges a user-supplied configuration list with default values from
#' \code{\link{biblio_config}}. User values override defaults.
#'
#' @param config A named list of user configuration values.
#' @return A named list with merged configuration.
#' @export
#' @examples
#' cfg <- merge_biblio_config(list(verbose = FALSE))
merge_biblio_config <- function(config = list()) {
  defaults <- biblio_config()
  for (nm in names(config)) {
    defaults[[nm]] <- config[[nm]]
  }
  defaults
}
