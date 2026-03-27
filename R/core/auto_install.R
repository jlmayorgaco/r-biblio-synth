# ============================================================================
# auto_install.R - Automatic dependency installation and loading
# ============================================================================
# This script automatically installs missing R packages and loads them.
# It reads dependencies from the DESCRIPTION file and checks/install/load them.
#
# Usage:
#   source("R/core/auto_install.R")
#   auto_install_dependencies()
#
# Alternatively, call from command line:
#   Rscript -e "source('R/core/auto_install.R'); auto_install_dependencies()"

#' Get package dependencies from DESCRIPTION file
#'
#' Parses the DESCRIPTION file to extract Import and Suggests packages.
#'
#' @param desc_path Path to DESCRIPTION file (default: "DESCRIPTION")
#' @return A list with imports and suggests character vectors
#' @export
get_package_dependencies <- function(desc_path = "DESCRIPTION") {
  # Resolve path relative to current working directory or project root
  if (!file.exists(desc_path)) {
    # Try to find DESCRIPTION from project root
    # Check if we're in a subdirectory of the project
    candidate <- file.path(dirname(desc_path), "DESCRIPTION")
    if (file.exists(candidate)) {
      desc_path <- candidate
    } else {
      # Try finding from script location
      script_dir <- tryCatch({
        dirname(sys.frame(1)$ofile)
      }, error = function(e) {
        getwd()
      })
      
      # Try project root (assuming R/core structure)
      candidate <- file.path(script_dir, "..", "..", "DESCRIPTION")
      candidate <- normalizePath(candidate, mustWork = FALSE)
      if (file.exists(candidate)) {
        desc_path <- candidate
      } else {
        # Last resort: search for DESCRIPTION
        found <- list.files(".", pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE)
        if (length(found) > 0) {
          desc_path <- found[1]
        }
      }
    }
  }
  
  if (!file.exists(desc_path)) {
    stop("DESCRIPTION file not found. Tried: ", desc_path,
         "\nPlease run this script from the project root directory.")
  }
  
  lines <- readLines(desc_path, warn = FALSE)
  
  # Find Imports section
  imports_start <- grep("^Imports:", lines)
  suggests_start <- grep("^Suggests:", lines)
  
  imports <- character()
  suggests <- character()
  
  # Parse Imports
  if (length(imports_start) == 1) {
    imports_lines <- lines[(imports_start + 1):length(lines)]
    # Find where next section starts
    next_section <- grep("^\\w+:", imports_lines)
    if (length(next_section) > 0) {
      imports_lines <- imports_lines[1:(next_section[1] - 1)]
    }
    imports_text <- paste(imports_lines, collapse = "")
    # Clean up the text
    imports_text <- gsub("\n", "", imports_text)
    imports_text <- gsub("\\s+", " ", imports_text)
    # Split by comma
    imports <- trimws(unlist(strsplit(imports_text, ",")))
    imports <- imports[imports != ""]
    # Remove version specifications
    imports <- gsub("\\s*\\([^)]*\\)", "", imports)
    imports <- trimws(imports)
  }
  
  # Parse Suggests
  if (length(suggests_start) == 1) {
    suggests_lines <- lines[(suggests_start + 1):length(lines)]
    # Find where next section starts
    next_section <- grep("^\\w+:", suggests_lines)
    if (length(next_section) > 0) {
      suggests_lines <- suggests_lines[1:(next_section[1] - 1)]
    }
    suggests_text <- paste(suggests_lines, collapse = "")
    suggests_text <- gsub("\n", "", suggests_text)
    suggests_text <- gsub("\\s+", " ", suggests_text)
    suggests <- trimws(unlist(strsplit(suggests_text, ",")))
    suggests <- suggests[suggests != ""]
    suggests <- gsub("\\s*\\([^)]*\\)", "", suggests)
    suggests <- trimws(suggests)
  }
  
  list(
    imports = imports,
    suggests = suggests
  )
}

#' Check if a package is installed
#'
#' @param package_name Name of the package
#' @return TRUE if installed, FALSE otherwise
#' @export
is_package_installed <- function(package_name) {
  requireNamespace(package_name, quietly = TRUE)
}

#' Install a package from CRAN or GitHub
#'
#' @param package_name Name of the package
#' @param cran_mirror CRAN mirror URL (default: auto)
#' @param quiet Logical: suppress output
#' @return TRUE if successful, FALSE otherwise
#' @export
install_single_package <- function(package_name, cran_mirror = NULL, quiet = FALSE) {
  if (is_package_installed(package_name)) {
    if (!quiet) message("Package '", package_name, "' already installed.")
    return(TRUE)
  }
  
  # Determine repository type
  # Some packages need to be installed from GitHub
  github_packages <- list(
    bibliometrix = "massimoaria/bibliometrix",
    stopwords = "davnn/stopwords"
    # Add more GitHub packages as needed
  )
  
  if (package_name %in% names(github_packages)) {
    # Install from GitHub
    if (!quiet) message("Installing '", package_name, "' from GitHub...")
    tryCatch({
      if (!is_package_installed("remotes")) {
        install.packages("remotes", repos = cran_mirror, quiet = quiet)
      }
      remotes::install_github(github_packages[[package_name]], 
                              quiet = quiet, upgrade = "never")
      return(is_package_installed(package_name))
    }, error = function(e) {
      warning("Failed to install '", package_name, "' from GitHub: ", e$message)
      return(FALSE)
    })
  } else {
    # Install from CRAN
    if (!quiet) message("Installing '", package_name, "' from CRAN...")
    tryCatch({
      install.packages(package_name, repos = cran_mirror %||% getOption("repos"), 
                       quiet = quiet)
      return(is_package_installed(package_name))
    }, error = function(e) {
      warning("Failed to install '", package_name, "' from CRAN: ", e$message)
      return(FALSE)
    })
  }
}

#' Auto-install and load all dependencies
#'
#' Main function to install missing packages and load them.
#'
#' @param desc_path Path to DESCRIPTION file
#' @param install_suggests Logical: whether to install Suggests packages
#' @param cran_mirror Optional CRAN mirror URL
#' @param quiet Logical: suppress messages
#' @param load Logical: whether to load packages after installation
#' @return A list with installation status
#' @export
auto_install_dependencies <- function(desc_path = "DESCRIPTION", 
                                       install_suggests = FALSE,
                                       cran_mirror = NULL,
                                       quiet = FALSE,
                                       load = TRUE) {
  if (!quiet) message("Checking and installing dependencies...")
  
  # Set default CRAN mirror if not provided
  if (is.null(cran_mirror)) {
    cran_mirror <- getOption("repos")
    if (is.null(cran_mirror) || cran_mirror == "@CRAN@") {
      cran_mirror <- "https://cloud.r-project.org"
    }
  }
  
  # Get dependencies
  deps <- get_package_dependencies(desc_path)
  
  imports <- deps$imports
  suggests <- deps$suggests
  
  packages_to_install <- if (install_suggests) {
    c(imports, suggests)
  } else {
    imports
  }
  
  # Remove empty strings
  packages_to_install <- packages_to_install[packages_to_install != ""]
  
  if (length(packages_to_install) == 0) {
    if (!quiet) message("No packages found in DESCRIPTION.")
    return(list(
      installed = character(),
      existing = character(),
      failed = character()
    ))
  }
  
  # Check status of each package
  installed <- character()
  existing <- character()
  failed <- character()
  
  for (pkg in packages_to_install) {
    if (is_package_installed(pkg)) {
      existing <- c(existing, pkg)
    } else {
      success <- install_single_package(pkg, cran_mirror, quiet)
      if (success) {
        installed <- c(installed, pkg)
      } else {
        failed <- c(failed, pkg)
      }
    }
  }
  
  # Print summary
  if (!quiet) {
    if (length(existing) > 0) {
      message("Already installed: ", paste(existing, collapse = ", "))
    }
    if (length(installed) > 0) {
      message("Successfully installed: ", paste(installed, collapse = ", "))
    }
    if (length(failed) > 0) {
      warning("Failed to install: ", paste(failed, collapse = ", "))
    }
  }
  
  # Load packages if requested
  if (load) {
    if (!quiet) message("Loading packages...")
    for (pkg in packages_to_install) {
      if (is_package_installed(pkg)) {
        tryCatch({
          library(pkg, character.only = TRUE)
        }, error = function(e) {
          warning("Failed to load '", pkg, "': ", e$message)
        })
      }
    }
  }
  
  invisible(list(
    installed = installed,
    existing = existing,
    failed = failed
  ))
}

#' Null-coalescing operator (for older R versions)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Run auto-install if this script is sourced
# This will automatically install and load dependencies when the script is sourced
# Comment out the line below if you want manual control
# auto_install_dependencies(quiet = FALSE)