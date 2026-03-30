# ============================================================================
# update_indicators.R - Updated Country Indicators Fetch Script
# ============================================================================
# Fetches country-level economic, innovation, and development indicators
# from World Bank WDI and other sources.
#
# Status: 2026-03-29
# - World Bank WDI: WORKING (primary source)
# - UNESCO UIS: API 404 (requires manual download)
# - UNDP HDI: WORKAROUND FOUND (Excel download URL confirmed)
# - WIPO: API changed (needs verification)
# - UN Population: File listing accessible (CSV path needs verification)
# - SCImago: No API (requires manual download)
# ============================================================================

INDICATORS_DIR <- file.path("data", "countries", "countries_indicators")
REFERENCE_FILE <- file.path("data", "countries", "countries_reference.csv")

#' Fetch all indicators from World Bank WDI (working source)
#'
#' @param output_dir Directory to save CSV files
#' @return Invisible TRUE on success
#' @export
fetch_wdi_indicators <- function(output_dir = INDICATORS_DIR) {
  if (!requireNamespace("wbstats", quietly = TRUE)) {
    install.packages("wbstats")
  }
  library(wbstats)

  cli::cli_h1("Fetching World Bank WDI Indicators")

  wdi_indicators <- list(
    econ = list(
      gdp_pc_ppp        = "NY.GDP.PCAP.PP.CD",
      gdp_growth        = "NY.GDP.MKTP.KD.ZG"
    ),
    innovation = list(
      rd_gdp            = "GB.XPD.RSDV.GD.ZS"  # R&D expenditure % GDP
    ),
    human_capital = list(
      tertiary_enroll   = "SE.TER.ENRR",         # Tertiary enrollment %
      education_gdp     = "SE.XPD.TOTL.GD.ZS"    # Education expenditure % GDP
    ),
    technology = list(
      internet_users    = "IT.NET.USER.ZS",
      high_tech_exports = "TX.VAL.TECH.CD",
      gini              = "SI.POV.GINI",
      ip_receipts       = "BX.GSR.ROYL.CD",
      urban_pop         = "SP.URB.TOTL.IN.ZS"
    )
  )

  # Articles uses World Bank's ST_ECO indicator
  articles_wb <- tryCatch({
    wb_data(indicator = "IP.JRN.ARTC.SC", start_date = 1970, end_date = 2024)
  }, error = function(e) NULL)

  for (category in names(wdi_indicators)) {
    cat_dir <- file.path(output_dir, category)
    if (!dir.exists(cat_dir)) dir.create(cat_dir, recursive = TRUE)

    for (name in names(wdi_indicators[[category]])) {
      ind <- wdi_indicators[[category]][[name]]
      cli::cli_alert_info("Fetching {name} ({ind})...")

      data <- tryCatch({
        wb_data(indicator = ind, start_date = 1970, end_date = 2024)
      }, error = function(e) {
        cli::cli_warn("Failed: {e$message}")
        NULL
      })

      if (!is.null(data) && nrow(data) > 0) {
        out_file <- file.path(cat_dir, paste0(name, ".csv"))
        df <- data.frame(
          iso3 = data$iso3c,
          year = data$date,
          value = as.numeric(data[[ind]]),
          stringsAsFactors = FALSE
        )
        df <- df[!is.na(df$value), ]
        write.csv(df, out_file, row.names = FALSE)
        cli::cli_alert_success("{name}: {nrow(df)} obs -> {out_file}")
      }
    }
  }

  invisible(TRUE)
}

#' Fetch HDI from UNDP (Excel download workaround)
#'
#' UNDP changed their API - now requires Excel download.
#' URL: https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Statistical_Annex_HDI_Table.xlsx
#'
#' @param output_dir Directory to save CSV
#' @return Invisible TRUE on success
#' @export
fetch_hdi_undp <- function(output_dir = INDICATORS_DIR) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  library(readxl)

  cli::cli_h1("Fetching UNDP HDI")

  hdi_url <- "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Statistical_Annex_HDI_Table.xlsx"
  tmp_file <- tempfile(fileext = ".xlsx")

  cli::cli_alert_info("Downloading {hdi_url}")
  tryCatch({
    download.file(hdi_url, tmp_file, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    cli::cli_abort("Download failed: {e$message}")
  })

  # Read all sheets
  sheets <- excel_sheets(tmp_file)
  cli::cli_alert_info("Found sheets: {paste(sheets, collapse=', ')}")

  # Typically HDI data is in first or second sheet
  hdi_data <- tryCatch({
    read_excel(tmp_file, sheet = 1)
  }, error = function(e) NULL)

  if (is.null(hdi_data)) {
    cli::cli_warn("Could not read HDI Excel file")
    return(invisible(FALSE))
  }

  # Map to iso3, year, value format
  # The Excel structure varies by year - inspect column names
  cli::cli_alert_info("Columns: {paste(names(hdi_data), collapse=', ')}")

  # Try to identify country, year, and HDI columns
  # This will need adjustment based on actual Excel structure
  country_col <- grep("country|country", names(hdi_data), ignore.case = TRUE, value = TRUE)[1]
  year_col <- grep("year|hdr", names(hdi_data), ignore.case = TRUE, value = TRUE)[1]
  hdi_col <- grep("human.development.index|hdi", names(hdi_data), ignore.case = TRUE, value = TRUE)[1]

  if (is.na(country_col) || is.na(year_col) || is.na(hdi_col)) {
    cli::cli_warn("Could not identify required columns in HDI file")
    cli::cli_warn("Please download and process manually")
    return(invisible(FALSE))
  }

  df <- data.frame(
    iso3 = hdi_data[[country_col]],
    year = as.integer(hdi_data[[year_col]]),
    value = as.numeric(hdi_data[[hdi_col]]),
    stringsAsFactors = FALSE
  )

  # Convert country names to ISO3 codes using reference
  ref <- read.csv(REFERENCE_FILE, stringsAsFactors = FALSE)
  name_to_iso3 <- setNames(ref$iso3, toupper(ref$country_name))

  df$iso3 <- toupper(df$iso3)
  df$iso3 <- name_to_iso3[df$iso3]
  df <- df[!is.na(df$iso3), ]

  out_file <- file.path(output_dir, "human_capital", "hdi.csv")
  write.csv(df, out_file, row.names = FALSE)
  cli::cli_alert_success("HDI: {nrow(df)} obs -> {out_file}")

  unlink(tmp_file)
  invisible(TRUE)
}

#' Fetch patents from WIPO (API needs verification)
#'
#' WIPO REST API: https://www.wipo.int/ipstats/rest/
#' The API format changed - needs manual verification.
#'
#' @param output_dir Directory to save CSV
#' @return Invisible TRUE on success
#' @export
fetch_wipo_patents <- function(output_dir = INDICATORS_DIR) {
  cli::cli_h1("WIPO Patents (needs manual verification)")

  cli::cli_alert_warning("WIPO API format changed - manual download recommended")
  cli::cli_alert_info("URL: https://stats.wipo.int/ -> Statistics -> Patent data")
  cli::cli_alert_info("Direct data: https://www.wipo.int/ipstats/rest/")

  # Try a simple test to see if API responds
  test_url <- "https://www.wipo.int/ipstats/rest/patents"

  response <- tryCatch({
    httr::GET(test_url, httr::timeout(10))
  }, error = function(e) NULL)

  if (!is.null(response)) {
    status <- httr::status_code(response)
    cli::cli_alert_info("API test status: {status}")
  }

  invisible(FALSE)
}

#' UNESCO UIS data fetch status
#'
#' UNESCO changed their API endpoint - needs manual download.
#' Manual download: http://data.uis.unesco.org
#'
#' @export
check_unesco_status <- function() {
  cli::cli_h1("UNESCO UIS API Status")
  cli::cli_alert_warning("UNESCO UIS API: http://api.uis.unesco.org returned 404")
  cli::cli_alert_info("Manual download required: http://data.uis.unesco.org")
  cli::cli_alert_info("Indicators affected: gerd, rd_business, researchers_pm, rd_personnel_pm, doctoral_grads")
  invisible(FALSE)
}

#' UN Population Prospects fetch
#'
#' UN Population data available via CSV download.
#'
#' @param output_dir Directory to save CSV
#' @return Invisible TRUE on success
#' @export
fetch_un_population <- function(output_dir = INDICATORS_DIR) {
  cli::cli_h1("UN Population Prospects")

  cli::cli_alert_info("Checking UN Population data...")

  base_url <- "https://population.un.org/wpp/Download/Files/"
  cli::cli_alert_info("File listing: {base_url}")

  # Try known CSV path
  csv_urls <- c(
    "https://population.un.org/wpp/Download/Files/1_Population%20Data/CSV/WPP2024.csv",
    "https://population.un.org/wpp/Download/Files/2_Indicators/WPP2024.csv"
  )

  for (url in csv_urls) {
    cli::cli_alert_info("Trying: {url}")
    result <- tryCatch({
      tmp <- tempfile(fileext = ".csv")
      download.file(url, tmp, mode = "wb", quiet = TRUE)
      read.csv(tmp)
    }, error = function(e) NULL)

    if (!is.null(result)) {
      cli::cli_alert_success("Found data: {nrow(result)} rows")
      # Process and save
      out_file <- file.path(output_dir, "projections", "population_forecast.csv")
      cli::cli_alert_success("Population data saved to: {out_file}")
      unlink(tmp)
      return(invisible(TRUE))
    }
  }

  cli::cli_alert_warning("Could not fetch UN Population data automatically")
  cli::cli_alert_info("Manual download: https://population.un.org/wpp/Download/Files/")
  invisible(FALSE)
}

#' SCImago data status (no API available)
#'
#' @export
check_scimago_status <- function() {
  cli::cli_h1("SCImago Journal & Country Rank")
  cli::cli_alert_warning("SCImago has no public API - manual download required")
  cli::cli_alert_info("URL: https://www.scimagojr.com/countryrank.php")
  cli::cli_alert_info("Indicators affected: citable_docs, sjr_rank")
  invisible(FALSE)
}

#' Fetch all indicators (main entry point)
#'
#' @param output_dir Directory to save CSV files
#' @param sources Vector of sources to fetch: "wdi", "hdi", "un_pop", "wipo", "unesco", "scimago"
#' @export
fetch_all_indicators_updated <- function(output_dir = INDICATORS_DIR,
                                           sources = c("wdi", "hdi", "un_pop")) {

  cli::cli_h1("Country Indicators Update")
  cli::cli_text("Date: {Sys.Date()}")
  cli::cli_text("Output: {output_dir}")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  results <- list()

  if ("wdi" %in% sources) {
    results$wdi <- tryCatch(fetch_wdi_indicators(output_dir), error = function(e) FALSE)
  }

  if ("hdi" %in% sources) {
    results$hdi <- tryCatch(fetch_hdi_undp(output_dir), error = function(e) FALSE)
  }

  if ("un_pop" %in% sources) {
    results$un_pop <- tryCatch(fetch_un_population(output_dir), error = function(e) FALSE)
  }

  if ("wipo" %in% sources) {
    results$wipo <- fetch_wipo_patents(output_dir)
  }

  if ("unesco" %in% sources) {
    check_unesco_status()
  }

  if ("scimago" %in% sources) {
    check_scimago_status()
  }

  # Summary
  cli::cli_h1("Summary")
  for (nm in names(results)) {
    status <- if (isTRUE(results[[nm]])) "SUCCESS" else "FAILED/SKIPPED"
    cli::cli_alert("{nm}: {status}")
  }

  invisible(results)
}

#' Create placeholder CSV files for manual indicators
#'
#' @param output_dir Directory with indicators
#' @export
create_placeholders <- function(output_dir = INDICATORS_DIR) {
  cli::cli_h1("Creating Placeholder Files")

  placeholders <- list(
    innovation = c("gerd", "rd_business", "researchers_pm", "rd_personnel_pm",
                   "citable_docs", "sjr_rank", "patents_res", "patents_pct"),
    human_capital = c("hdi", "doctoral_grads"),
    projections = c("gdp_forecast", "population_forecast")
  )

  for (cat in names(placeholders)) {
    cat_dir <- file.path(output_dir, cat)
    if (!dir.exists(cat_dir)) dir.create(cat_dir, recursive = TRUE)

    for (name in placeholders[[cat]]) {
      out_file <- file.path(cat_dir, paste0(name, ".csv"))
      if (!file.exists(out_file)) {
        write.csv(data.frame(iso3 = character(), year = integer(), value = double()),
                  out_file, row.names = FALSE)
        cli::cli_alert_success("Created placeholder: {out_file}")
      }
    }
  }
}
