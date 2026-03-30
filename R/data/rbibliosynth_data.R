# ============================================================================
# rbibliosynth_data.R - Embedded Sample Dataset for RBiblioSynth
# ============================================================================
# This file creates a comprehensive sample dataset for testing, demos, and vignettes.
# The dataset simulates a bibliometric analysis in the field of renewable energy.

#' @title Sample Bibliometric Dataset for Renewable Energy Research
#' @description A simulated bibliometric dataset representing publications in renewable energyresearch from 2010-2023.
#' @format A data frame with 500 observations and 20 variables:
#' \itemize{
#'   \item \code{AU}: Authors (semicolon-separated)
#'   \item \code{TI}: Title
#'   \item \code{PY}: Publication year
#'   \item \code{SO}: Source/Journal name
#'   \item \code{TC}: Times cited
#'   \item \code{DI}: DOI
#'   \item \code{AB}: Abstract
#'   \item \code{AU_CO}: Author countries (semicolon-separated)
#'   \item \code{DE}: Author keywords (semicolon-separated)
#'   \item \code{ID}: Keywords Plus (semicolon-separated)
#'   \item \code{DT}: Document type
#'   \item \code{LA}: Language
#'   \item \code{NR}: Number of references
#'   \item \code{PN}: Page count
#'   \item \code{DB}: Source database
#' }

"biblio_sample"

# ============================================================================
# Dataset Generation Function
# ============================================================================

#' Generate sample bibliometric dataset
#' @keywords internal
generate_biblio_sample <- function() {
  set.seed(42)
  n <- 500
  
  # Author pools by country
  authors <- list(
    USA = c("Smith J", "Johnson M", "Williams R", "Brown K", "Davis L", "Miller A", "Wilson T", "Moore E"),
    China = c("Wang Y", "Li X", "Zhang H", "Liu W", "Chen Z", "Yang M", "Huang G", "Zhou B"),
    UK = c("Taylor S", "Davies P", "Evans N", "Thomas R", "Johnson K", "Roberts M", "Walker J", "Wright B"),
    Germany = c("Mueller H", "Schmidt F", "Weber G", "Wagner K", "Becker L", "Hoffmann R", "Schulz M"),
    Japan = c("Tanaka K", "Suzuki Y", "Takahashi M", "Watanabe T", "Yamamoto H", "Nakamura J"),
    Brazil = c("Silva C", "Santos P", "Oliveira A", "Costa M", "Pereira R", "Rodrigues F"),
    India = c("Kumar S", "Sharma R", "Patel A", "Singh N", "Gupta V", "Verma P"),
    France = c("Martin L", "Bernard G", "Dubois R", "Thomas J", "Robert Y", "Richard M"),
    Australia = c("Brown D", "Jones M", "Miller K", "Davis R", "Wilson J", "Taylor P"),
    Canada = c("Tremblay M", "Gagnon L", "Roy P", "Gauthier R", "Lavoie J", "Belanger F")
  )
  
  countries <- names(authors)
  
  # Journal pool
  journals <- c(
    "Renewable Energy",
    "Solar Energy",
    "Wind Engineering",
    "Journal of Cleaner Production",
    "Energy Policy",
    "Applied Energy",
    "Energy Conversion and Management",
    "Renewable and Sustainable Energy Reviews",
    "International Journal of Energy Research",
    "Energy"
  )
  
  # Keywords pool
  keywords <- c(
    "solar energy", "photovoltaic", "wind power", "renewable energy",
    "energy storage", "battery", "fuel cell", "hydrogen",
    "sustainability", "carbon emissions", "energy efficiency",
    "smart grid", "microgrid", "energy management",
    "wind turbine", "solar panel", "biomass", "geothermal",
    "ocean energy", "hydropower", "nuclear energy",
    "energy policy", "climate change", "carbon footprint",
    "life cycle assessment", "energy economics"
  )
  
  # Document types
  doc_types <- c("article", "review", "conference paper", "book chapter", "letter")
  doc_probs <- c(0.65, 0.15, 0.12, 0.05, 0.03)
  
  # Generate records
  records <- vector("list", n)
  
  for (i in 1:n) {
    # Publication year (more recent = more publications)
    year_weights <- exp(seq(0, 1, length.out = 14))
    year_weights <- year_weights / sum(year_weights)
    year <- sample(2010:2023, 1, prob = year_weights)
    
    # Number of authors (1-8, skewed towards fewer)
    n_authors <- sample(1:8, 1, prob = c(0.15, 0.25, 0.25, 0.15, 0.10, 0.05, 0.03, 0.02))
    
    # Select countries for authors
    n_countries <- sample(1:min(4, n_authors), 1)
    selected_countries <- sample(countries, min(n_countries, length(countries)))
    
    # Generate authors
    author_list <- character(0)
    country_list <- character(0)
    
    for (j in 1:n_authors) {
      # Assign author to a country (may have multiple authors from same country)
      cntry <- sample(selected_countries, 1)
      auth_pool <- authors[[cntry]]
      author_list <- c(author_list, sample(auth_pool, 1))
      country_list <- c(country_list, cntry)
    }
    
    # Citation count (follows log-normal distribution, age-adjusted)
    age <- 2024 - year
    base_citations <- exp(rnorm(1, mean = log(10 + 5 * age), sd = 1))
    citations <- round(pmax(0, base_citations))
    
    # Journal
    journal <- sample(journals, 1)
    
    # Keywords (3-8 keywords)
    n_kw <- sample(3:8, 1)
    doc_keywords <- sample(keywords, min(n_kw, length(keywords)))
    
    # Document type
    dt <- sample(doc_types, 1, prob = doc_probs)
    
    # Title (realistic-sounding)
    title_parts <- c(
      paste(sample(c("Advances in", "Analysis of", "Development of", "Optimization of",
                      "Review of", "Assessment of", "Impact of", "Modeling of"), 1),
            sample(keywords, 1)),
      paste(sample(keywords, 1), "and", sample(keywords, 1)),
      paste(sample(c("A novel approach to", "Critical review of", "Comparative study of",
                      "Recent developments in", "Future prospects of"), 1),sample(keywords, 1))
    )
    title <- paste(sample(title_parts, 1), "in renewable energy")
    
    # Abstract (placeholder)
    abstract <- paste("This study investigates", paste(sample(keywords, 3), collapse = ", "),
                      ". Our analysis shows significant results.")
    
    # DOI
    doi <- paste0("10.", sample(1000:9999, 1), "/", sample(letters, 8, replace = TRUE), collapse = "")
    
    # Create record
    records[[i]] <- data.frame(
      AU = paste(unique(author_list), collapse = "; "),
      TI = title,
      PY = year,
      SO = journal,
      TC = citations,
      DI = doi,
      AB = abstract,
      AU_CO = paste(unique(country_list), collapse = "; "),
      DE = paste(doc_keywords, collapse = "; "),
      ID = paste(toupper(doc_keywords), collapse = "; "),
      DT = dt,
      LA = "English",
      NR = sample(5:80, 1),
      PN = sample(1:25, 1),
      DB = sample(c("SCOPUS", "WOS"), 1, prob = c(0.6, 0.4)),
      stringsAsFactors = FALSE
    )
  }
  
  # Combine all records
  df <- do.call(rbind, records)
  rownames(df) <- NULL
  
  # Add citations per year (age-adjusted citation metric)
  df$TC_PER_YEAR <- round(df$TC / (2024 - df$PY + 1), 2)
  
  # Add reference count if missing
  df$NR[is.na(df$NR)] <- sample(10:60, sum(is.na(df$NR)))
  
  df
}

# Generate dataset in memory
biblio_sample <- generate_biblio_sample()

# Also create annual production data
annual_sample <- aggregate(TC ~ PY, data = biblio_sample, FUN = length)
names(annual_sample) <- c("Year", "Articles")
annual_sample <- annual_sample[order(annual_sample$Year), ]

# Add cumulative
annual_sample$Cumulative <- cumsum(annual_sample$Articles)
annual_sample$Growth <- c(NA, diff(annual_sample$Articles) / head(annual_sample$Articles, -1) * 100)

# ============================================================================
# Sample Country Data for M3 Testing
# ============================================================================

#' @title Sample Country-Level Dataset for M3 Module
#' @description Aggregated country-level publication data derived from biblio_sample.
"country_sample"

generate_country_sample <- function(bib_data) {
  # Extract country-author counts
  country_records <- strsplit(bib_data$AU_CO, "; ")
  country_records <- lapply(country_records, function(x) unique(trimws(x)))
  
  # Count publications per country
  all_countries <- unlist(country_records)
  country_counts <- table(all_countries)
  
  # Create data frame
  df <- data.frame(
    country = names(country_counts),
    articles = as.numeric(country_counts),
    stringsAsFactors = FALSE
  )
  
  # Sort by articles
  df <- df[order(-df$articles), ]
  rownames(df) <- NULL
  
  # Add rank
  df$rank <- seq_len(nrow(df))
  
  # Add SCP/MCP (simplified)
  # SCP = single country publications
  # MCP = multi-country publications
  scp_counts <- sapply(country_records, function(x) if (length(x) == 1) x[1] else NA)
  mcp_records <- sapply(country_records, function(x) if (length(x) > 1) x else NA)
  
  scp_table <- table(na.omit(scp_counts))
  mcp_list <- unlist(mcp_records[!is.na(mcp_records)])
  mcp_table <- table(mcp_list)
  
  df$scp <- scp_table[df$country]
  df$scp[is.na(df$scp)] <- 0
  df$mcp <- mcp_table[df$country]
  df$mcp[is.na(df$mcp)] <- 0
  
  # MCP ratio
  df$mcp_ratio <- df$mcp / (df$scp + df$mcp)
  df$mcp_ratio[is.na(df$mcp_ratio)] <- 0
  
  # Average citations (simplified)
  avg_cit <- tapply(bib_data$TC, sapply(country_records, function(x) x[1]), mean)
  df$avg_citations <- avg_cit[df$country]
  df$avg_citations[is.na(df$avg_citations)] <- 0
  
  df
}

country_sample <- generate_country_sample(biblio_sample)
save(country_sample, file = "R/data/country_sample.rda", compress = "xz")

# ============================================================================
# Dataset Documentation
# ============================================================================

#' Sample Bibliometric Dataset
#'
#' Datasets for testing and demonstrating RBiblioSynth functions.
#'
#' @format
#' \itemize{
#'   \item \code{biblio_sample}: Full bibliographic dataset (500 records)
#'   \item \code{annual_sample}: Annualproduction time series (14 years)
#'   \item \code{country_sample}: Country-level aggregated data
#' }
#'
#' @source Simulated data for demonstration purposes.
#' @usage
#' data(biblio_sample)
#' data(annual_sample)
#' data(country_sample)
"rbibliosynth_data"