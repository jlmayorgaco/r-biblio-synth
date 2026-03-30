# ============================================================================
# m3_compute_regional.R - Regional Aggregation and Analysis
# ============================================================================
# Regional grouping, continent analysis, regional comparisons

#' Regional country groupings
REGIONAL_GROUPS <- list(
  # Continents
  "Africa" = c("ALGERIA", "ANGOLA", "BENIN", "BOTSWANA", "BURKINA FASO", "BURUNDI",
               "CAMEROON", "CAPE VERDE", "CENTRAL AFRICAN REPUBLIC", "CHAD", "COMOROS",
               "CONGO", "DEMOCRATIC REPUBLIC OF THE CONGO", "DJIBOUTI", "EGYPT",
               "EQUATORIAL GUINEA", "ERITREA", "ESWATINI", "ETHIOPIA", "GABON",
               "GAMBIA", "GHANA", "GUINEA", "GUINEA-BISSAU", "IVORY COAST", "KENYA",
               "LESOTHO", "LIBERIA", "LIBYA", "MADAGASCAR", "MALAWI", "MALI",
               "MAURITANIA", "MAURITIUS", "MOROCCO", "MOZAMBIQUE", "NAMIBIA", "NIGER",
               "NIGERIA", "RWANDA", "SAO TOME AND PRINCIPE", "SENEGAL", "SEYCHELLES",
               "SIERRA LEONE", "SOMALIA", "SOUTH AFRICA", "SOUTH SUDAN", "SUDAN",
               "TANZANIA", "TOGO", "TUNISIA", "UGANDA", "ZAMBIA", "ZIMBABWE"),
  
  "Asia" = c("AFGHANISTAN", "ARMENIA", "AZERBAIJAN", "BAHRAIN", "BANGLADESH", "BHUTAN",
             "BRUNEI", "CAMBODIA", "CHINA", "CYPRUS", "GEORGIA", "INDIA", "INDONESIA",
             "IRAN", "IRAQ", "ISRAEL", "JAPAN", "JORDAN", "KAZAKHSTAN", "KUWAIT",
             "KYRGYZSTAN", "LAOS", "LEBANON", "MALAYSIA", "MALDIVES", "MONGOLIA",
             "MYANMAR", "NEPAL", "NORTH KOREA", "OMAN", "PAKISTAN", "PHILIPPINES",
             "QATAR", "SAUDI ARABIA", "SINGAPORE", "SOUTH KOREA", "SRI LANKA", "SYRIA",
             "TAJIKISTAN", "THAILAND", "TIMOR-LESTE", "TURKEY", "TURKMENISTAN",
             "UNITED ARAB EMIRATES", "UZBEKISTAN", "VIETNAM", "YEMEN", "HONG KONG",
             "MACAU", "TAIWAN"),
  
  "Europe" = c("ALBANIA", "ANDORRA", "AUSTRIA", "BELARUS", "BELGIUM", "BOSNIA AND HERZEGOVINA",
               "BULGARIA", "CROATIA", "CZECHIA", "DENMARK", "ESTONIA", "FINLAND",
               "FRANCE", "GERMANY", "GREECE", "HUNGARY", "ICELAND", "IRELAND", "ITALY",
               "LATVIA", "LIECHTENSTEIN", "LITHUANIA", "LUXEMBOURG", "MALTA", "MOLDOVA",
               "MONACO", "MONTENEGRO", "NETHERLANDS", "NORTH MACEDONIA", "NORWAY",
               "POLAND", "PORTUGAL", "ROMANIA", "RUSSIA", "SAN MARINO", "SERBIA",
               "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN", "SWITZERLAND", "UKRAINE",
               "UNITED KINGDOM", "VATICAN CITY"),
  
  "North America" = c("ANTIGUA AND BARBUDA", "BAHAMAS", "BARBADOS", "BELIZE", "CANADA",
                      "COSTA RICA", "CUBA", "DOMINICA", "DOMINICAN REPUBLIC", "EL SALVADOR",
                      "GRENADA", "GUATEMALA", "HAITI", "HONDURAS", "JAMAICA", "MEXICO",
                      "NICARAGUA", "PANAMA", "SAINT KITTS AND NEVIS", "SAINT LUCIA",
                      "SAINT VINCENT AND THE GRENADINES", "TRINIDAD AND TOBAGO",
                      "UNITED STATES"),
  
  "South America" = c("ARGENTINA", "BOLIVIA", "BRAZIL", "CHILE", "COLOMBIA", "ECUADOR",
                      "GUYANA", "PARAGUAY", "PERU", "SURINAME", "URUGUAY", "VENEZUELA"),
  
  "Oceania" = c("AUSTRALIA", "FIJI", "KIRIBATI", "MARSHALL ISLANDS", "MICRONESIA",
                "NAURU", "NEW ZEALAND", "PALAU", "PAPUA NEW GUINEA", "SAMOA",
                "SOLOMON ISLANDS", "TONGA", "TUVALU", "VANUATU"),
  
  # Economic groups
  "OECD" = c("AUSTRALIA", "AUSTRIA", "BELGIUM", "CANADA", "CHILE", "COLOMBIA",
             "CZECHIA", "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY",
             "GREECE", "HUNGARY", "ICELAND", "IRELAND", "ISRAEL", "ITALY", "JAPAN",
             "SOUTH KOREA", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MEXICO",
             "NETHERLANDS", "NEW ZEALAND", "NORWAY", "POLAND", "PORTUGAL",
             "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN", "SWITZERLAND", "TURKEY",
             "UNITED KINGDOM", "UNITED STATES"),
  
  "BRICS" = c("BRAZIL", "RUSSIA", "INDIA", "CHINA", "SOUTH AFRICA"),
  
  "G7" = c("CANADA", "FRANCE", "GERMANY", "ITALY", "JAPAN", "UNITED KINGDOM", "UNITED STATES"),
  
  "G20" = c("ARGENTINA", "AUSTRALIA", "BRAZIL", "CANADA", "CHINA", "FRANCE",
            "GERMANY", "INDIA", "INDONESIA", "ITALY", "JAPAN", "MEXICO",
            "RUSSIA", "SAUDI ARABIA", "SOUTH AFRICA", "SOUTH KOREA", "TURKEY",
            "UNITED KINGDOM", "UNITED STATES", "EUROPEAN UNION"),
  
  "ASEAN" = c("BRUNEI", "CAMBODIA", "INDONESIA", "LAOS", "MALAYSIA", "MYANMAR",
              "PHILIPPINES", "SINGAPORE", "THAILAND", "VIETNAM"),
  
  "EU" = c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECHIA",
           "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE",
           "HUNGARY", "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG",
           "MALTA", "NETHERLANDS", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA",
           "SLOVENIA", "SPAIN", "SWEDEN")
)

#' Aggregate countries by region
#'
#' @param country_data Data frame with country, value columns
#' @param group_by Which regional grouping ("continent", "OECD", "BRICS", etc.)
#' @return List with aggregated data
#' @export
m3_aggregate_by_region <- function(country_data, group_by = "continent") {
  if (!is.data.frame(country_data) || !"country" %in% names(country_data)) {
    return(list(status = "error: invalid input"))
  }
  
  # Normalize country names
  country_data$country_norm <- toupper(trimws(as.character(country_data$country)))
  
  # Select grouping
  if (group_by == "continent") {
    groups <- REGIONAL_GROUPS[c("Africa", "Asia", "Europe", "North America", 
                                "South America", "Oceania")]
  } else if (group_by %in% names(REGIONAL_GROUPS)) {
    groups <- REGIONAL_GROUPS[group_by]
  } else {
    return(list(status = "error: unknown grouping"))
  }
  
  # Aggregate
  aggregated <- list()
  unassigned <- country_data
  
  for (region_name in names(groups)) {
    countries_in_region <- groups[[region_name]]
    
    # Find matches
    matches <- country_data$country_norm %in% countries_in_region
    region_data <- country_data[matches, ]
    
    if (nrow(region_data) > 0) {
      # Aggregate metrics
      region_summary <- list(
        region = region_name,
        n_countries = nrow(region_data),
        countries = unique(region_data$country),
        total_production = sum(region_data$value, na.rm = TRUE)
      )
      
      # Add other numeric columns
      numeric_cols <- sapply(country_data, is.numeric)
      numeric_cols <- setdiff(names(numeric_cols)[numeric_cols], "value")
      
      for (col in numeric_cols) {
        if (col %in% names(region_data)) {
          region_summary[[paste0("total_", col)]] <- sum(region_data[[col]], na.rm = TRUE)
          region_summary[[paste0("mean_", col)]] <- mean(region_data[[col]], na.rm = TRUE)
        }
      }
      
      aggregated[[region_name]] <- region_summary
      unassigned <- unassigned[!matches, ]
    }
  }
  
  # Create summary data frame
  summary_df <- do.call(rbind, lapply(aggregated, function(x) {
    data.frame(
      region = x$region,
      n_countries = x$n_countries,
      total_production = x$total_production,
      stringsAsFactors = FALSE
    )
  }))
  
  # Calculate shares
  total_all <- sum(summary_df$total_production)
  summary_df$share <- summary_df$total_production / total_all * 100
  
  list(
    aggregated = aggregated,
    summary = summary_df,
    unassigned = if (nrow(unassigned) > 0) unassigned$country else character(0),
    n_unassigned = nrow(unassigned),
    n_regions = length(aggregated),
    group_by = group_by,
    status = "success"
  )
}

#' Compute regional comparisons
#'
#' @param regional_data Output from m3_aggregate_by_region
#' @return List with comparative statistics
#' @export
m3_regional_comparison <- function(regional_data) {
  if (is.null(regional_data) || regional_data$status != "success") {
    return(list(status = "error: invalid input"))
  }
  
  summary <- regional_data$summary
  
  if (nrow(summary) < 2) {
    return(list(status = "error: need at least 2 regions"))
  }
  
  # Top region
  top_region <- summary$region[which.max(summary$total_production)]
  top_share <- max(summary$share)
  
  # Regional concentration
  gini_regional <- safe_gini(summary$total_production)
  
  # Statistical tests
  # Chi-square test for equal distribution
  expected <- rep(sum(summary$total_production) / nrow(summary), nrow(summary))
  chi_test <- chisq.test(summary$total_production, p = expected / sum(expected))
  
  # Growth comparison (if year data available)
  growth_comparison <- list()
  
  list(
    top_region = top_region,
    top_share = top_share,
    regional_concentration = gini_regional,
    chi_square_test = list(
      statistic = chi_test$statistic,
      p_value = chi_test$p.value,
      significant = chi_test$p.value < 0.05
    ),
    dominance_ratio = max(summary$total_production) / min(summary$total_production[summary$total_production > 0]),
    n_regions = nrow(summary),
    status = "success"
  )
}

#' Safe Gini coefficient
#' @keywords internal
safe_gini <- function(x) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  if (n < 2) return(0)
  
  x <- sort(x)
  index <- 1:n
  (n + 1 - 2 * sum((n + 1 - index) * x) / sum(x)) / n
}

`%||%` <- function(a, b) if (!is.null(a)) a else b