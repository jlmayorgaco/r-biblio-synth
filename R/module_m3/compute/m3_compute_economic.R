# ============================================================================
# m3_compute_economic.R - Economic/Development Correlation
# ============================================================================
# GDP, HDI, R&D expenditure integration and correlation analysis

#' World development indicators (sample data for when API unavailable)
WORLD_INDICATORS <- list(
  "UNITED STATES" = list(gdp_usd = 25462700, hdi = 0.921, population = 331900000, rd_pct = 3.46),
  "CHINA" = list(gdp_usd = 17963200, hdi = 0.768, population = 1412000000, rd_pct = 2.40),
  "JAPAN" = list(gdp_usd = 4231140, hdi = 0.925, population = 125800000, rd_pct = 3.30),
  "GERMANY" = list(gdp_usd = 4072190, hdi = 0.942, population = 83200000, rd_pct = 3.13),
  "INDIA" = list(gdp_usd = 3385090, hdi = 0.633, population = 1408000000, rd_pct = 0.66),
  "UNITED KINGDOM" = list(gdp_usd = 3070670, hdi = 0.929, population = 67500000, rd_pct = 1.71),
  "FRANCE" = list(gdp_usd = 2782910, hdi = 0.903, population = 67700000, rd_pct = 2.22),
  "ITALY" = list(gdp_usd = 2010430, hdi = 0.895, population = 59100000, rd_pct = 1.53),
  "CANADA" = list(gdp_usd = 2139840, hdi = 0.936, population = 38200000, rd_pct = 1.70),
  "SOUTH KOREA" = list(gdp_usd = 1665250, hdi = 0.925, population = 51800000, rd_pct = 4.81),
  "RUSSIA" = list(gdp_usd = 1778780, hdi = 0.822, population = 144100000, rd_pct = 1.04),
  "BRAZIL" = list(gdp_usd = 1920100, hdi = 0.754, population = 214000000, rd_pct = 1.21),
  "AUSTRALIA" = list(gdp_usd = 1675420, hdi = 0.951, population = 25900000, rd_pct = 1.83),
  "SPAIN" = list(gdp_usd = 1397510, hdi = 0.905, population = 47400000, rd_pct = 1.44),
  "MEXICO" = list(gdp_usd = 1322490, hdi = 0.758, population = 128900000, rd_pct = 0.32),
  "INDONESIA" = list(gdp_usd = 1319100, hdi = 0.705, population = 273800000, rd_pct = 0.28),
  "NETHERLANDS" = list(gdp_usd = 991110, hdi = 0.941, population = 17500000, rd_pct = 2.29),
  "SAUDI ARABIA" = list(gdp_usd = 833540, hdi = 0.875, population = 35000000, rd_pct = 0.46),
  "TURKEY" = list(gdp_usd = 905990, hdi = 0.838, population = 84300000, rd_pct = 1.09),
  "SWITZERLAND" = list(gdp_usd = 807710, hdi = 0.962, population = 8700000, rd_pct = 3.37),
  "POLAND" = list(gdp_usd = 688180, hdi = 0.876, population = 37800000, rd_pct = 1.44),
  "SWEDEN" = list(gdp_usd = 585940, hdi = 0.947, population = 10400000, rd_pct = 3.53),
  "BELGIUM" = list(gdp_usd = 578600, hdi = 0.937, population = 11600000, rd_pct = 3.14),
  "THAILAND" = list(gdp_usd = 495340, hdi = 0.800, population = 70000000, rd_pct = 1.21),
  "IRAN" = list(gdp_usd = 388120, hdi = 0.774, population = 85100000, rd_pct = 0.83),
  "AUSTRIA" = list(gdp_usd = 471400, hdi = 0.916, population = 9000000, rd_pct = 3.20),
  "NORWAY" = list(gdp_usd = 482440, hdi = 0.961, population = 5400000, rd_pct = 2.09),
  "ISRAEL" = list(gdp_usd = 520700, hdi = 0.919, population = 9200000, rd_pct = 5.44),
  "SINGAPORE" = list(gdp_usd = 397000, hdi = 0.939, population = 5500000, rd_pct = 2.16),
  "MALAYSIA" = list(gdp_usd = 372700, hdi = 0.803, population = 32700000, rd_pct = 0.95),
  "PHILIPPINES" = list(gdp_usd = 404280, hdi = 0.699, population = 110000000, rd_pct = 0.32),
  "SOUTH AFRICA" = list(gdp_usd = 405270, hdi = 0.713, population = 59300000, rd_pct = 0.73),
  "DENMARK" = list(gdp_usd = 395400, hdi = 0.948, population = 5800000, rd_pct = 2.85),
  "FINLAND" = list(gdp_usd = 280830, hdi = 0.940, population = 5500000, rd_pct = 2.94),
  "VIETNAM" = list(gdp_usd = 408800, hdi = 0.703, population = 98200000, rd_pct = 0.53),
  "CZECHIA" = list(gdp_usd = 290920, hdi = 0.889, population = 10500000, rd_pct = 2.00),
  "PORTUGAL" = list(gdp_usd = 251920, hdi = 0.866, population = 10300000, rd_pct = 1.54),
  "NEW ZEALAND" = list(gdp_usd = 247230, hdi = 0.937, population = 5100000, rd_pct = 1.46),
  "GREECE" = list(gdp_usd = 219070, hdi = 0.887, population = 10400000, rd_pct = 1.38),
  "IRELAND" = list(gdp_usd = 516150, hdi = 0.955, population = 5000000, rd_pct = 1.03),
  "HUNGARY" = list(gdp_usd = 178790, hdi = 0.846, population = 9700000, rd_pct = 1.64),
  "ROMANIA" = list(gdp_usd = 301260, hdi = 0.821, population = 19100000, rd_pct = 0.47),
  "CHILE" = list(gdp_usd = 301030, hdi = 0.855, population = 19500000, rd_pct = 0.35),
  "COLOMBIA" = list(gdp_usd = 314460, hdi = 0.752, population = 51900000, rd_pct = 0.29),
  "EGYPT" = list(gdp_usd = 404140, hdi = 0.731, population = 102300000, rd_pct = 0.72),
  "PAKISTAN" = list(gdp_usd = 348260, hdi = 0.544, population = 220900000, rd_pct = 0.24),
  "ARGENTINA" = list(gdp_usd = 487230, hdi = 0.842, population = 45400000, rd_pct = 0.52),
  "BANGLADESH" = list(gdp_usd = 416270, hdi = 0.661, population = 169400000, rd_pct = 0.30),
  "NIGERIA" = list(gdp_usd = 440780, hdi = 0.535, population = 211400000, rd_pct = 0.20),
  "UAE" = list(gdp_usd = 507540, hdi = 0.911, population = 9900000, rd_pct = 1.50),
  "UKRAINE" = list(gdp_usd = 200090, hdi = 0.773, population = 44100000, rd_pct = 0.48),
  "KENYA" = list(gdp_usd = 110350, hdi = 0.575, population = 54000000, rd_pct = 0.79),
  "MOROCCO" = list(gdp_usd = 132640, hdi = 0.683, population = 37100000, rd_pct = 0.72),
  "ETHIOPIA" = list(gdp_usd = 111270, hdi = 0.498, population = 118000000, rd_pct = 0.35),
  "PERU" = list(gdp_usd = 223250, hdi = 0.762, population = 33400000, rd_pct = 0.13)
)

#' Compute economic/development correlations
#'
#' @param country_data Data frame with country, production columns
#' @param config Configuration list
#' @return List with correlation results
#' @export
m3_compute_economic_correlation <- function(country_data, config = biblio_config()) {
  if (!is.data.frame(country_data) || !"country" %in% names(country_data)) {
    return(list(status = "error: invalid input"))
  }
  
  # Normalize country names
  country_data$country_norm <- toupper(trimws(as.character(country_data$country)))
  
  # Match with indicators
  matched_data <- match_indicators(country_data)
  
  if (nrow(matched_data) < 5) {
    return(list(status = "error: insufficient country matches"))
  }
  
  # Compute correlations
  correlations <- compute_correlations(matched_data)
  
  # Per capita analysis
  per_capita <- compute_per_capita_metrics(matched_data)
  
  # Efficiency analysis
  efficiency <- compute_research_efficiency(matched_data)
  
  # Regression analysis
  regression <- compute_development_regression(matched_data)
  
  list(
    matched_data = matched_data,
    n_matched = nrow(matched_data),
    n_total = nrow(country_data),
    correlations = correlations,
    per_capita = per_capita,
    efficiency = efficiency,
    regression = regression,
    status = "success"
  )
}

#' Match country data with world indicators
#' @keywords internal
match_indicators <- function(country_data) {
  matched <- data.frame(
    country = character(0),
    production = numeric(0),
    gdp_usd = numeric(0),
    hdi = numeric(0),
    population = numeric(0),
    rd_pct = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(country_data))) {
    country_norm <- country_data$country_norm[i]
    
    if (country_norm %in% names(WORLD_INDICATORS)) {
      indicators <- WORLD_INDICATORS[[country_norm]]
      
      matched <- rbind(matched, data.frame(
        country = country_data$country[i],
        production = country_data$article_count[i],
        gdp_usd = indicators$gdp_usd,
        hdi = indicators$hdi,
        population = indicators$population,
        rd_pct = indicators$rd_pct,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  matched
}

#' Compute correlations with development indicators
#' @keywords internal
compute_correlations <- function(matched_data) {
  cors <- list()
  
  # GDP correlation
  if (sum(!is.na(matched_data$gdp_usd)) >= 5) {
    cors$gdp <- tryCatch({
      cor.test(matched_data$production, matched_data$gdp_usd, method = "spearman", exact = FALSE)
    }, error = function(e) list(estimate = NA, p.value = NA))
  }
  
  # HDI correlation
  if (sum(!is.na(matched_data$hdi)) >= 5) {
    cors$hdi <- tryCatch({
      cor.test(matched_data$production, matched_data$hdi, method = "spearman", exact = FALSE)
    }, error = function(e) list(estimate = NA, p.value = NA))
  }
  
  # Population correlation
  if (sum(!is.na(matched_data$population)) >= 5) {
    cors$population <- tryCatch({
      cor.test(matched_data$production, matched_data$population, method = "spearman", exact = FALSE)
    }, error = function(e) list(estimate = NA, p.value = NA))
  }
  
  # R&D expenditure correlation
  if (sum(!is.na(matched_data$rd_pct)) >= 5) {
    cors$rd <- tryCatch({
      cor.test(matched_data$production, matched_data$rd_pct, method = "spearman", exact = FALSE)
    }, error = function(e) list(estimate = NA, p.value = NA))
  }
  
  cors
}

#' Compute per capita metrics
#' @keywords internal
compute_per_capita_metrics <- function(matched_data) {
  if (!"population" %in% names(matched_data)) {
    return(list(status = "error: no population data"))
  }
  
  matched_data$production_per_capita <- matched_data$production / matched_data$population
  matched_data$production_per_100k <- matched_data$production_per_capita * 100000
  
  matched_data$production_per_gdp <- matched_data$production / (matched_data$gdp_usd / 1e9)
  
  # Rank by per capita
  matched_data$rank_per_capita <- rank(-matched_data$production_per_capita)
  matched_data$rank_absolute <- rank(-matched_data$production)
  
  # Efficiency ratio (per capita rank / absolute rank)
  matched_data$efficiency_ratio <- matched_data$rank_per_capita / matched_data$rank_absolute
  
  list(
    data = matched_data,
    top_per_capita = head(matched_data[order(-matched_data$production_per_capita), ], 10),
    top_per_gdp = head(matched_data[order(-matched_data$production_per_gdp), ], 10),
    most_efficient = head(matched_data[order(matched_data$efficiency_ratio), ], 10),
    status = "success"
  )
}

#' Compute research efficiency analysis
#' @keywords internal
compute_research_efficiency <- function(matched_data) {
  if (!"rd_pct" %in% names(matched_data)) {
    return(list(status = "error: no R&D data"))
  }
  
  # R&D adjusted production
  rd_adjusted <- matched_data$rd_pct * (matched_data$gdp_usd / 1e9) / 100
  matched_data$rd_usd_billions <- rd_adjusted
  matched_data$production_per_rd_billion <- matched_data$production / rd_adjusted
  
  # DEA-like efficiency (simplified)
  # Best performing countries on efficiency frontier
  efficiency_score <- matched_data$production / (matched_data$rd_usd_billions + 0.01)
  matched_data$efficiency_score <- efficiency_score
  
  list(
    data = matched_data,
    most_efficient = head(matched_data[order(-matched_data$efficiency_score), ], 10),
    least_efficient = head(matched_data[order(matched_data$efficiency_score), ], 10),
    mean_efficiency = mean(matched_data$efficiency_score, na.rm = TRUE),
    status = "success"
  )
}

#' Compute development regression
#' @keywords internal
compute_development_regression <- function(matched_data) {
  # Log-transform production
  log_prod <- log1p(matched_data$production)
  
  # Multiple regression: production ~ GDP + HDI + population
  fit <- tryCatch({
    lm(log_prod ~ log(gdp_usd) + hdi + log(population), data = matched_data)
  }, error = function(e) NULL)
  
  if (is.null(fit)) {
    return(list(status = "error: regression failed"))
  }
  
  summary_fit <- summary(fit)
  
  list(
    coefficients = coef(fit),
    R_squared = summary_fit$r.squared,
    adj_R_squared = summary_fit$adj.r.squared,
    p_values = summary_fit$coefficients[, 4],
    interpretation = interpret_development_regression(coef(fit), summary_fit$r.squared),
    status = "success"
  )
}

#' Interpret development regression
#' @keywords internal
interpret_development_regression <- function(coefs, r_sq) {
  gdp_effect <- coefs[2]
  hdi_effect <- coefs[3]
  pop_effect <- coefs[4]
  
  interpretation <- sprintf(
    "Development explains %.1f%% of production variance. ",
    r_sq * 100
  )
  
  if (abs(gdp_effect) > abs(pop_effect)) {
    interpretation <- paste0(interpretation, "GDP is the strongest predictor. ")
  } else {
    interpretation <- paste0(interpretation, "Population is the strongest predictor. ")
  }
  
  if (hdi_effect > 0 && hdi_effect < 0.05) {
    interpretation <- paste0(interpretation, "HDI has modest positive effect.")
  }
  
  interpretation
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
