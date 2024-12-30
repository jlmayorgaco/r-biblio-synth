# helpers.R
library(ggplot2)

# Generate dynamic breaks
generate_breaks <- function(min_val, max_val, intervals) {
  breaks <- seq(min_val, max_val, by = ceiling((max_val - min_val) / intervals))
  return(breaks[breaks <= max_val])
}

# Parse parameters from a string
parse_params <- function(params_str) {
  params_list <- strsplit(params_str, ",")[[1]]
  params <- setNames(
    as.numeric(sapply(params_list, function(x) strsplit(trimws(x), " = ")[[1]][2])),
    sapply(params_list, function(x) strsplit(trimws(x), " = ")[[1]][1])
  )
  return(params)
}

# Format numbers for tables
format_number <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (abs(x) >= 1000) {
    # Format large numbers in scientific notation
    sign <- ifelse(x > 0, 1, -1)
    exponent <- floor(log10(abs(x)))
    mantissa <- sign * abs(x) / 10^exponent
    return(sprintf("%.1fx10^%d", mantissa, exponent))
  } else if (abs(x) >= 1) {
    return(formatC(x, format = "f", digits = 2))
  } else {
    return(formatC(x, format = "f", digits = 4))
  }
}
