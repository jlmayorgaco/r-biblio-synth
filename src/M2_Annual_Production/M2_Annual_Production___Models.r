# Load necessary libraries
library(stats)

# Function to fit a linear model
fit_linear_model <- function(data) {
  lm(Articles ~ Year, data = data)
}

# Function to fit a polynomial model
fit_polynomial_model <- function(data, degree) {
  lm(Articles ~ poly(Year, degree), data = data)
}

# Function to fit an exponential model
fit_exponential_model <- function(data) {
  nls(Articles ~ a * exp(b * Year), data = data, start = list(a = 1, b = 0.1))
}


# Function to get the best model based on a comparison table
get_best_model <- function(metrics_comparison_table) {
  # Check if the table is empty
  if (nrow(metrics_comparison_table) == 0) {
    stop("Metrics comparison table is empty.")
  }
  
  # Select the best model based on the AIC (or other criterion)
  best_model_row <- metrics_comparison_table %>% arrange(AIC) %>% slice(1)  

  # Return the best model details
  return(list(
    name = best_model_row$Model_Name,
    params = best_model_row$Parameters,
    R2 = best_model_row$R_Squared,
    AIC = best_model_row$AIC,
    RMSE = best_model_row$RMSE
  ))
}