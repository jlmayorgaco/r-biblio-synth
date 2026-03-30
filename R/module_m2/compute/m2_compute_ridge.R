# ============================================================================
# m2_compute_ridge.R - Ridge Regression with Cross-Validation
# ============================================================================
# L2 regularization (Ridge) for growth model fitting with automatic
# hyperparameter selection via cross-validation.
#
# IEEE Q1 Enhancement: Provides regularized regression to prevent overfitting
# in growth models, with automatic lambda selection using k-fold CV.

#' Compute Ridge regression with cross-validation for time series
#'
#' @param data Data frame with Year and Articles columns
#' @param config Configuration list
#' @return List with Ridge regression results
#' @export
compute_m2_ridge <- function(data, config = biblio_config()) {
  validate_is_data_frame(data)
  
  required_cols <- c("Year", "Articles")
  validation <- validate_required_columns(data, required_cols)
  if (!validation$ok) {
    return(list(
      ridge = list(),
      cv_results = data.frame(),
      status = paste("error: missing columns:", paste(validation$missing_columns, collapse = ", "))
    ))
  }
  
  if (nrow(data) < 5) {
    return(list(
      ridge = list(),
      cv_results = data.frame(),
      status = "error: insufficient data points (need at least 5)"
    ))
  }
  
  year <- data$Year
  articles <- data$Articles
  
  year_centered <- year - min(year)
  
  cv_result <- perform_ridge_cv(year_centered, articles)
  
  ridge_result <- fit_ridge_model(year_centered, articles, cv_result$best_lambda)
  
  pred_year_centered <- year_centered
  predictions <- predict_ridge(ridge_result, pred_year_centered)
  
  residuals_val <- articles - predictions
  rmse <- sqrt(mean(residuals_val^2, na.rm = TRUE))
  mae <- mean(abs(residuals_val), na.rm = TRUE)
  ss_res <- sum(residuals_val^2, na.rm = TRUE)
  ss_tot <- sum((articles - mean(articles, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  
  ridge_output <- list(
    coefficients = ridge_result$coefficients,
    lambda = cv_result$best_lambda,
    lambda_sequence = cv_result$lambda_sequence,
    cv_mean_mse = cv_result$cv_mean_mse,
    cv_se = cv_result$cv_se,
    best_r2 = r2,
    rmse = rmse,
    mae = mae,
    predictions = data.frame(
      year = year,
      actual = articles,
      predicted = as.numeric(predictions)
    ),
    n_folds = cv_result$n_folds,
    method = "Ridge (L2 regularization)"
  )
  
  list(
    ridge = ridge_output,
    cv_results = cv_result$cv_summary,
    status = "success"
  )
}

#' Perform k-fold cross-validation for Ridge lambda selection
#' @keywords internal
perform_ridge_cv <- function(x, y, n_folds = 5, lambda_sequence = NULL) {
  n <- length(y)
  if (n < n_folds) n_folds <- n
  
  if (is.null(lambda_sequence)) {
    lambda_max <- max(abs(crossprod(x, y))) / n
    lambda_sequence <- exp(seq(log(lambda_max), log(lambda_max * 1e-4), length.out = 100))
  }
  
  fold_ids <- sample(rep(1:n_folds, length.out = n))
  
  cv_mse <- matrix(NA, nrow = n_folds, ncol = length(lambda_sequence))
  
  for (fold in 1:n_folds) {
    train_idx <- fold_ids != fold
    test_idx <- fold_ids == fold
    
    x_train <- x[train_idx]
    y_train <- y[train_idx]
    x_test <- x[test_idx]
    y_test <- y[test_idx]
    
    for (l_idx in seq_along(lambda_sequence)) {
      lambda <- lambda_sequence[l_idx]
      
      coef <- solve_ridge(x_train, y_train, lambda)
      pred <- cbind(1, x_test) %*% coef
      
      cv_mse[fold, l_idx] <- mean((y_test - pred)^2, na.rm = TRUE)
    }
  }
  
  mean_mse <- apply(cv_mse, 2, mean, na.rm = TRUE)
  se_mse <- apply(cv_mse, 2, sd, na.rm = TRUE) / sqrt(n_folds)
  
  best_idx <- which.min(mean_mse)
  best_lambda <- lambda_sequence[best_idx]
  
  se_rule_idx <- which(mean_mse <= mean_mse[best_idx] + se_mse[best_idx])[1]
  if (is.na(se_rule_idx)) se_rule_idx <- best_idx
  
  cv_summary <- data.frame(
    lambda = lambda_sequence,
    mean_mse = mean_mse,
    se_mse = se_mse
  )
  
  list(
    best_lambda = best_lambda,
    se_rule_lambda = lambda_sequence[se_rule_idx],
    lambda_sequence = lambda_sequence,
    cv_mean_mse = mean_mse,
    cv_se = se_mse,
    cv_summary = cv_summary,
    n_folds = n_folds
  )
}

#' Solve Ridge regression (closed-form solution)
#' @keywords internal
solve_ridge <- function(x, y, lambda) {
  X <- cbind(1, x)
  
  XtX <- crossprod(X)
  XtX_ridge <- XtX + lambda * diag(c(0, 1))
  
  Xty <- crossprod(X, y)
  
  solve(XtX_ridge, Xty)
}

#' Fit Ridge model
#' @keywords internal
fit_ridge_model <- function(x, y, lambda) {
  coefficients <- solve_ridge(x, y, lambda)
  
  list(
    coefficients = coefficients,
    lambda = lambda,
    intercept = coefficients[1],
    slope = coefficients[2]
  )
}

#' Predict using Ridge model
#' @keywords internal
predict_ridge <- function(model, x_new) {
  X_new <- cbind(1, x_new)
  as.numeric(X_new %*% model$coefficients)
}

#' Compute Ridge with polynomial features
#' @export
compute_m2_ridge_polynomial <- function(data, degree = 2, config = biblio_config()) {
  validate_is_data_frame(data)
  
  required_cols <- c("Year", "Articles")
  validation <- validate_required_columns(data, required_cols)
  if (!validation$ok) {
    return(list(
      ridge = list(),
      status = paste("error: missing columns:", paste(validation$missing_columns, collapse = ", "))
    ))
  }
  
  year <- data$Year
  articles <- data$Articles
  
  year_centered <- year - min(year)
  
  X_poly <- poly(year_centered, degree = degree, raw = TRUE)
  X <- cbind(1, X_poly)
  
  y <- articles
  
  cv_result <- perform_ridge_cv_poly(X, y)
  
  coef <- solve_ridge_poly(X, y, cv_result$best_lambda)
  
  predictions <- X %*% coef
  
  residuals_val <- y - as.numeric(predictions)
  rmse <- sqrt(mean(residuals_val^2, na.rm = TRUE))
  ss_res <- sum(residuals_val^2, na.rm = TRUE)
  ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  
  list(
    ridge = list(
      coefficients = coef,
      lambda = cv_result$best_lambda,
      degree = degree,
      best_r2 = r2,
      rmse = rmse,
      predictions = data.frame(
        year = year,
        actual = articles,
        predicted = as.numeric(predictions)
      ),
      method = paste0("Ridge Polynomial (degree = ", degree, ")")
    ),
    status = "success"
  )
}

#' Perform CV for polynomial Ridge
#' @keywords internal
perform_ridge_cv_poly <- function(X, y, n_folds = 5, lambda_sequence = NULL) {
  n <- length(y)
  p <- ncol(X) - 1
  
  if (is.null(lambda_sequence)) {
    lambda_max <- max(abs(crossprod(X, y))) / n
    lambda_sequence <- exp(seq(log(lambda_max), log(lambda_max * 1e-4), length.out = 100))
  }
  
  if (n < n_folds) n_folds <- n
  fold_ids <- sample(rep(1:n_folds, length.out = n))
  
  cv_mse <- matrix(NA, nrow = n_folds, ncol = length(lambda_sequence))
  
  for (fold in 1:n_folds) {
    train_idx <- fold_ids != fold
    test_idx <- fold_ids == fold
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_test <- X[test_idx, , drop = FALSE]
    y_test <- y[test_idx]
    
    for (l_idx in seq_along(lambda_sequence)) {
      lambda <- lambda_sequence[l_idx]
      
      coef <- solve_ridge_poly(X_train, y_train, lambda)
      pred <- X_test %*% coef
      
      cv_mse[fold, l_idx] <- mean((y_test - as.numeric(pred))^2, na.rm = TRUE)
    }
  }
  
  mean_mse <- apply(cv_mse, 2, mean, na.rm = TRUE)
  best_idx <- which.min(mean_mse)
  
  list(
    best_lambda = lambda_sequence[best_idx],
    lambda_sequence = lambda_sequence,
    cv_mean_mse = mean_mse
  )
}

#' Solve polynomial Ridge
#' @keywords internal
solve_ridge_poly <- function(X, y, lambda) {
  XtX <- crossprod(X)
  penalty <- lambda * diag(c(0, rep(1, ncol(X) - 1)))
  XtX_ridge <- XtX + penalty
  Xty <- crossprod(X, y)
  solve(XtX_ridge, Xty)
}