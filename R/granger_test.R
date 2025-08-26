#' Perform Granger Causality Test
#'
#' This function performs a Granger causality test on multivariate time series data.
#' The test determines whether one time series is useful in forecasting another.
#'
#' @param data A multivariate time series object or data frame with time series data
#' @param max_lag Maximum lag to consider in the test
#' @param alpha Significance level for the test (default: 0.05)
#' @param type Type of test to perform: "wald" (default), "f", or "chisq"
#'
#' @return A list containing the test results, p-values, and significance matrix
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' x <- rnorm(100)
#' y <- c(0, 0, x[1:98]) + rnorm(100, 0, 0.5)
#' data <- data.frame(x = x, y = y)
#'
#' # Run Granger causality test
#' result <- granger_test(data, max_lag = 3)
#' print(result)
#' }
#'
#' @import stats lmtest vars
#' @export
granger_test <- function(data, max_lag = 1, alpha = 0.05, type = "wald") {
  # Check inputs
  if (!is.data.frame(data) && !stats::is.ts(data)) {
    stop("Data must be a data frame or time series object")
  }
  
  if (max_lag < 1) {
    stop("Maximum lag must be at least 1")
  }
  
  # Convert to data frame if it's a time series
  if (stats::is.ts(data)) {
    data <- as.data.frame(data)
  }
  
  # Get variable names
  var_names <- colnames(data)
  n_vars <- length(var_names)
  
  # Initialize result matrices
  p_values <- matrix(NA, n_vars, n_vars)
  significance <- matrix(FALSE, n_vars, n_vars)
  colnames(p_values) <- var_names
  rownames(p_values) <- var_names
  colnames(significance) <- var_names
  rownames(significance) <- var_names
  
  # Perform Granger causality test for each pair of variables
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      # Skip self-causality test
      if (i == j) {
        p_values[i, j] <- 1
        next
      }
      
      # Extract the two variables
      x <- data[, i]
      y <- data[, j]
      
      # Create a bivariate VAR model
      var_data <- cbind(x, y)
      colnames(var_data) <- c("x", "y")
      
      # Fit VAR model
      tryCatch({
        model <- vars::VAR(var_data, p = max_lag, type = "const")
        
        # Test if x Granger-causes y
        test_result <- lmtest::grangertest(x, y, order = max_lag)
        
        # Extract p-value
        p_value <- test_result$`Pr(>F)`[2]
        p_values[i, j] <- p_value
        
        # Check significance
        significance[i, j] <- p_value < alpha
      }, error = function(e) {
        warning(paste("Error in Granger test for variables", 
                      var_names[i], "and", var_names[j], ":", e$message))
        p_values[i, j] <- NA
        significance[i, j] <- FALSE
      })
    }
  }
  
  # Create a summary of results
  results <- list(
    p_values = p_values,
    significance = significance,
    alpha = alpha,
    max_lag = max_lag,
    variable_names = var_names
  )
  
  class(results) <- "granger_test_result"
  return(results)
}
