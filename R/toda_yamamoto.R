#' Toda-Yamamoto Causality Test
#'
#' This function implements the Toda and Yamamoto (1995) version of Granger causality test.
#' The test is robust to the presence of unit roots and does not require pre-testing for
#' cointegration.
#'
#' @param var.model A VAR model object created by vars::VAR
#' @param test Character string, type of test to use for determining integration order.
#'        Options are "kpss", "adf", or "pp" (default: "kpss")
#'
#' @return A data frame with the causality test results including:
#'   \item{cause}{The potential causing variable}
#'   \item{effect}{The potential affected variable}
#'   \item{chisq}{The chi-squared test statistic}
#'   \item{pvalue}{The p-value of the test}
#'
#' @details
#' The Toda-Yamamoto procedure involves estimating a VAR model with p+d_max lags,
#' where p is the optimal lag order and d_max is the maximum order of integration
#' of the time series. The Wald test is then applied to the first p lags, while
#' the additional d_max lags serve as exogenous variables.
#'
#' @references
#' Toda, H. Y., & Yamamoto, T. (1995). Statistical inference in vector autoregressions
#' with possibly integrated processes. Journal of econometrics, 66(1-2), 225-250.
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(vars)
#'
#' # Create a VAR model
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' var_model <- VAR(data, p = 2, type = "const")
#'
#' # Run Toda-Yamamoto causality test
#' result <- toda_yamamoto(var_model)
#' print(result)
#' }
#'
#' @import forecast vars aod
#' @export
toda_yamamoto <- function(var.model, test = c("kpss", "adf", "pp")) {
  # Ensure required packages are available
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is needed for this function to work. Please install it.")
  }
  if (!requireNamespace("vars", quietly = TRUE)) {
    stop("Package 'vars' is needed for this function to work. Please install it.")
  }
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' is needed for this function to work. Please install it.")
  }
  
  # Default test method
  test <- match.arg(test)
  
  # Extract data from VAR model
  ty.df <- data.frame(var.model$y)
  ty.varnames <- colnames(ty.df)
  
  # Determine the maximum integration order
  d.max <- max(sapply(ty.df, function(x) forecast::ndiffs(x, test = test)))
  
  # Estimate the augmented VAR model (p + d_max lags)
  ty.lags <- var.model$p + d.max
  ty.augmented_var <- vars::VAR(ty.df, ty.lags, type = var.model$type)

  # Initialize results data frame
  ty.results <- data.frame(
    cause = character(0), 
    effect = character(0), 
    chisq = numeric(0), 
    pvalue = numeric(0)
  )
  
  # Perform Toda-Yamamoto test for each pair of variables
  for (k in seq_along(ty.varnames)) {
    for (j in seq_along(ty.varnames)) {
      if (k != j) {
        # Identify coefficients to test (first p lags, ignoring d_max extra lags)
        ty.coefres <- head(
          grep(
            ty.varnames[j], 
            setdiff(colnames(ty.augmented_var$datamat), colnames(ty.augmented_var$y))
          ),
          var.model$p  # Only test the first p lags, not the extra d_max lags
        )
        
        # Perform Wald test
        wald.res <- aod::wald.test(
          b = coef(ty.augmented_var$varresult[[k]]), 
          Sigma = vcov(ty.augmented_var$varresult[[k]]),
          Terms = ty.coefres
        ) 
        
        # Add results to the data frame
        ty.results <- rbind(
          ty.results, 
          data.frame(
            cause = ty.varnames[j], 
            effect = ty.varnames[k], 
            chisq = as.numeric(wald.res$result$chi2[1]),
            pvalue = as.numeric(wald.res$result$chi2[3])
          )
        )
      }
    }
  }
  
  # Add class for method dispatch
  class(ty.results) <- c("ty_test_result", "data.frame")
  
  return(ty.results)
}
