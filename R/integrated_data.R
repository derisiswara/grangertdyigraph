#' Generate example integrated time series data
#'
#' This function generates a dataset with integrated time series variables
#' that may have unit roots, suitable for Toda-Yamamoto causality testing.
#'
#' @param n Number of observations
#' @param integration_orders Vector of integration orders for each series
#' @param causal_strength Strength of the causal relationships
#' @param seed Random seed for reproducibility
#'
#' @return A data frame with multiple time series with known causal relationships
#'   and different orders of integration
#'
#' @examples
#' example_integrated_data <- generate_integrated_data(500)
#' head(example_integrated_data)
#'
#' @export
generate_integrated_data <- function(n = 500, 
                                    integration_orders = c(1, 1, 0), 
                                    causal_strength = 0.8,
                                    seed = 123) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # Generate the base series
  x_base <- numeric(n)
  y_base <- numeric(n)
  z_base <- numeric(n)
  
  # Generate AR(1) processes
  x_base[1] <- rnorm(1)
  y_base[1] <- rnorm(1)
  z_base[1] <- rnorm(1)
  
  for (i in 2:n) {
    x_base[i] <- 0.7 * x_base[i-1] + rnorm(1, 0, 1)
    
    # y is caused by x with a lag
    if (i > 50) {
      y_base[i] <- 0.4 * y_base[i-1] + causal_strength * x_base[i-2] + rnorm(1, 0, 1)
    } else {
      y_base[i] <- 0.4 * y_base[i-1] + rnorm(1, 0, 1)
    }
    
    # z is a stationary AR(1) process not affected by others
    z_base[i] <- 0.5 * z_base[i-1] + rnorm(1, 0, 1)
  }
  
  # Apply integration to create series with different orders
  x <- x_base
  y <- y_base
  z <- z_base
  
  # Integrate x series if required
  if (integration_orders[1] > 0) {
    for (d in 1:integration_orders[1]) {
      x <- cumsum(x)
    }
  }
  
  # Integrate y series if required
  if (integration_orders[2] > 0) {
    for (d in 1:integration_orders[2]) {
      y <- cumsum(y)
    }
  }
  
  # Integrate z series if required
  if (integration_orders[3] > 0) {
    for (d in 1:integration_orders[3]) {
      z <- cumsum(z)
    }
  }
  
  # Create data frame
  data <- data.frame(x = x, y = y, z = z)
  
  # Add attributes
  attr(data, "integration_orders") <- integration_orders
  attr(data, "causal_relations") <- "x → y (from t=50)"
  
  return(data)
}

#' Example Integrated Time Series Data
#'
#' A dataset with integrated time series variables for Toda-Yamamoto causality testing.
#'
#' @format A data frame with 500 rows and 3 variables:
#' \describe{
#'   \item{x}{First variable, integrated of order 1 (I(1))}
#'   \item{y}{Second variable, integrated of order 1 (I(1)), caused by x}
#'   \item{z}{Third variable, stationary (I(0)), not causally related}
#' }
#' @source Generated using generate_integrated_data()
"example_integrated_data"

# Generate and save the data
example_integrated_data <- generate_integrated_data(500)
