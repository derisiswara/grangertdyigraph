#' Generate Example Time Series Data
#'
#' This function generates a dataset with known causal relationships
#' for demonstration purposes.
#'
#' @param n Number of observations
#' @param causal_strength Strength of the causal relationships
#' @param noise_sd Standard deviation of the noise term
#' @param seed Random seed for reproducibility
#'
#' @return A data frame with multiple time series with known causal relationships
#'
#' @examples
#' example_data <- generate_example_data(500)
#' head(example_data)
#'
#' @export
generate_example_data <- function(n = 500, causal_strength = 0.8, 
                                 noise_sd = 0.5, seed = 123) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # Generate the first variable (autoregressive)
  x <- numeric(n)
  x[1] <- rnorm(1)
  for (i in 2:n) {
    x[i] <- 0.7 * x[i-1] + rnorm(1, 0, noise_sd)
  }
  
  # Generate y that is caused by x after a delay of 50 time points
  y <- numeric(n)
  y[1] <- rnorm(1)
  for (i in 2:50) {
    y[i] <- 0.3 * y[i-1] + rnorm(1, 0, noise_sd)
  }
  for (i in 51:n) {
    # y depends on x with a lag of 1
    y[i] <- 0.3 * y[i-1] + causal_strength * x[i-1] + rnorm(1, 0, noise_sd)
  }
  
  # Generate z that is caused by y after a delay of 150 time points
  z <- numeric(n)
  z[1] <- rnorm(1)
  for (i in 2:150) {
    z[i] <- 0.4 * z[i-1] + rnorm(1, 0, noise_sd)
  }
  for (i in 151:n) {
    # z depends on y with a lag of 2
    z[i] <- 0.4 * z[i-1] + causal_strength * y[i-2] + rnorm(1, 0, noise_sd)
  }
  
  # Generate w that affects y at the end of the time series
  w <- numeric(n)
  w[1] <- rnorm(1)
  for (i in 2:n) {
    w[i] <- 0.5 * w[i-1] + rnorm(1, 0, noise_sd)
  }
  
  # Make y also depend on w in the last 100 time points
  for (i in (n-100+1):n) {
    y[i] <- y[i] + 0.7 * w[i-1]
  }
  
  # Create data frame
  data <- data.frame(x = x, y = y, z = z, w = w)
  
  # Add timestamp attribute
  attr(data, "timestamp") <- Sys.time()
  attr(data, "description") <- "Example data with known causal relationships"
  attr(data, "causal_relations") <- "x → y (from t=50), y → z (from t=150), w → y (from t=400)"
  
  return(data)
}

#' Example Time Series Data
#'
#' A dataset with known causal relationships for demonstration purposes.
#'
#' @format A data frame with 500 rows and 4 variables:
#' \describe{
#'   \item{x}{First variable (autoregressive)}
#'   \item{y}{Second variable, caused by x after a delay of 50 time points}
#'   \item{z}{Third variable, caused by y after a delay of 150 time points}
#'   \item{w}{Fourth variable, affects y in the last 100 time points}
#' }
#' @source Generated using generate_example_data()
"example_timeseries"

# Generate and save the data
example_timeseries <- generate_example_data(500)

# Function to save example data when package is built
.onLoad <- function(libname, pkgname) {
  # This function runs when the package is loaded
  # The data is already included in the package
}
