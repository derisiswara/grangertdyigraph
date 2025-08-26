#' Create a Dynamic Causality Graph from Time Series Data
#'
#' This function creates a dynamic graph representation of evolving Granger causality
#' relationships over time using a moving window approach.
#'
#' @param data A multivariate time series object or data frame with time series data
#' @param window_size Size of the rolling window for causality analysis
#' @param step Step size for moving the window (default: window_size/5)
#' @param max_lag Maximum lag to consider in the Granger test (default: 2)
#' @param alpha Significance level for the test (default: 0.05)
#'
#' @return A list containing dynamic graph information with time points and corresponding graphs
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' n <- 500
#' x <- arima.sim(model = list(ar = c(0.7)), n = n)
#' y <- c(rep(0, 100), 0.8 * x[1:(n-100)]) + arima.sim(model = list(ar = c(0.3)), n = n, sd = 0.5)
#' z <- c(rep(0, 300), 0.6 * y[1:(n-300)]) + arima.sim(model = list(ar = c(0.4)), n = n, sd = 0.5)
#' data <- data.frame(x = x, y = y, z = z)
#'
#' # Create dynamic graph
#' dyn_graph <- create_dynamic_graph(data, window_size = 150, step = 30)
#'
#' # Plot dynamic causality
#' plot_dynamic_causality(dyn_graph)
#' }
#'
#' @import stats igraph
#' @export
create_dynamic_graph <- function(data, window_size, step = NULL, max_lag = 2, alpha = 0.05) {
  # Check inputs
  if (!is.data.frame(data) && !stats::is.ts(data)) {
    stop("Data must be a data frame or time series object")
  }
  
  if (window_size < max_lag * 5) {
    stop("Window size should be at least 5 times the maximum lag")
  }
  
  # Set default step size if not provided
  if (is.null(step)) {
    step <- max(1, floor(window_size / 5))
  }
  
  # Convert to data frame if it's a time series
  if (stats::is.ts(data)) {
    data <- as.data.frame(data)
  }
  
  n_obs <- nrow(data)
  n_windows <- floor((n_obs - window_size) / step) + 1
  
  # Initialize results list
  result <- list(
    time_points = numeric(n_windows),
    graphs = list(),
    window_size = window_size,
    step = step,
    variable_names = colnames(data)
  )
  
  # Sliding window analysis
  for (i in 1:n_windows) {
    start_idx <- (i - 1) * step + 1
    end_idx <- start_idx + window_size - 1
    
    if (end_idx > n_obs) {
      end_idx <- n_obs
    }
    
    # Extract window data
    window_data <- data[start_idx:end_idx, ]
    
    # Perform Granger causality test
    granger_result <- granger_test(window_data, max_lag = max_lag, alpha = alpha)
    
    # Create graph for this window
    g <- plot_granger_network(granger_result, layout = "fr")
    
    # Store results
    result$time_points[i] <- (start_idx + end_idx) / 2  # Middle of the window
    result$graphs[[i]] <- g
  }
  
  class(result) <- "dynamic_granger_graph"
  return(result)
}
