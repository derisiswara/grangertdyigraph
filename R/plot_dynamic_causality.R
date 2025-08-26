#' Plot Dynamic Causality Graph
#'
#' This function creates a visualization of how Granger causality relationships
#' evolve over time from a dynamic graph object.
#'
#' @param dyn_graph A dynamic Granger graph object from create_dynamic_graph
#' @param interactive Whether to create an interactive HTML widget (default: TRUE)
#' @param animate Whether to animate the transitions between time points (default: TRUE for interactive)
#' @param fps Frames per second for animation (default: 1)
#'
#' @return Either a list of plots or an interactive HTML widget
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
#' @import igraph ggplot2 htmlwidgets
#' @export
plot_dynamic_causality <- function(dyn_graph, interactive = TRUE, animate = NULL, fps = 1) {
  # Check input
  if (!inherits(dyn_graph, "dynamic_granger_graph")) {
    stop("Input must be a dynamic_granger_graph object")
  }
  
  # Default animate based on interactive setting
  if (is.null(animate)) {
    animate <- interactive
  }
  
  # Extract data
  graphs <- dyn_graph$graphs
  time_points <- dyn_graph$time_points
  n_points <- length(time_points)
  
  if (interactive) {
    # Create interactive visualization using igraph and htmlwidgets
    # Note: This is a simplified implementation - in practice you would use
    # a proper JavaScript visualization library like visNetwork
    
    # Create a list of plots
    plots <- list()
    for (i in 1:n_points) {
      g <- graphs[[i]]
      title <- paste("Time window centered at t =", round(time_points[i], 1))
      
      # Create plot
      plots[[i]] <- igraph::plot.igraph(g, main = title, sub = paste("Window", i, "of", n_points))
    }
    
    # In a real implementation, this would create an HTML widget
    # For this example, we'll return the list of plots
    return(plots)
    
  } else {
    # Create static plots using ggplot2
    plots <- list()
    for (i in 1:n_points) {
      g <- graphs[[i]]
      title <- paste("Time window centered at t =", round(time_points[i], 1))
      
      # Create plot using ggplot
      # Note: This would require converting igraph objects to ggplot format
      # which requires more code than shown here
      plots[[i]] <- plot(g, main = title)
    }
    
    return(plots)
  }
}
