#' Create a Network Graph from Granger Test Results
#'
#' This function creates a network graph visualization from Granger causality test results.
#' Each node represents a variable, and directed edges indicate Granger causality.
#'
#' @param granger_result A result object from the granger_test function
#' @param layout The layout algorithm to use for the graph (default: "fr" for Fruchterman-Reingold)
#' @param edge_threshold Optional p-value threshold for including edges (overrides the alpha in granger_result)
#' @param node_size Size of nodes in the plot
#' @param edge_arrow_size Size of the arrow heads
#' @param edge_width Width of edges
#'
#' @return An igraph object representing the causality network
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' x <- rnorm(100)
#' y <- c(0, 0, x[1:98]) + rnorm(100, 0, 0.5)
#' z <- c(0, 0, y[1:98]) + rnorm(100, 0, 0.5)
#' data <- data.frame(x = x, y = y, z = z)
#'
#' # Run Granger causality test
#' result <- granger_test(data, max_lag = 2)
#'
#' # Create and plot network
#' g <- plot_granger_network(result)
#' plot(g)
#' }
#'
#' @import igraph
#' @export
plot_granger_network <- function(granger_result, layout = "fr", edge_threshold = NULL,
                                 node_size = 15, edge_arrow_size = 0.5, edge_width = 1) {
  # Check input
  if (!inherits(granger_result, "granger_test_result")) {
    stop("Input must be a granger_test_result object")
  }
  
  # Extract data from the result
  p_values <- granger_result$p_values
  var_names <- granger_result$variable_names
  n_vars <- length(var_names)
  
  # Use provided threshold or default from the test
  if (is.null(edge_threshold)) {
    edge_threshold <- granger_result$alpha
  }
  
  # Create an empty graph
  g <- igraph::make_empty_graph(n = n_vars, directed = TRUE)
  
  # Set vertex names
  igraph::V(g)$name <- var_names
  
  # Add edges based on significant Granger causality
  edges <- c()
  edge_weights <- c()
  
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i != j && !is.na(p_values[i, j]) && p_values[i, j] < edge_threshold) {
        # Add edge: i Granger-causes j
        edges <- c(edges, i, j)
        edge_weights <- c(edge_weights, 1 - p_values[i, j])  # Higher weight for more significant causality
      }
    }
  }
  
  # Add edges to the graph
  if (length(edges) > 0) {
    g <- igraph::add_edges(g, edges, weight = edge_weights)
  }
  
  # Set visualization attributes
  igraph::V(g)$size <- node_size
  igraph::V(g)$label <- var_names
  igraph::V(g)$color <- "lightblue"
  igraph::V(g)$frame.color <- "darkblue"
  
  igraph::E(g)$width <- edge_width * igraph::E(g)$weight
  igraph::E(g)$arrow.size <- edge_arrow_size
  
  # Set layout
  layout_func <- get(paste0("layout_with_", layout), envir = asNamespace("igraph"))
  g$layout <- layout_func(g)
  
  return(g)
}
