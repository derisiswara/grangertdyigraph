#' Plot Toda-Yamamoto Causality Network
#'
#' This function creates a network visualization of the causality relationships
#' identified by the Toda-Yamamoto causality test.
#'
#' @param ty_results A ty_test_result object or data frame with Toda-Yamamoto test results
#' @param alpha Significance level for determining causality relationships (default: 0.05)
#' @param layout Layout algorithm to use for the graph (default: "circle")
#' @param vertex_size Size of vertices in the plot (default: 40)
#' @param vertex_color Color of vertices (default: "lightblue")
#' @param edge_width Width of edges for significant and non-significant relationships (default: c(3, 2))
#' @param edge_color Colors for significant and non-significant relationships (default: c("red", "gray50"))
#' @param add_legend Whether to add a legend to the plot (default: TRUE)
#'
#' @return An igraph object representing the causality network
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(vars)
#'
#' # Create a VAR model
#' data <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' var_model <- VAR(data, p = 2, type = "const")
#'
#' # Run Toda-Yamamoto causality test
#' ty_results <- toda_yamamoto(var_model)
#'
#' # Create and plot the network
#' g <- plot_ty_network(ty_results)
#' }
#'
#' @import igraph
#' @export
plot_ty_network <- function(ty_results, alpha = 0.05, layout = "circle",
                          vertex_size = 40, vertex_color = "lightblue",
                          edge_width = c(3, 2), edge_color = c("red", "gray50"),
                          add_legend = TRUE) {
  # Check input
  if (!inherits(ty_results, "ty_test_result") && !is.data.frame(ty_results)) {
    stop("Input must be a ty_test_result object or data frame")
  }
  
  # Make sure we have required columns
  required_cols <- c("cause", "effect", "pvalue")
  if (!all(required_cols %in% colnames(ty_results))) {
    stop("Input must have columns: cause, effect, pvalue")
  }
  
  # Create a graph from the results
  g <- igraph::graph_from_data_frame(ty_results[, c("cause", "effect", "pvalue")], directed = TRUE)
  
  # Set edge attributes based on significance
  igraph::E(g)$significant <- igraph::E(g)$pvalue < alpha
  igraph::E(g)$color <- ifelse(igraph::E(g)$significant, edge_color[1], edge_color[2])
  igraph::E(g)$width <- ifelse(igraph::E(g)$significant, edge_width[1], edge_width[2])
   
  # Set vertex attributes
  igraph::V(g)$size <- vertex_size
  igraph::V(g)$color <- vertex_color
  
  # Set layout
  if (layout == "circle") {
    g$layout <- igraph::layout_in_circle(g)
  } else {
    layout_func <- get(paste0("layout_with_", layout), envir = asNamespace("igraph"))
    g$layout <- layout_func(g)
  }
  
  # Plot the graph
  plot(g, 
       vertex.label.cex = 1.2,
       edge.arrow.size = 1,
       edge.curved = 0.2,
       edge.label.cex = 0.8,
       main = "Toda-Yamamoto Causality Network")
  
  # Add legend if requested
  if (add_legend) {
    legend("bottom", 
           legend = c(paste("Significant (p <", alpha, ")"), "Not significant"),
           col = edge_color, 
           lwd = edge_width,
           cex = 0.8,
           horiz = TRUE)
  }
  
  # Return the graph invisibly
  invisible(g)
}
