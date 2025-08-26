library(testthat)
library(grangertdyigraph)

test_that("granger_test works correctly", {
  # Generate test data
  x <- rnorm(100)
  y <- c(0, 0, x[1:98]) + rnorm(100, 0, 0.5)
  z <- rnorm(100)  # Independent series
  data <- data.frame(x = x, y = y, z = z)
  
  # Run test
  result <- granger_test(data, max_lag = 2)
  
  # Check structure
  expect_s3_class(result, "granger_test_result")
  expect_named(result, c("p_values", "significance", "alpha", "max_lag", "variable_names"))
  
  # Check dimensions
  expect_equal(dim(result$p_values), c(3, 3))
  expect_equal(dim(result$significance), c(3, 3))
  
  # Check that x should Granger-cause y
  expect_true(is.na(result$p_values[1, 1]))  # Self-causation undefined
  expect_lt(result$p_values[1, 2], 0.1)  # x should cause y with high probability
  
  # z should not be caused by or cause others
  expect_gt(result$p_values[1, 3], 0.05)
  expect_gt(result$p_values[2, 3], 0.05)
  expect_gt(result$p_values[3, 1], 0.05)
  expect_gt(result$p_values[3, 2], 0.05)
})

test_that("plot_granger_network creates valid graph", {
  # Generate test data
  x <- rnorm(100)
  y <- c(0, 0, x[1:98]) + rnorm(100, 0, 0.5)
  data <- data.frame(x = x, y = y)
  
  # Run test
  result <- granger_test(data, max_lag = 2)
  g <- plot_granger_network(result)
  
  # Check structure
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 2)  # Should have 2 nodes
  
  # Check vertex attributes
  expect_equal(igraph::V(g)$name, c("x", "y"))
  
  # There should be an edge from x to y
  expect_true(igraph::are_adjacent(g, "x", "y"))
})
