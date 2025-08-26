library(testthat)
library(grangertdyigraph)
library(vars)

test_that("toda_yamamoto works correctly", {
  # Skip if vars package is not available
  skip_if_not_installed("vars")
  skip_if_not_installed("forecast")
  skip_if_not_installed("aod")
  
  # Generate test data
  set.seed(123)
  x <- arima.sim(model = list(ar = c(0.7)), n = 100)
  y <- c(0, 0, x[1:98]) + rnorm(100, 0, 0.5)  # y is caused by x
  z <- rnorm(100)  # Independent series
  data <- data.frame(x = x, y = y, z = z)
  
  # Create VAR model
  var_model <- VAR(data, p = 2, type = "const")
  
  # Run Toda-Yamamoto test
  result <- toda_yamamoto(var_model)
  
  # Check structure
  expect_s3_class(result, "ty_test_result")
  expect_named(result, c("cause", "effect", "chisq", "pvalue"))
  
  # Check dimensions - should be 6 rows (3 variables, each can cause 2 others)
  expect_equal(nrow(result), 6)
  
  # Check that x should cause y with high probability
  x_causes_y <- result[result$cause == "x" & result$effect == "y", ]
  expect_true(nrow(x_causes_y) == 1)
  expect_lt(x_causes_y$pvalue, 0.1)  # p-value should be low
})

test_that("plot_ty_network creates valid graph", {
  # Skip if igraph package is not available
  skip_if_not_installed("igraph")
  skip_if_not_installed("vars")
  
  # Generate test data
  set.seed(123)
  x <- arima.sim(model = list(ar = c(0.7)), n = 100)
  y <- c(0, 0, x[1:98]) + rnorm(100, 0, 0.5)
  data <- data.frame(x = x, y = y)
  
  # Create VAR model and run test
  var_model <- VAR(data, p = 2, type = "const")
  result <- toda_yamamoto(var_model)
  
  # Create graph
  g <- plot_ty_network(result, plot = FALSE)
  
  # Check structure
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 2)  # Should have 2 nodes
  
  # Check vertex attributes
  expect_equal(sort(igraph::V(g)$name), sort(c("x", "y")))
})
