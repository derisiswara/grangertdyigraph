#' Prepare coffee data for analysis
#'
#' This function prepares the coffee data for analysis by handling missing values,
#' converting date formats, and optionally differencing the series.
#'
#' @param data A data frame containing coffee price data
#' @param diff_order Order of differencing to apply (default: 0 for no differencing)
#' @param log_transform Whether to apply log transformation (default: FALSE)
#'
#' @return A prepared data frame ready for analysis
#'
#' @examples
#' \dontrun{
#' data(coffee_data)
#' prepared_data <- prepare_coffee_data(coffee_data)
#' }
#'
#' @export
prepare_coffee_data <- function(data, diff_order = 0, log_transform = FALSE) {
  # Check input
  if (!inherits(data, "data.frame")) {
    stop("Input must be a data frame")
  }
  
  if (!all(c("Arabica", "Robusta") %in% colnames(data))) {
    stop("Data must contain 'Arabica' and 'Robusta' columns")
  }
  
  # Make a copy to avoid modifying the original
  result <- data
  
  # Extract only numeric columns (excluding Date)
  numeric_cols <- sapply(result, is.numeric)
  numeric_data <- result[, numeric_cols]
  
  # Apply log transformation if requested
  if (log_transform) {
    numeric_data <- log(numeric_data)
  }
  
  # Apply differencing if requested
  if (diff_order > 0) {
    for (i in seq_len(ncol(numeric_data))) {
      for (d in seq_len(diff_order)) {
        numeric_data[, i] <- c(NA, diff(numeric_data[, i]))
      }
    }
    # Remove rows with NAs from differencing
    na_rows <- apply(numeric_data, 1, function(x) any(is.na(x)))
    result <- result[!na_rows, ]
    numeric_data <- numeric_data[!na_rows, ]
  }
  
  # Replace numeric columns with transformed data
  result[, numeric_cols] <- numeric_data
  
  # Add attributes
  attr(result, "diff_order") <- diff_order
  attr(result, "log_transform") <- log_transform
  
  return(result)
}
