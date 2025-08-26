#' Coffee Price Time Series Data
#'
#' Monthly data on coffee prices for Arabica and Robusta varieties from 1960 to 2011.
#' This dataset is ideal for time series analysis and causality testing.
#'
#' @format A data frame with 615 rows and 3 variables:
#' \describe{
#'   \item{Date}{Date in YYYY-MM-DD format for the first day of each month}
#'   \item{Arabica}{Monthly price for Arabica coffee}
#'   \item{Robusta}{Monthly price for Robusta coffee}
#' }
#'
#' @source Historical coffee price data
#' @examples
#' data(coffee_data)
#' head(coffee_data)
#'
#' # Plot the time series
#' plot(coffee_data$Date, coffee_data$Arabica, type="l", col="darkgreen", 
#'      xlab="Year", ylab="Price", main="Coffee Price Trends")
#' lines(coffee_data$Date, coffee_data$Robusta, col="brown")
#' legend("topleft", legend=c("Arabica", "Robusta"), col=c("darkgreen", "brown"), lwd=1)
#'
"coffee_data"

# Process coffee data from the CSV file
process_coffee_data <- function() {
  # Load the CSV data file
  file_path <- system.file("extdata", "coffee_data.csv", package = "grangertdyigraph")
  if (file_path == "") {
    # If the file is not found in the installed package, try the development version
    file_path <- "coffee_data.csv"
  }
  
  coffee_raw <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Clean the data: Remove rows with NA values or NA-01 in the Date
  coffee_raw <- coffee_raw[!grepl("NA-01", coffee_raw$Date), ]
  
  # Convert the Date column to Date type
  coffee_raw$Date <- as.Date(coffee_raw$Date, format = "%Y-%m-%d")
  
  return(coffee_raw)
}

# Create the coffee_data dataset
coffee_data <- process_coffee_data()

# This function is run when the package is loaded
.onLoad <- function(libname, pkgname) {
  # The data is already included in the package
}
