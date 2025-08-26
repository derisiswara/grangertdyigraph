# Copilot instructions for grangertdyigraph package

This R package implements Granger causality tests and creates dynamic graph visualizations of time series relationships.

## Core Features
- Granger causality testing between time series variables
- Network visualization of causal relationships
- Dynamic/evolving causality analysis with sliding windows
- Interactive graph visualization

## File Structure
- `R/`: R source code files for package functions
- `man/`: Documentation files (generated from roxygen comments)
- `tests/`: Unit tests for package functions
- `vignettes/`: Package tutorials and examples
- `inst/`: Additional files included with the package
- `data/`: Example datasets for demonstration

## Development Tasks
- Build and check the package using `devtools::check()`
- Update documentation with `devtools::document()`
- Run tests with `devtools::test()`
- Build vignettes with `devtools::build_vignettes()`

## Implementation Notes
- The package depends on igraph for network visualization
- Time series analysis uses lmtest and vars packages
- Interactive visualizations could be improved using htmlwidgets or plotly
