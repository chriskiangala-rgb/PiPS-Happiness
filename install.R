# Install the packages this project needs ----
# Run this file once: source("install.R")
# When you use a new package in the code, add its name to this list.

required_packages <- c(
  "bslib",
  "countrycode",
  "ggplot2",
  "lmtest",
  "plotly",
  "reactable",
  "rmarkdown",
  "sandwich",
  "shiny",
  "shinyWidgets",
  "thematic"
)

# Install only the packages that are not installed yet.
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}
