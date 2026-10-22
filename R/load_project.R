# Load the project ----
# main.Rmd, the examples and the tests start with:
#   source("R/load_project.R")
#   load_project()

#' Load the data and functions of the project
#'
#' Runs every script in R/ in one project environment, then copies only the
#' high-level functions and the prepared data into the global environment.
#' RStudio's Environment pane then shows what a user needs, not the dozens
#' of helper functions behind them. The functions still find their helpers,
#' because those stay in the project environment.
#'
#' @param internals TRUE copies everything, helpers and raw data included:
#'   for the tests, which check the helpers directly.
#' @param envir Where to copy the objects to.
#'
#' @return The project environment, invisibly.
load_project <- function(internals = FALSE, envir = globalenv()) {
  project <- new.env(parent = globalenv())
  scripts <- list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  for (script in setdiff(scripts, "R/load_project.R")) {
    sys.source(script, envir = project, keep.source = FALSE)
  }

  # The functions for main.Rmd, the examples and the README, and the
  # prepared data they use.
  exports <- c(
    # Analysis
    "analyse_happiness", "plot_happiness", "format_coefficients",
    "describe_result", "check_assumptions",
    # App
    "happiness_app", "group_predictors",
    # Data (R/prepare_data.R)
    "combined_data", "available_predictors", "predictor_types",
    "predictor_notes", "variable_info"
  )
  shown <- if (internals) ls(project) else exports
  list2env(mget(shown, envir = project), envir = envir)
  invisible(project)
}
