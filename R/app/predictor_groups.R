# Group the predictors by theme for the app's menus ----

#' Sort the predictors into themed groups
#'
#' Predictors that are not listed in any theme go into "Other", so new or
#' renamed variables still show up in the menus.
#'
#' @param predictors The variable names the user can choose from.
#' @param themes Named list with the variable names of each theme.
#'
#' @return A named list of groups, without empty groups, for the
#'   predictor_groups argument of happiness_app().
group_predictors <- function(predictors, themes = predictor_themes) {
  groups <- lapply(themes, function(theme) intersect(predictors, theme))
  groups$Other <- setdiff(predictors, unlist(groups))
  groups[lengths(groups) > 0]
}

predictor_themes <- list(
  "World Happiness Report" = c(
    "Freedom to make life choices", "Generosity", "Healthy life expectancy",
    "Logged GDP per capita", "Perceptions of corruption", "Social support"
  ),
  "Economy and work" = c(
    "CPI", "CPI Change (%)", "Gasoline Price", "GDP", "Minimum wage",
    "Population: Labor force participation (%)", "Tax revenue (%)",
    "Total tax rate", "Unemployment rate"
  ),
  "Health" = c(
    "Infant mortality", "Life expectancy", "Maternal mortality ratio",
    "Out of pocket health expenditure", "Physicians per thousand"
  ),
  "Population and education" = c(
    "Armed Forces size", "Birth Rate", "Density\n(P/Km2)", "Fertility Rate",
    "Gross primary education enrollment (%)",
    "Gross tertiary education enrollment (%)", "Population", "Urban_population"
  ),
  "Land and environment" = c(
    "Agricultural Land( %)", "Co2-Emissions", "Forested Area (%)",
    "Land Area(Km2)", "Latitude", "Longitude"
  ),
  "Categories" = c("continent", "Official language", "Currency-Code")
)
