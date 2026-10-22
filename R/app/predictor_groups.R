# Group the predictors by theme for the app's menus ----

#' Sort the predictors into themed groups
#'
#' Predictors that are not listed in any theme go into "Other", so new or
#' renamed variables still show up in the menus.
#'
#' @param predictors The variable names the user can choose from.
#' @param themes Named list with the variable names of each theme; by
#'   default the themes of the happiness and world data.
#'
#' @return A named list of groups, without empty groups, for the
#'   predictor_groups argument of happiness_app().
group_predictors <- function(predictors, themes = predictor_themes()) {
  groups <- lapply(themes, function(theme) intersect(predictors, theme))
  groups$Other <- setdiff(predictors, unlist(groups))
  groups[lengths(groups) > 0]
}

# The variable names of each theme in the happiness and world data.
predictor_themes <- function() {
  list(
    "World Happiness Report" = c(
      "Freedom to make life choices", "Generosity", "Healthy life expectancy",
      "Logged GDP per capita", "Perceptions of corruption", "Social support"
    ),
    "Economy and work" = c(
      "log2(CPI)", "CPI Change (%)", "Gasoline Price", "log2(GDP per person)", "Minimum wage",
      "Population: Labor force participation (%)", "Tax revenue (%)",
      "Total tax rate", "Unemployment rate"
    ),
    "Health" = c(
      "log2(Infant mortality)", "Life expectancy", "log2(Maternal mortality ratio)",
      "Out of pocket health expenditure", "Physicians per thousand"
    ),
    "Population and education" = c(
      "Armed forces per 1000 people", "Birth Rate", "log2(Density (P/Km2))", "Fertility Rate",
      "Gross primary education enrollment (%)",
      "Gross tertiary education enrollment (%)", "log2(Population)", "Urban population (%)"
    ),
    "Land and environment" = c(
      "Agricultural Land( %)", "log2(CO2 per person)", "Forested Area (%)",
      "log2(Land Area(Km2))", "Distance from equator", "Longitude"
    ),
    "Categories" = c("continent", "Official language", "Currency-Code")
  )
}
