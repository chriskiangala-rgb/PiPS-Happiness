source("https://raw.githubusercontent.com/chriskiangala-rgb/PiPS_mood/refs/heads/Data-and-stats/data/Data_analysis_change.R")

#Error checks for the happiness data analysis

#Check for unmatched countries

print(unmatched_countries)

known_unmatched <- c("Kosovo", "Taiwan Province of China", "Hong Kong S.A.R. of China", "Turkiye")

stopifnot("Unexpected unmatched countries" = all(unmatched_countries %in% known_unmatched))

#There are unmatched countries. Maybe the app should put out a warning
#that these countries cannot be used for the analysis

if (length(unmatched_countries) > 0) {
  warning(
    "These countries could not be matched and are excluded from analyses using world-data variables: ",
    paste(unmatched_countries, collapse = ", ")
  )
}

# Check that the main script has created the objects used below

stopifnot(
  "Run the data preparation code first" = all(
    c("world_data", "happiness_data", "combined_data", "name_changes",
      "numeric_columns", "available_predictors") %in% ls(envir = globalenv())
  )
)

# Check the imported datasets

stopifnot(
  "world_data is empty"                     = nrow(world_data) > 0,
  "world_data has too few columns"          = ncol(world_data) > 10,
  "happiness_data has too few rows"         = nrow(happiness_data) > 100,
  "Duplicate column names in world_data"    = !anyDuplicated(names(world_data)),
  "Expected world_data columns missing"     = all(c("Country", "GDP", "Population") %in% names(world_data)),
  "Expected happiness_data columns missing" = all(c("Country name", "Ladder score") %in% names(happiness_data))
)

# Check country names and duplicates

stopifnot(
  "Missing country names in happiness data" = !anyNA(happiness_data$join_country),
  "Empty country names in happiness data"   = all(happiness_data$join_country != ""),
  "Duplicate countries in happiness data"   = !anyDuplicated(happiness_data$join_country),
  "Duplicate countries in world data"       = !anyDuplicated(world_data$join_country)
)

# Check that every entry in name_changes matches a country (possible typo).
# The original column is used, because join_country has already been changed.
unused_changes <- setdiff(names(name_changes), happiness_data[["Country name"]])

if (length(unused_changes) > 0) {
  warning(
    "These name_changes entries were not found in the happiness data: ",
    paste(unused_changes, collapse = ", ")
  )
}

# Check the merged dataset

stopifnot(
  "combined_data is empty"           = nrow(combined_data) > 0,
  "Merge changed the number of rows" = nrow(combined_data) == nrow(happiness_data),
  "happiness column missing"         = "happiness" %in% names(combined_data)
)

# Check numeric conversion and plausible values
# (the value ranges are assumptions; adjust them if a check fails on correct data)

stopifnot(
  "happiness is not numeric"        = is.numeric(combined_data$happiness),
  "GDP is not numeric"              = is.numeric(combined_data$GDP),
  "Population is not numeric"       = is.numeric(combined_data$Population),
  "Negative population"             = all(combined_data$Population >= 0, na.rm = TRUE),
  "Unemployment rate outside 0-100" = all(combined_data$`Unemployment rate` >= 0 &
                                            combined_data$`Unemployment rate` <= 100, na.rm = TRUE),
  "Happiness outside 0-10"          = all(combined_data$happiness >= 0 &
                                            combined_data$happiness <= 10, na.rm = TRUE)
)

# Check the share of missing values in the converted columns.
# A column that is mostly NA often indicates a conversion problem.

na_share <- colMeans(is.na(combined_data[numeric_columns]))
print(round(na_share, 2))

stopifnot("A numeric column is completely NA" = all(na_share < 1))

if (any(na_share > 0.5)) {
  warning(
    "These columns have more than 50% missing values: ",
    paste(names(na_share)[na_share > 0.5], collapse = ", ")
  )
}

# Check continents

stopifnot(
  "Countries without continent" = !anyNA(combined_data$continent),
  "continent is not a factor"   = is.factor(combined_data$continent),
  "Unexpected continent labels" = all(levels(combined_data$continent) %in%
                                        c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"))
)

# Warn about very small continent groups, which give unstable estimates.

group_sizes <- table(combined_data$continent)

if (any(group_sizes < 5)) {
  warning(
    "These continents have fewer than 5 countries: ",
    paste(names(group_sizes)[group_sizes < 5], collapse = ", ")
  )
}

# Check the predictors

stopifnot(
  "Predictor missing in data" = all(available_predictors %in% names(combined_data)),
  "Duplicate predictor names" = !anyDuplicated(available_predictors)
)

no_variation <- available_predictors[vapply(
  combined_data[available_predictors],
  function(x) length(unique(na.omit(x))) < 2,
  logical(1)
)]

if (length(no_variation) > 0) {
  warning(
    "These predictors have no variation: ",
    paste(no_variation, collapse = ", ")
  )
}

# Define a check for the results of any regression.
# Usage: check_analysis_result(unadjusted)

check_analysis_result <- function(result,
                                  min_n = 30,
                                  max_excluded_share = 0.2) {
  coefficients <- result$all_coefficients
  
  stopifnot(
    "Too few observations"    = result$n_countries >= min_n,
    "R-squared outside 0-1"   = result$r_squared >= 0 && result$r_squared <= 1,
    "Non-finite coefficients" = all(is.finite(coefficients$estimate)),
    "p-values outside 0-1"    = all(coefficients$p_value >= 0 &
                                      coefficients$p_value <= 1, na.rm = TRUE)
  )
  
  total_countries <- result$n_countries + result$n_excluded
  
  if (result$n_excluded > max_excluded_share * total_countries) {
    warning(
      "More than ", max_excluded_share * 100,
      "% of the countries were excluded because of missing data."
    )
  }
  
  invisible(TRUE)
}

# Check the regression results of your examples
# (run this after unadjusted and moderated have been created)

check_analysis_result(unadjusted)
check_analysis_result(moderated)

# Check that invalid input produces an error, as intended

bad_input <- try(analyse_happiness("Not a variable"), silent = TRUE)

stopifnot("Invalid predictor should cause an error" = inherits(bad_input, "try-error"))

message("All error checks passed.")
