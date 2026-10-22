# Prepare the combined happiness + world dataset ----
# Creates combined_data and available_predictors. Paths are relative to the
# project root, so run this with the repo folder as the working directory.

library(countrycode)

# Import datasets ----

# Read the world dataset.
# Keep columns as text for now; numbers will be cleaned later
world_data <- read.csv(
  "data/world-data-2023.csv",
  colClasses = "character",
  na.strings = "",
  check.names = FALSE
)

# Read the happiness dataset.
happiness_data <- read.csv("data/WHR2023.csv", check.names = FALSE)

# Standardize country names for matching ----

# Create matching columns, keeping the original country names unchanged.
world_data$join_country <- trimws(world_data$Country)
happiness_data$join_country <- trimws(happiness_data[["Country name"]])

# List the country names that need changing.
name_changes <- c(
  "Congo (Brazzaville)" = "Republic of the Congo",
  "Congo (Kinshasa)" = "Democratic Republic of the Congo",
  "Czechia" = "Czech Republic",
  "Gambia" = "The Gambia",
  "Ireland" = "Republic of Ireland",
  "State of Palestine" = "Palestinian National Authority",
  "Turkiye" = "Turkey"
)
control_variables = c("Logged GDP per capita", "Life expectancy")
# Replace these names in the happiness dataset's matching column.
for (old_name in names(name_changes)) {
  happiness_data$join_country[happiness_data$join_country == old_name] <- name_changes[[old_name]]
}

# Show countries in the happiness dataset without a world-data match.
unmatched_countries <- setdiff(happiness_data$join_country, world_data$join_country)
# Check duplicate countries and merge datasets ----

# Check that each dataset has only one row per country.
stopifnot(
  !anyDuplicated(happiness_data$join_country),
  !anyDuplicated(world_data$join_country)
)

# Combine the datasets using the matching country names.
combined_data <- merge(
  happiness_data,
  world_data,
  by = "join_country",
  all.x = TRUE,
  sort = FALSE
)

# Rename "Ladder score" to "happiness".
names(combined_data)[names(combined_data) == "Ladder score"] <- "happiness"
names(combined_data)[names(combined_data) == "Standard error of ladder score"] <- "SE of happiness score"

# Clean and convert numeric world-data columns ----

# These stay text. Keep Official language and Currency-Code here even though
# they are not used as predictors, otherwise they would be turned into NA.

text_columns <- c(
  "Country",
  "Abbreviation",
  "Capital/Major City",
  "Currency-Code",
  "Largest city",
  "Official language",
  "join_country"
)

# All other world-dataset columns should contain numbers.
numeric_columns <- setdiff(names(world_data), text_columns)

# Remove commas, currency symbols, percentages, and spaces.
# Then convert these columns to numbers in the combined dataset.
for (column in numeric_columns) {
  combined_data[[column]] <- as.numeric(gsub("[,$%[:space:]]", "", combined_data[[column]]))
}
# Transform and rescale predictors ----

# Before z-scoring, one log2 unit represents a doubling.
# After z-scoring, one unit represents one SD of the logged variable.
# Zero or negative values become NA.

log2_positive <- function(x) {
  x[!is.na(x) & x <= 0] <- NA
  log2(x)
}
# 1. Totals -> comparable measures
combined_data[["GDP per person"]] <- combined_data$GDP / combined_data$Population
combined_data[["CO2 per person"]] <- combined_data[["Co2-Emissions"]] / combined_data$Population
combined_data[["Armed forces per 1000 people"]] <-
  1000 * combined_data[["Armed Forces size"]] / combined_data$Population
combined_data[["Urban population (%)"]] <-
  100 * combined_data$Urban_population / combined_data$Population

combined_data[["log2(GDP per person)"]] <- log2_positive(combined_data[["GDP per person"]])
combined_data[["log2(CO2 per person)"]] <- log2_positive(combined_data[["CO2 per person"]])

# 2. Log the skewed variables (no log: minimum wage, CPI change, percentages)
log_vars <- c("Maternal mortality ratio", "Infant mortality", "Population",
              "Land Area(Km2)", "Density\n(P/Km2)", "CPI")
stopifnot(all(log_vars %in% names(combined_data)))
log_names <- paste0("log2(", gsub("\\s+", " ", log_vars), ")")

for (v in log_vars) {
  combined_data[[paste0("log2(", gsub("\\s+", " ", v), ")")]] <- log2_positive(combined_data[[v]])
}

# 3. Other variables
combined_data[["Distance from equator"]] <- abs(combined_data$Latitude)

# Assign continents and split North and South America ----
# Specify classifications for names that may not be recognized,
# and countries whose continent classification can vary.
continent_choices <- c(
  "Kosovo" = "Europe",
  "Hong Kong S.A.R. of China" = "Asia",
  "Taiwan Province of China" = "Asia",
  "State of Palestine" = "Asia",
  "Congo (Brazzaville)" = "Africa",
  "Congo (Kinshasa)" = "Africa",
  "Turkiye" = "Asia",
  "Russia" = "Europe",
  "Cyprus" = "Asia",
  "Georgia" = "Asia",
  "Armenia" = "Asia",
  "Azerbaijan" = "Asia",
  "Kazakhstan" = "Asia"
)

# Add a continent for each country.
combined_data$continent <- countrycode::countrycode(
  combined_data[["Country name"]],
  origin = "country.name",
  destination = "continent",
  custom_match = continent_choices
)

# Identify countries using standard three-letter country codes.
country_iso3 <- countrycode::countrycode(
  combined_data[["Country name"]],
  origin = "country.name",
  destination = "iso3c",
  warn = FALSE
)

# The 12 sovereign countries in South America.
south_america <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "ECU",
  "GUY", "PRY", "PER", "SUR", "URY", "VEN"
)

# Split the Americas.
is_americas <- combined_data$continent %in% "Americas"

combined_data$continent[
  is_americas & country_iso3 %in% south_america
] <- "South America"

combined_data$continent[
  is_americas & !is.na(country_iso3) &
    !(country_iso3 %in% south_america)
] <- "North America"

# Tell R that continent contains categories.
combined_data$continent <- factor(combined_data$continent)

# Define available predictors and identify variable types ----

# Select the explanatory variables from the happiness dataset.
happiness_predictors <- c(
  "Logged GDP per capita",
  "Social support",
  "Healthy life expectancy",
  "Freedom to make life choices",
  "Generosity",
  "Perceptions of corruption"
)

# Select the numeric world-dataset variables.
# Calling Code is an identifier, not a useful numeric predictor. Remove this from the variable-selection menu.
# Exclude identifiers and raw versions replaced by transformations.
replaced <- c(
  "GDP",
  "Co2-Emissions",
  "Urban_population",
  "Armed Forces size",
  "Latitude",
  log_vars
)

world_predictors <- setdiff(
  numeric_columns,
  c("Calling Code", replaced)
)

derived_predictors <- c(
  "log2(GDP per person)",
  "log2(CO2 per person)",
  "Urban population (%)",
  "Armed forces per 1000 people",
  "Distance from equator",
  log_names
)

available_predictors <- unique(c(
  happiness_predictors,
  world_predictors,
  derived_predictors,
  "continent"
))

# Identify whether each variable is numeric or categorical.
predictor_types <- data.frame(
  variable = available_predictors,
  type = vapply(combined_data[available_predictors], function(column) {
    if (is.numeric(column))
      "numeric"
    else
      "categorical"
  }, character(1)),
  row.names = NULL
)

# z-scores ----
# Keep original/transformed units in combined_data.
# Use scaled_data for modelling; happiness remains on its original scale.
# Missing values remain NA.

numeric_predictors <- predictor_types$variable[
  predictor_types$type == "numeric"
]

# Save the means and standard deviations used for scaling.
predictor_scale <- data.frame(
  variable = numeric_predictors,
  mean = vapply(
    numeric_predictors,
    function(p) mean(combined_data[[p]], na.rm = TRUE),
    numeric(1)
  ),
  sd = vapply(
    numeric_predictors,
    function(p) sd(combined_data[[p]], na.rm = TRUE),
    numeric(1)
  ),
  row.names = NULL
)
# Scaling requires finite values and a nonzero standard deviation.
stopifnot(
  all(is.finite(predictor_scale$mean)),
  all(is.finite(predictor_scale$sd)),
  all(predictor_scale$sd > 0),
  !any(vapply(
    combined_data[numeric_predictors],
    function(x) any(is.infinite(x) | is.nan(x)),
    logical(1)
  ))
)
# Create a modelling copy and standardize numeric predictors.
scaled_data <- combined_data

for (i in seq_along(numeric_predictors)) {
  p <- numeric_predictors[i]

  scaled_data[[p]] <- (
    combined_data[[p]] - predictor_scale$mean[i]
  ) / predictor_scale$sd[i]
}
# Validate analysis data ----

stopifnot(
  "happiness" %in% names(combined_data),
  is.numeric(combined_data$happiness),
  all(available_predictors %in% names(combined_data)),
  !anyDuplicated(available_predictors),
  !any(c("Calling Code", replaced, "Official language", "Currency-Code") %in% available_predictors),
  all(combined_data[["Urban population (%)"]] <= 100, na.rm = TRUE),
  !any(vapply(combined_data[numeric_predictors],
              function(x) any(is.infinite(x) | is.nan(x)), logical(1)))
 )
  
# How many countries each predictor covers
predictor_coverage <- data.frame(
  variable    = available_predictors,
  n_available = vapply(combined_data[available_predictors],
                       function(x) sum(!is.na(x)), integer(1)),
  n_total     = nrow(combined_data),
  row.names   = NULL
)
  
predictor_notes <- c(
  "log2(CPI)" = paste(
    "Check whether countries with extreme CPI values strongly influence",
    "the results; report a sensitivity analysis."
  ),
  "Longitude" = paste(
    "East-west position has no simple linear interpretation;",
    "interpret with caution."
  )
)
predictor_notes["Minimum wage"] <- paste(
  "Missing for about 1 in 6 countries, mostly richer ones, and some may have",
  "no national minimum wage. Results describe the countries with data."
)
