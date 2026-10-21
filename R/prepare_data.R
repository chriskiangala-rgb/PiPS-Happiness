# Prepare the combined happiness + world dataset ----
# Creates combined_data and available_predictors. Paths are relative to the
# project root, so run this with the repo folder as working directory.

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

# Replace these names in the happiness dataset's matching column.
for (old_name in names(name_changes)) {
  happiness_data$join_country[happiness_data$join_country == old_name] <- name_changes[[old_name]]
}

# Show countries in the happiness dataset without a world-data match.
unmatched_countries <- setdiff(happiness_data$join_country, world_data$join_country)

# Check duplicate countries and merge datasets ----

# Check that each dataset has only one row per country.
stopifnot(
  !anyDuplicated(happiness_data$join_country),!anyDuplicated(world_data$join_country)
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

# Identify the world-dataset columns that should stay as text.
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
# Calling Code is an identifier, not a useful numeric predictor. Remove this from variable-selection menu.
world_predictors <- setdiff(numeric_columns, "Calling Code")

# Combine the options and add categorical variables.
available_predictors <- unique(
  c(
    happiness_predictors,
    world_predictors,
    "continent",
    "Official language",
    "Currency-Code"
  )
)

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

# Validate analysis data ----

stopifnot(
  "happiness" %in% names(combined_data),
  is.numeric(combined_data$happiness),
  all(available_predictors %in% names(combined_data))
)
