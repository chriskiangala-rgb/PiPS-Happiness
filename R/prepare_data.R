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
# Co2-Emissions is in kilotonnes; per person in tonnes.
combined_data[["CO2 per person"]] <- 1000 * combined_data[["Co2-Emissions"]] / combined_data$Population
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

for (i in seq_along(log_vars)) {
  combined_data[[log_names[i]]] <- log2_positive(combined_data[[log_vars[i]]])
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
# Exclude identifiers (Calling Code) and raw versions replaced by transformations.
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

predictor_scale <- data.frame(
  variable = numeric_predictors,
  n = vapply(
    numeric_predictors,
    function(p) sum(!is.na(combined_data[[p]])),
    integer(1)
  ),
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

# Variable list ----
# One row per predictor: a readable label, the unit, the source and the
# transformation, plus the number of countries with data and the SD (across
# all countries with data, after any log). Used for the app's menus, its
# "About the data" table and the effect per SD.

world_source <- "Global Country Information 2023"
whr_source <- "World Happiness Report 2023"

variable_details <- rbind(
  c("Logged GDP per capita", "GDP per capita (WHR)", "PPP, constant 2017 international $", whr_source, "natural log (in the WHR)"),
  c("Social support", "Social support", "share who have someone to count on (0-1)", whr_source, "none"),
  c("Healthy life expectancy", "Healthy life expectancy", "years", whr_source, "none"),
  c("Freedom to make life choices", "Freedom to make life choices", "share satisfied with their freedom (0-1)", whr_source, "none"),
  c("Generosity", "Generosity", "donating, relative to GDP (0 = as expected)", whr_source, "none"),
  c("Perceptions of corruption", "Perceptions of corruption", "share who see corruption (0-1)", whr_source, "none"),
  c("Agricultural Land( %)", "Agricultural land (%)", "% of land area", world_source, "none"),
  c("Birth Rate", "Birth rate", "births per 1,000 people per year", world_source, "none"),
  c("CPI Change (%)", "CPI change (%)", "% price change per year", world_source, "none"),
  c("Fertility Rate", "Fertility rate", "births per woman", world_source, "none"),
  c("Forested Area (%)", "Forested area (%)", "% of land area", world_source, "none"),
  c("Gasoline Price", "Gasoline price", "US$ per litre", world_source, "none"),
  c("Gross primary education enrollment (%)", "Primary education enrollment (%)", "% of the age group (can exceed 100)", world_source, "none"),
  c("Gross tertiary education enrollment (%)", "Tertiary education enrollment (%)", "% of the age group (can exceed 100)", world_source, "none"),
  c("Life expectancy", "Life expectancy", "years", world_source, "none"),
  c("Minimum wage", "Minimum wage", "US$ per hour", world_source, "none"),
  c("Out of pocket health expenditure", "Out-of-pocket health spending", "% of health spending", world_source, "none"),
  c("Physicians per thousand", "Physicians per 1,000 people", "physicians per 1,000 people", world_source, "none"),
  c("Population: Labor force participation (%)", "Labour force participation (%)", "% of people aged 15+", world_source, "none"),
  c("Tax revenue (%)", "Tax revenue (%)", "% of GDP", world_source, "none"),
  c("Total tax rate", "Total tax rate", "% of commercial profits", world_source, "none"),
  c("Unemployment rate", "Unemployment rate", "% of the labour force", world_source, "none"),
  c("Longitude", "Longitude", "degrees (negative = west)", world_source, "none"),
  c("log2(GDP per person)", "GDP per person", "US$ per person", world_source, "GDP / population, log2"),
  c("log2(CO2 per person)", "CO2 per person", "tonnes per person", world_source, "CO2 emissions / population, log2"),
  c("Urban population (%)", "Urban population (%)", "% of the population", world_source, "urban population / population"),
  c("Armed forces per 1000 people", "Armed forces per 1,000 people", "personnel per 1,000 people", world_source, "armed forces / population"),
  c("Distance from equator", "Distance from equator", "degrees latitude", world_source, "absolute latitude"),
  c("log2(Maternal mortality ratio)", "Maternal mortality", "deaths per 100,000 live births", world_source, "log2"),
  c("log2(Infant mortality)", "Infant mortality", "deaths per 1,000 live births", world_source, "log2"),
  c("log2(Population)", "Population", "people", world_source, "log2"),
  c("log2(Land Area(Km2))", "Land area", "km2", world_source, "log2"),
  c("log2(Density (P/Km2))", "Population density", "people per km2", world_source, "log2"),
  c("log2(CPI)", "Consumer price index", "index, 2010 = 100", world_source, "log2"),
  c("continent", "Continent", "category (North and South America split)", "countrycode package", "assigned from the country name")
)
variable_info <- data.frame(
  variable = variable_details[, 1],
  label = variable_details[, 2],
  unit = variable_details[, 3],
  source = variable_details[, 4],
  transformation = variable_details[, 5],
  row.names = NULL
)
variable_info$n <- predictor_coverage$n_available[match(variable_info$variable, predictor_coverage$variable)]
variable_info$sd <- predictor_scale$sd[match(variable_info$variable, predictor_scale$variable)]

stopifnot(
  "Every predictor needs a row in variable_info" =
    setequal(variable_info$variable, available_predictors),
  "Labels must be unique" = !anyDuplicated(variable_info$label)
)
