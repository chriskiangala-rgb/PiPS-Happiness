library(countrycode)

# Import datasets

# Read the world dataset.
# Keep columns as text for now; numbers will be cleaned later
world_data <- read.csv(
  "https://github.com/chriskiangala-rgb/PiPS_mood/blob/Data-and-stats/data/world-data-2023.csv?raw=true",
  colClasses = "character",
  na.strings = "",
  check.names = FALSE
) #changed link, so it is reproducible 

# Read the happiness dataset.
happiness_data <- read.csv(
  "https://github.com/chriskiangala-rgb/PiPS_mood/blob/Data-and-stats/data/WHR2023.csv?raw=true",
  check.names = FALSE
) #changed the link again


# Standardize country names for matching

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
  "State of Palestine" = "Palestinian National Authority"
) 


# Replace these names in the happiness dataset's matching column.
for (old_name in names(name_changes)) {
  happiness_data$join_country[happiness_data$join_country == old_name] <- name_changes[[old_name]]
}

# Show countries in the happiness dataset without a world-data match.
unmatched_countries <- setdiff(happiness_data$join_country, world_data$join_country)

# Check duplicate countries and merge datasets

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

# Rename "Ladder" to "happiness".
names(combined_data)[names(combined_data) == "Ladder score"] <- "happiness"
names(combined_data)[names(combined_data) == "Standard error of ladder score"] <- "SE of happiness score"


# Clean and convert numeric world-data columns

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

# Check a few converted columns.
str(combined_data[, c(
 "happiness",
 "GDP",
 "Population",
 "Unemployment rate"
)])

# Assign continents and split North and South America

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

# Check the categories and country counts.
print(table(combined_data$continent, useNA = "ifany"))

# Define available predictors and identify variable types

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

# Validate analysis data and display predictor types

stopifnot(
  "happiness" %in% names(combined_data),
  is.numeric(combined_data$happiness),
  all(available_predictors %in% names(combined_data))
)

# Comment out an earlier print(predictor_types) to avoid duplicate output.
print(predictor_types, row.names = FALSE)

# Define happiness regression with an optional moderator

analyse_happiness <- function(
    primary_predictor,
    moderator = "",
    data = combined_data,
    predictors = available_predictors
) {
  # Validate selected predictors and outcome
  
  if (length(primary_predictor) != 1L || is.na(primary_predictor) ||
      !primary_predictor %in% predictors) {
    stop("Choose a valid primary predictor.")
  }
  if (is.null(moderator)) moderator <- ""
  if (length(moderator) != 1L || is.na(moderator) ||
      (moderator != "" && !moderator %in% predictors)) {
    stop("Choose a valid moderator or leave it empty.")
  }
  if (moderator == primary_predictor) {
    stop("The primary predictor and moderator must be different.")
  }
  selected_columns <- c("happiness", primary_predictor)
  if (moderator != "") selected_columns <- c(selected_columns, moderator)
  if (!all(selected_columns %in% names(data))) {
    stop("A selected column is missing from the supplied data.")
  }
  if (!is.numeric(data$happiness)) stop("Happiness must be numeric.")
  
  # Prepare model data and exclude missing or nonfinite values
  
  # Internal names avoid problems with spaces in original column names.
  model_data <- data.frame(
    happiness = data$happiness,
    primary = data[[primary_predictor]]
  )
  if (moderator != "") model_data$moderator <- data[[moderator]]
  
  # Use complete, finite values for every selected variable.
  usable <- rep(TRUE, nrow(model_data))
  for (column in names(model_data)) {
    values <- model_data[[column]]
    if (is.numeric(values)) {
      usable <- usable & is.finite(values)
    } else {
      usable <- usable & !is.na(values) & trimws(as.character(values)) != ""
    }
  }
  model_data <- model_data[usable, , drop = FALSE]
  if (nrow(model_data) < 3L) {
    stop("Too few countries with complete data for these selections.")
  }
  
  # Set categorical reference groups and check variable variation
  
  # Non numeric predictors become factors. Numeric-coded categories must
  # already be converted to factors in your data-preparation steps.
  for (column in setdiff(names(model_data), "happiness")) {
    if (!is.numeric(model_data[[column]])) {
      model_data[[column]] <- droplevels(factor(model_data[[column]]))
    }
    if (length(unique(model_data[[column]])) < 2L) {
      stop(paste("No variation in", column, "among the available countries."))
    }
    if (is.factor(model_data[[column]])) {
      # Explicit treatment coding: first level is the reference category.
      contrasts(model_data[[column]]) <- contr.treatment(
        levels(model_data[[column]]), base = 1
      )
    }
  }
  if (length(unique(model_data$happiness)) < 2L) {
    stop("Happiness has no variation among the available countries.")
  }
  
  # Fit the regression model and check estimability
  
  # includes both main effects AND their interaction.
  formula <- if (moderator == "") {
    happiness ~ primary
  } else {
    happiness ~ primary * moderator
  }
  model <- lm(formula, data = model_data)
  
  if (model$rank < ncol(model.matrix(model))) {
    stop("The model cannot estimate all coefficients independently. Choose different variables or groups.")
  }
  if (df.residual(model) < 1L) {
    stop("Too few countries for this model, possibly because there are too many categories or interactions.")
  }
  
  # Extract coefficients, p-values, and confidence intervals
  
  model_summary <- summary(model)
  intervals <- confint(model)
  coefficient_table <- data.frame(
    term = rownames(model_summary$coefficients),
    estimate = model_summary$coefficients[, 1],
    std_error = model_summary$coefficients[, 2],
    t_value = model_summary$coefficients[, 3],
    p_value = model_summary$coefficients[, 4],
    ci_lower = intervals[, 1],
    ci_upper = intervals[, 2],
    row.names = NULL
  )
  
  # Separate primary and interaction coefficients
  
  # Separate main-effect coefficients from interaction coefficients.
  is_interaction <- grepl(":", coefficient_table$term, fixed = TRUE)
  primary_coefficients <- coefficient_table[
    startsWith(coefficient_table$term, "primary") & !is_interaction,
    , drop = FALSE
  ]
  interaction_coefficients <- coefficient_table[
    is_interaction, , drop = FALSE
  ]
  
  # Test the overall interaction against the additive model
  
  # Overall moderation test: compare the additive and interaction models
  # using exactly the same countries. This tests all interaction terms jointly,
  # which is important when a categorical variable has more than two levels.
  interaction_test <- NULL
  additive_model <- NULL
  if (moderator != "") {
    additive_model <- lm(happiness ~ primary + moderator, data = model_data)
    comparison <- anova(additive_model, model)
    interaction_test <- data.frame(
      df_added = comparison$Df[2],
      f_value = comparison$F[2],
      p_value = comparison$`Pr(>F)`[2]
    )
  }
  
  # Identify reference categories and return analysis results
  
  reference_categories <- lapply(
    model_data[vapply(model_data, is.factor, logical(1))],
    function(column) levels(column)[1]
  )
  
  list(
    primary = primary_predictor,
    moderator = if (moderator == "") NULL else moderator,
    moderation = moderator != "",
    n_countries = nrow(model_data),
    n_excluded = nrow(data) - nrow(model_data),
    primary_coefficients = primary_coefficients,
    interaction_coefficients = interaction_coefficients,
    interaction_test = interaction_test,
    all_coefficients = coefficient_table,
    reference_categories = reference_categories,
    r_squared = model_summary$r.squared,
    model = model,
    additive_model = additive_model,
    model_data = model_data
  )
}

