# Examples of the happiness analysis ----
# Run from the project root: source("examples/examples.R")
source("R/prepare_data.R")
source("R/analyse_happiness.R")

# Available predictors and the number of countries per continent
print(predictor_types, row.names = FALSE)
print(table(combined_data$continent, useNA = "ifany"))

# Examples (names must exactly match the predictor list) ----
unadjusted <- analyse_happiness(primary_predictor = "Social support")
print(unadjusted$primary_coefficients)

moderated <- analyse_happiness(
  primary_predictor = "Social support",
  moderator = "continent",
  data = combined_data
)
print(moderated$interaction_test)
print(moderated$interaction_coefficients)
print(moderated$all_coefficients)

# Asia versus Europe only (the category labels must be exactly Asia and Europe):
asia_europe <- combined_data[
  combined_data$continent %in% c("Asia", "Europe"), , drop = FALSE
]
asia_europe$continent <- relevel(
  droplevels(factor(asia_europe$continent)), ref = "Europe"
)
moderated <- analyse_happiness(
  primary_predictor = "Social support",
  moderator = "continent",
  data = asia_europe
)
print(moderated$interaction_coefficients)
print(moderated$interaction_test)

# Interpretation of the results ----
# No moderator: the primary coefficient is its unadjusted association.
# With a moderator: main effects are CONDITIONAL, not overall effects.
# Numeric moderator: the primary coefficient is the slope at moderator = 0.
# Factor moderator: it is the slope in the reference category.
# Numeric primary + factor moderator: interaction coefficients are slope
# differences compared with the reference category.
# Two numeric predictors: the interaction describes how one slope changes
# per one-unit increase in the other predictor.
# Centering numeric predictors can make zero a more meaningful value.
# Both categorical: the model is a two-way ANOVA with interaction.
# The overall interaction test asks whether adding the interaction improves
# the additive model; it does not provide every pairwise group comparison.
# To compare only Asia and Europe, filter the input data as above. The app
# needs additional group-selection controls if users should choose groups.
# Check assumptions, outliers, and group sample sizes before inference.
# These country-level associations do not establish individual-level or
# causal effects. A nonsignificant interaction does not prove equal slopes.
