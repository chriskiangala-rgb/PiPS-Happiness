# Examples of the happiness analysis ----
# Run from the project root: source("examples/examples.R")
#
# Each example below asks one question about the data, runs
# analyse_happiness() to answer it and prints the result. The comments say
# what the question is and how to read the printed numbers, so you can follow
# along without knowing R. Predictor names must be written exactly as in the
# list printed first.
source("R/prepare_data.R")
source("R/analysis/analyse_happiness.R")

# Available predictors and the number of countries per continent
print(predictor_types, row.names = FALSE)
print(table(combined_data$continent, useNA = "ifany"))

# Example 1: one predictor, no moderator ----
# Question: are countries where people feel more socially supported happier?
# The "primary" row shows the estimate: how much higher the happiness score
# (0 to 10) is, on average, for each extra point of social support (0 to 1).
# A positive estimate means more support goes together with more happiness.
# The p_value says how likely a link this strong is if there were no link at
# all; below 0.05 is usually called significant. ci_lower and ci_upper give
# the 95% confidence interval: the range the true estimate is likely in.
unadjusted <- analyse_happiness(primary_predictor = "Social support")
print(unadjusted$primary_coefficients)

# Example 2: the same predictor with control variables ----
# Question: does social support still matter when we compare countries with
# the same GDP per person and life expectancy? Richer and healthier countries often
# have both more social support and more happiness, so part of the link in
# example 1 may come from those. Controlling for them takes that part out.
# Compare the "primary" estimate with example 1: if it gets smaller, part of
# the link was explained by GDP per person and life expectancy. all_coefficients also
# shows the estimates of the control variables themselves.
controlled <- analyse_happiness(
  primary_predictor = "Social support",
  control_variables = c("log2(GDP per person)", "Life expectancy")
)
print(controlled$primary_coefficients)
print(controlled$all_coefficients)

# Example 3: continent as moderator ----
# Question: does social support go together with happiness equally strongly
# on every continent? A moderator is a variable that may change the link
# between the predictor and happiness.
# The "primary" estimate is now the link in the reference continent (the
# first one alphabetically, Africa). Each "primary:moderator..." row in
# interaction_coefficients shows how much stronger (positive) or weaker
# (negative) the link is on that continent than in Africa.
# interaction_test asks one overall question: does the link differ between
# continents at all? A p_value below 0.05 says it does.
# Oceania has only 2 countries with data, too few for a link of its own, so
# it is left out (see excluded_groups). All standard errors, CIs and
# p-values are robust (HC3); use robust = FALSE for the normal ones.
moderated <- analyse_happiness(
  primary_predictor = "Social support",
  moderator = "continent"
)
print(moderated$excluded_groups)
print(moderated$interaction_test)
print(moderated$interaction_coefficients)
print(moderated$all_coefficients)

# Example 4: comparing only Asia and Europe ----
# Question: is the link between social support and happiness different in
# Asia than in Europe? selected_continents keeps only the countries of these
# two continents, and reference_continent makes Europe the continent the
# other is compared with.
# The "primary" estimate is the link in Europe. The "primary:moderatorAsia"
# row shows how much stronger or weaker it is in Asia.
asia_europe <- analyse_happiness(
  primary_predictor = "Social support",
  moderator = "continent",
  selected_continents = c("Asia", "Europe"),
  reference_continent = "Europe"
)
print(asia_europe$interaction_coefficients)
print(asia_europe$interaction_test)

# Example 5: a curve instead of a straight line ----
# Question: does happiness rise and then fall with labour force
# participation? A straight line finds almost nothing (R-squared near 0),
# but the countries with a medium participation are the happiest.
# shape = "curve" fits a natural spline that can bend. curve_test compares
# it with the straight line; r_squared and line_r_squared show how much
# each explains. The spline coefficients have no meaning on their own:
# look at the plot (plot_happiness(curved)).
curved <- analyse_happiness(
  primary_predictor = "Population: Labor force participation (%)",
  shape = "curve"
)
print(curved$curve_test)
print(c(curve = curved$r_squared, line = curved$line_r_squared))

# Interpretation of the results ----
# No moderator: the primary coefficient is its unadjusted association.
# With control variables: the primary coefficient is the association when
# the control variables are held equal.
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
# To compare only some continents, use selected_continents as in example 4.
# Check assumptions, outliers, and group sample sizes before inference.
# These country-level associations do not establish individual-level or
# causal effects. A nonsignificant interaction does not prove equal slopes.
