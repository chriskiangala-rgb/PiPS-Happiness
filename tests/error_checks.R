# Run from the project root: source("tests/error_checks.R")
# internals = TRUE: these checks also inspect the raw data and the helpers.
source("R/load_project.R")
load_project(internals = TRUE)

# Error checks for the happiness data analysis

# Check that load_project() shows only the high-level functions and the
# prepared data, not the helpers or intermediate objects
shown <- new.env()
load_project(envir = shown)
stopifnot(
  "load_project() should show only the high-level functions and the data" = setequal(
    ls(shown),
    c("analyse_happiness", "plot_happiness", "format_coefficients", "describe_result",
      "check_assumptions", "happiness_app", "group_predictors", "combined_data",
      "available_predictors", "predictor_types", "predictor_notes", "variable_info")
  ),
  "The shown functions should still find their helpers" =
    is.data.frame(shown$format_coefficients(shown$analyse_happiness("Social support")))
)

# Check that data preparation created the objects used below
stopifnot(
  "Run the data preparation code first" = all(
    c("world_data", "happiness_data", "combined_data", "name_changes",
      "numeric_columns", "available_predictors", "unmatched_countries") %in%
      ls(envir = globalenv())
  )
)

#Check for unmatched countries

print(unmatched_countries)

known_unmatched <- c("Kosovo", "Taiwan Province of China", "Hong Kong S.A.R. of China")

stopifnot("Unexpected unmatched countries" = all(unmatched_countries %in% known_unmatched))

#There are unmatched countries. Maybe the app should put out a warning
#that these countries cannot be used for the analysis

if (length(unmatched_countries) > 0) {
  warning(
    "These countries could not be matched and are excluded from analyses using world-data variables: ",
    paste(unmatched_countries, collapse = ", ")
  )
}

# Check the imported datasets

stopifnot(
  "world_data is empty"                     = nrow(world_data) > 0,
  "world_data has too few columns"          = ncol(world_data) > 10,
  "happiness_data has too few rows"         = nrow(happiness_data) > 100,
  "Duplicate column names in world_data"    = !anyDuplicated(names(world_data)),
  "Expected world_data columns missing"     = all(c("Country", "GDP", "Population") %in% names(world_data)),
  "Expected happiness_data columns missing" = all(c("Country name", "Ladder score") %in% names(happiness_data))
)

# Check that required columns exist before accessing them.
stopifnot(
  "join_country missing from happiness_data" =
    "join_country" %in% names(happiness_data),

  "join_country missing from world_data" =
    "join_country" %in% names(world_data),

  "Required combined_data columns missing" =
    all(c("happiness", "GDP", "Population", "Unemployment rate", "continent") %in%
          names(combined_data)),

  "numeric_columns is empty" =
    length(numeric_columns) > 0L,

  "A numeric_columns entry is missing from combined_data" =
    all(numeric_columns %in% names(combined_data)),

  "Unemployment rate is not numeric" =
    is.numeric(combined_data[["Unemployment rate"]])
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

unadjusted <- analyse_happiness(primary_predictor = "Social support")
moderated <- analyse_happiness(primary_predictor = "Social support", moderator = "continent")

check_analysis_result(unadjusted)
check_analysis_result(moderated)

# Check that invalid input produces an error, as intended

bad_input <- try(analyse_happiness("Not a variable"), silent = TRUE)

stopifnot("Invalid predictor should cause an error" = inherits(bad_input, "try-error"))

# Check that a control variable with a colon in its name is not counted as an
# interaction (continent as moderator gives 5 interaction terms)

colon_control <- analyse_happiness(
  primary_predictor = "Social support", moderator = "continent",
  control_variables = "Population: Labor force participation (%)"
)

stopifnot(
  "A control with a colon counts as an interaction" =
    !"Population: Labor force participation (%)" %in% colon_control$interaction_coefficients$term,
  "Wrong number of interaction terms" =
    nrow(colon_control$interaction_coefficients) == nlevels(colon_control$model_data$moderator) - 1
)

# Check that every control keeps its own name with 10 or more controls
# (control1 must not also match control10)

many_controls <- available_predictors[vapply(
  combined_data[available_predictors], is.numeric, logical(1)
)]
many_controls <- setdiff(many_controls, "Social support")[1:10]
ten_controls <- analyse_happiness(
  primary_predictor = "Social support", control_variables = many_controls
)

stopifnot(
  "Control names are wrong with 10 or more controls" =
    all(many_controls %in% ten_controls$all_coefficients$term)
)

# Check that errors name the real variable, not an internal name like control1

one_continent <- try(
  analyse_happiness(
    primary_predictor = "Social support", control_variables = "continent",
    data = combined_data[combined_data$continent == "Europe", ]
  ),
  silent = TRUE
)

stopifnot(
  "Error should name the real variable" =
    grepl("No variation in continent", one_continent)
)

# Check the robust standard errors (HC3) and the small groups

robust_result <- analyse_happiness("Social support", control_variables = "Logged GDP per capita")
normal_result <- analyse_happiness("Social support", control_variables = "Logged GDP per capita",
                                   robust = FALSE)
oceania_reference <- try(
  analyse_happiness("Social support", moderator = "continent", reference_continent = "Oceania"),
  silent = TRUE
)

stopifnot(
  "HC3 should not change the estimates" =
    isTRUE(all.equal(robust_result$all_coefficients$estimate, normal_result$all_coefficients$estimate)),
  "robust = FALSE should give the normal standard errors" =
    isTRUE(all.equal(normal_result$all_coefficients$std_error,
                     unname(summary(normal_result$model)$coefficients[, 2]))),
  "Standard errors should be HC3" =
    isTRUE(all.equal(robust_result$all_coefficients$std_error,
                     unname(sqrt(diag(sandwich::vcovHC(robust_result$model, type = "HC3")))))),
  "The covariance should be returned for the app" =
    identical(dim(robust_result$covariance), rep(nrow(robust_result$all_coefficients), 2L)),
  "Continent as moderator should leave out Oceania (2 countries)" =
    identical(moderated$excluded_groups$continent, "Oceania") && moderated$n_small_groups == 2,
  "model_rows should leave out the small group too" =
    length(moderated$model_rows) == moderated$n_countries &&
    !any(combined_data$continent[moderated$model_rows] == "Oceania"),
  "With Oceania left out, every standard error should be a number" =
    all(is.finite(moderated$all_coefficients$std_error)),
  "A reference continent that is too small should give a clear error" =
    inherits(oceania_reference, "try-error") && grepl("Oceania", oceania_reference),
  "Without a categorical variable, no groups are left out" =
    length(unadjusted$excluded_groups) == 0 && unadjusted$n_small_groups == 0
)

# Check the curve (natural spline)

labour <- "Population: Labor force participation (%)"
curve_result <- analyse_happiness(labour, shape = "curve")
curve_normal <- analyse_happiness(labour, shape = "curve", robust = FALSE,
                                  control_variables = "log2(GDP per person)")
line_normal <- analyse_happiness(labour, robust = FALSE, control_variables = "log2(GDP per person)")
classic_test <- anova(line_normal$model, curve_normal$model)
curve_with_moderator <- try(analyse_happiness(labour, "continent", shape = "curve"), silent = TRUE)
curve_categorical <- try(analyse_happiness("continent", shape = "curve"), silent = TRUE)

stopifnot(
  "The curve should be a natural spline with 3 degrees of freedom" =
    nrow(curve_result$primary_coefficients) == 3 &&
    isTRUE(all.equal(curve_result$r_squared, summary(stats::lm(
      happiness ~ splines::ns(primary, df = 3), data = curve_result$model_data))$r.squared)),
  "The curve should fit at least as well as the line" =
    curve_result$r_squared >= curve_result$line_r_squared,
  "Without HC3 the curve test should equal the classic F-test" =
    isTRUE(all.equal(curve_normal$curve_test$f_value, classic_test$F[2])) &&
    isTRUE(all.equal(curve_normal$curve_test$p_value, classic_test$`Pr(>F)`[2])),
  "Labour force participation should clearly bend" = curve_result$curve_test$p_value < 0.001,
  "A curve with a moderator should give a clear error" =
    inherits(curve_with_moderator, "try-error") && grepl("moderator", curve_with_moderator),
  "A curve needs a numeric predictor" =
    inherits(curve_categorical, "try-error") && grepl("numeric", curve_categorical),
  "A straight line is the default" = unadjusted$shape == "line" && is.null(unadjusted$curve_test)
)

# Check the assumption checks ----

checks <- check_assumptions()
per_predictor <- checks$per_predictor
row_of <- function(predictor) per_predictor[per_predictor$predictor == predictor, ]
social_support <- analyse_happiness("Social support")
social_support_line <- summary(social_support$model)$coefficients

stopifnot(
  "There should be one row per predictor" =
    nrow(per_predictor) == length(available_predictors) &&
    setequal(per_predictor$predictor, available_predictors),
  "The HC3 p-value should equal the one of analyse_happiness()" =
    isTRUE(all.equal(row_of("Social support")$p_hc3, social_support$primary_coefficients$p_value)),
  "The normal p-value should equal the one of lm()" =
    isTRUE(all.equal(row_of("Social support")$p_normal, social_support_line["primary", "Pr(>|t|)"])),
  "Only numeric predictors get a curve check" =
    all(is.na(per_predictor$curve_p[per_predictor$type == "categorical"])),
  "Labour force participation should be flagged as misleading as a line" =
    isTRUE(row_of("Population: Labor force participation (%)")$line_misleading),
  "Freedom should not be flagged" =
    isFALSE(row_of("Freedom to make life choices")$line_misleading),
  "The summary counts should fit within their totals" =
    all(checks$summary$predictors <= checks$summary$out_of),
  "Continent as control should reduce Moran's I" =
    all(checks$spatial$i_with_continent < checks$spatial$i_alone),
  "Clustering should use the 6 continents" = all(checks$clustered$clusters == 6)
)

message("All error checks passed.")
