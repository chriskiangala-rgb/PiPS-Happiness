# Check the assumptions of the regression ----
# The numbers in the README's Methods section come from check_assumptions(); main.Rmd
# runs it, so they follow the data and the code.

#' Check the regression assumptions for every predictor
#'
#' Fits happiness ~ predictor with analyse_happiness(), the same model as
#' the app without a moderator (HC3, groups below 5 countries left out),
#' and checks for each predictor:
#' - unequal spread: the Breusch-Pagan test, and how much larger the HC3
#'   standard errors are than the normal ones;
#' - whether HC3 changes the conclusion (p < alpha or not);
#' - normal residuals: the Shapiro-Wilk test;
#' - a straight line or a curve: curvature_test(), the same rule as the
#'   app's warning;
#' - influential countries: Cook's distance.
#' Then, for a few key predictors, whether nearby countries are alike
#' (Moran's I, with and without continent as control) and whether the
#' effect survives standard errors clustered by continent.
#'
#' The tests document how common the violations are; they do not choose
#' the method. HC3 is used by default, because the Breusch-Pagan test
#' misses unequal spread that comes from a few extreme countries.
#'
#' @param data Data frame with happiness, the predictors, continent,
#'   Latitude and Longitude.
#' @param predictors Predictors to check.
#' @param key_predictors Predictors for the Moran's I and clustering checks.
#' @param alpha Significance level for the counts.
#'
#' @return A list with per_predictor (one row per predictor), summary (the
#'   counts in the README's Methods section), spatial (Moran's I) and clustered.
check_assumptions <- function(
    data = combined_data,
    predictors = available_predictors,
    key_predictors = c("Social support", "Logged GDP per capita",
                       "Freedom to make life choices", "Healthy life expectancy"),
    alpha = 0.05
) {
  key_predictors <- intersect(key_predictors, predictors)
  per_predictor <- do.call(rbind, lapply(predictors, check_predictor,
                                         data = data, predictors = predictors))
  per_predictor$conclusion_changes <- (per_predictor$p_normal < alpha) != (per_predictor$p_hc3 < alpha)
  numeric <- per_predictor$type == "numeric"

  summary <- data.frame(
    check = c(
      "Unequal spread (Breusch-Pagan)",
      "HC3 standard errors at least 20% larger",
      "Significance conclusion changes with HC3",
      "Non-normal residuals (Shapiro-Wilk)",
      "Extreme country (Cook's distance above 1)",
      "Curve fits better than a straight line",
      "Straight line misleading (the app's warning)"
    ),
    predictors = c(
      sum(per_predictor$spread_p < alpha),
      sum(per_predictor$se_ratio >= 1.2),
      sum(per_predictor$conclusion_changes),
      sum(per_predictor$normality_p < alpha),
      sum(per_predictor$max_cooks > 1),
      sum(per_predictor$curve_p < alpha, na.rm = TRUE),
      sum(per_predictor$line_misleading, na.rm = TRUE)
    ),
    out_of = c(rep(nrow(per_predictor), 5), rep(sum(numeric), 2))
  )

  list(
    per_predictor = per_predictor,
    summary = summary,
    spatial = do.call(rbind, lapply(key_predictors, spatial_check,
                                    data = data, predictors = predictors)),
    clustered = do.call(rbind, lapply(key_predictors, clustered_check,
                                      data = data, predictors = predictors))
  )
}

# All checks for one predictor in one row.
check_predictor <- function(predictor, data, predictors) {
  result <- analyse_happiness(predictor, data = data, predictors = predictors)
  model <- result$model
  n <- result$n_countries
  hc3 <- result$covariance
  normal <- stats::vcov(model)
  slopes <- names(stats::coef(model)) != "(Intercept)"
  cooks <- stats::cooks.distance(model)
  curve <- curvature_test(result)

  data.frame(
    predictor = predictor,
    type = if (is.numeric(result$model_data$primary)) "numeric" else "categorical",
    n = n,
    spread_p = lmtest::bptest(model)$p.value,
    se_ratio = max(sqrt(diag(hc3)[slopes] / diag(normal)[slopes])),
    p_normal = slopes_wald_p(model, normal),
    p_hc3 = slopes_wald_p(model, hc3),
    normality_p = stats::shapiro.test(stats::residuals(model))$p.value,
    n_influential = sum(cooks > 4 / n),
    max_cooks = max(cooks),
    curve_p = if (is.null(curve)) NA else curve$p_value,
    r_squared_line = result$r_squared,
    r_squared_curve = if (is.null(curve)) NA else curve$r_squared_curve,
    line_misleading = if (is.null(curve)) NA else curve$misleading,
    row.names = NULL
  )
}

# p-value of the Wald F-test that all slopes are 0, with a given covariance.
# For one slope this is the usual t-test; for a categorical predictor it
# tests all groups together.
slopes_wald_p <- function(model, covariance) {
  slopes <- names(stats::coef(model)) != "(Intercept)"
  estimates <- stats::coef(model)[slopes]
  f_value <- drop(t(estimates) %*% solve(covariance[slopes, slopes, drop = FALSE], estimates)) /
    sum(slopes)
  stats::pf(f_value, sum(slopes), stats::df.residual(model), lower.tail = FALSE)
}

# Moran's I of the residuals, alone and with continent as control. A check,
# not a correction: it changes no standard error.
spatial_check <- function(predictor, data, predictors) {
  residual_i <- function(result) {
    rows <- result$model_rows
    morans_i(stats::residuals(result$model), data$Latitude[rows], data$Longitude[rows])
  }
  alone <- residual_i(analyse_happiness(predictor, data = data, predictors = predictors))
  with_continent <- residual_i(analyse_happiness(predictor, data = data, predictors = predictors,
                                                 control_variables = "continent"))
  data.frame(
    predictor = predictor,
    i_alone = alone$statistic, p_alone = alone$p_value,
    i_with_continent = with_continent$statistic, p_with_continent = with_continent$p_value
  )
}

# The effect with continent as control, with standard errors clustered by
# continent as a robustness check. With few clusters the t-test uses
# (clusters - 1) degrees of freedom; even so, treat it with caution.
clustered_check <- function(predictor, data, predictors) {
  result <- analyse_happiness(predictor, data = data, predictors = predictors,
                              control_variables = "continent")
  continents <- droplevels(factor(data$continent[result$model_rows]))
  clustered <- sandwich::vcovCL(result$model, cluster = continents, type = "HC1")
  estimate <- stats::coef(result$model)[["primary"]]
  se <- sqrt(clustered["primary", "primary"])
  data.frame(
    predictor = predictor,
    estimate = estimate,
    se_hc3 = sqrt(result$covariance["primary", "primary"]),
    se_clustered = se,
    clusters = nlevels(continents),
    p_clustered = 2 * stats::pt(abs(estimate / se), df = nlevels(continents) - 1, lower.tail = FALSE)
  )
}

#' Moran's I: are nearby countries more alike than distant ones?
#'
#' Each country is compared with all other countries, weighted by inverse
#' great-circle distance between country centres, so nearby countries
#' weigh more. The weights of each country are scaled to add up to 1
#' (row standardisation): every country has the same total weight, so
#' densely packed regions such as Europe do not dominate the result.
#' The p-value uses the normal approximation. The test is one-sided: it
#' asks whether nearby countries are more alike, not less.
#'
#' @param values Numeric values per country, e.g. model residuals.
#' @param latitude,longitude Country coordinates in degrees, same order.
#'
#' @return A list with the statistic, the one-sided p-value and the number
#'   of countries with coordinates.
morans_i <- function(values, latitude, longitude) {
  usable <- is.finite(values) & is.finite(latitude) & is.finite(longitude)
  values <- values[usable]
  n <- length(values)
  if (n < 3L) stop("Moran's I needs at least 3 countries with coordinates.")

  latitude <- latitude[usable] * pi / 180
  longitude <- longitude[usable] * pi / 180
  # Distance along the earth's surface, in km. pmin guards against
  # rounding just above 1, which acos() cannot take.
  cosine <- outer(sin(latitude), sin(latitude)) +
    outer(cos(latitude), cos(latitude)) * cos(outer(longitude, longitude, "-"))
  distance <- 6371 * acos(pmin(cosine, 1))
  weights <- 1 / distance
  weights[!is.finite(weights)] <- 0  # the country itself, and shared centres
  diag(weights) <- 0
  weights <- weights / rowSums(weights)

  deviation <- values - mean(values)
  s0 <- sum(weights)
  statistic <- n / s0 * sum(weights * outer(deviation, deviation)) / sum(deviation^2)

  s1 <- sum((weights + t(weights))^2) / 2
  s2 <- sum((rowSums(weights) + colSums(weights))^2)
  expected <- -1 / (n - 1)
  variance <- (n^2 * s1 - n * s2 + 3 * s0^2) / ((n^2 - 1) * s0^2) - expected^2

  list(
    statistic = statistic,
    p_value = stats::pnorm((statistic - expected) / sqrt(variance), lower.tail = FALSE),
    n = n
  )
}
