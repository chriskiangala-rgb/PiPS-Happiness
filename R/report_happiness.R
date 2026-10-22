# Turn the result of analyse_happiness() into readable output ----

#' Make a readable coefficient table
#'
#' Replaces the internal model names (primary, moderator) with the chosen
#' variable names and rounds the numbers for display.
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return A data frame with one row per model term.
format_coefficients <- function(result) {
  coefficients <- result$all_coefficients
  data.frame(
    Term = vapply(coefficients$term, label_term, character(1),
                  primary = result$primary, moderator = result$moderator),
    Estimate = sprintf("%.3f", coefficients$estimate),
    `95% CI` = sprintf("[%.3f, %.3f]", coefficients$ci_lower, coefficients$ci_upper),
    p = format_p(coefficients$p_value),
    check.names = FALSE,
    row.names = NULL
  )
}

#' Describe the result in plain language
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return A character vector with one sentence per element.
describe_result <- function(result) {
  model_summary <- summary(result$model)
  sentences <- sprintf(
    "Based on %d countries (%d excluded because of missing data).",
    result$n_countries, result$n_excluded
  )

  if (!result$moderation) {
    model_test <- model_summary$fstatistic
    model_p <- pf(model_test[1], model_test[2], model_test[3], lower.tail = FALSE)

    if (is.numeric(result$model_data$primary)) {
      slope <- result$primary_coefficients
      sentences <- c(sentences, sprintf(
        "Countries with a one-unit higher %s have a %.2f points %s happiness score on average (95%% CI [%.2f, %.2f], p %s).",
        result$primary, abs(slope$estimate),
        if (slope$estimate >= 0) "higher" else "lower",
        slope$ci_lower, slope$ci_upper, format_p(slope$p_value, with_sign = TRUE)
      ))
    }
    sentences <- c(sentences, sprintf(
      "%s explains %.0f%% of the differences in happiness between countries (F(%d, %d) = %.2f, p %s).",
      result$primary, 100 * result$r_squared,
      as.integer(model_test[2]), as.integer(model_test[3]), model_test[1],
      format_p(model_p, with_sign = TRUE)
    ))
  } else {
    test <- result$interaction_test
    significant <- test$p_value < 0.05
    sentences <- c(sentences, sprintf(
      "Adding the interaction between %s and %s %s the model (F(%d, %d) = %.2f, p %s).",
      result$primary, result$moderator,
      if (significant) "significantly improves" else "does not significantly improve",
      as.integer(test$df_added), as.integer(df.residual(result$model)),
      test$f_value, format_p(test$p_value, with_sign = TRUE)
    ))
    sentences <- c(sentences, if (significant) {
      sprintf("So the association between %s and happiness differs depending on %s.",
              result$primary, result$moderator)
    } else {
      sprintf("There is no evidence that the association between %s and happiness depends on %s (which is not the same as evidence that it does not).",
              result$primary, result$moderator)
    })
  }

  c(sentences, "These are country-level associations: they do not show causal effects or effects on individual people.")
}

#' Summarise the main effect as one number with its 95% CI
#'
#' The effect depends on the analysis:
#' - numeric predictor: the slope (change in happiness per unit);
#' - categorical predictor: the largest difference between two groups, with
#'   a Tukey-adjusted CI and p-value (because the largest of many differences
#'   is picked out);
#' - with a moderator: the extra variance explained by the interaction
#'   (the R-squared change compared with the model without interaction).
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return A list with a title, the value and a detail line (CI and p-value).
key_effect <- function(result) {
  if (result$moderation) {
    test <- result$interaction_test
    r_squared_change <- result$r_squared - summary(result$additive_model)$r.squared
    return(list(
      title = "Extra variance explained by the interaction",
      value = sprintf("%.1f%%", 100 * r_squared_change),
      detail = sprintf("F(%d, %d) = %.2f, p %s",
                       as.integer(test$df_added), as.integer(df.residual(result$model)),
                       test$f_value, format_p(test$p_value, with_sign = TRUE))
    ))
  }

  if (is.numeric(result$model_data$primary)) {
    slope <- result$primary_coefficients
    return(list(
      title = paste("Change in happiness per unit of", result$primary),
      value = sprintf("%+.2f", slope$estimate),
      detail = sprintf("95%% CI [%.2f, %.2f], p %s",
                       slope$ci_lower, slope$ci_upper,
                       format_p(slope$p_value, with_sign = TRUE))
    ))
  }

  # Categorical predictor: all pairwise group differences, Tukey-adjusted.
  differences <- stats::TukeyHSD(
    stats::aov(happiness ~ primary, data = result$model_data)
  )$primary
  largest <- which.max(abs(differences[, "diff"]))
  groups <- strsplit(rownames(differences)[largest], "-", fixed = TRUE)[[1]]
  difference <- differences[largest, ]
  # Report the difference as positive: higher group vs lower group.
  if (difference[["diff"]] < 0) {
    groups <- rev(groups)
    difference[c("diff", "lwr", "upr")] <- -difference[c("diff", "upr", "lwr")]
  }
  list(
    title = "Largest difference in happiness between groups",
    value = sprintf("%+.2f", difference[["diff"]]),
    detail = sprintf("%s vs %s, 95%% CI [%.2f, %.2f], p %s (Tukey-adjusted)",
                     groups[1], groups[2], difference[["lwr"]], difference[["upr"]],
                     format_p(difference[["p adj"]], with_sign = TRUE))
  )
}

#' Describe one country's place in the analysis
#'
#' Gives the country's happiness and predictor value with their ranks among
#' the analysed countries, and how much happier or unhappier it is than the
#' model predicts. If the country was not analysed, says why.
#'
#' @param country A country name.
#' @param result The list returned by analyse_happiness().
#' @param data All countries, before the user's selection.
#' @param analysed_data The countries that were passed to analyse_happiness().
#'
#' @return A character vector with one sentence per element.
country_profile <- function(country, result, data, analysed_data) {
  if (!country %in% analysed_data[["Country name"]]) {
    return("Left out by your selection of continents or countries.")
  }
  rows <- as.integer(rownames(result$model_data))
  analysed_countries <- analysed_data[["Country name"]][rows]
  if (!country %in% analysed_countries) {
    variables <- c("happiness", result$primary, result$moderator)
    values <- analysed_data[analysed_data[["Country name"]] == country, variables]
    missing <- variables[vapply(values, function(value) is.na(value[[1]]), logical(1))]
    return(sprintf("Not in this analysis: missing data on %s.", paste(missing, collapse = ", ")))
  }

  position <- match(country, analysed_countries)
  n <- length(rows)
  happiness <- result$model_data$happiness
  predictor <- analysed_data[[result$primary]][rows]
  residual <- stats::residuals(result$model)[position]

  sentences <- sprintf(
    "Happiness %.2f: number %d of %d countries.",
    happiness[position], rank(-happiness, ties.method = "min")[position], n
  )
  sentences <- c(sentences, if (is.numeric(predictor)) {
    sprintf("%s %s: number %d of %d countries.",
            result$primary, format(signif(predictor[position], 3), big.mark = ","),
            rank(-predictor, ties.method = "min")[position], n)
  } else {
    sprintf("%s: %s.", result$primary, predictor[position])
  })
  if (result$moderation) {
    moderator <- analysed_data[[result$moderator]][rows][position]
    if (is.numeric(moderator)) moderator <- format(signif(moderator, 3), big.mark = ",")
    sentences <- c(sentences, sprintf("%s: %s.", result$moderator, moderator))
  }
  c(sentences, sprintf(
    "%.2f points %s than the model predicts.",
    abs(residual), if (residual >= 0) "happier" else "unhappier"
  ))
}

# Helpers ----

# Replace the internal names in a model term, e.g.
# "primary:moderatorEurope" -> "Social support x continent [Europe]".
label_term <- function(term, primary, moderator) {
  if (term == "(Intercept)") return("Intercept")
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  parts <- vapply(parts, function(part) {
    for (internal in c("primary", "moderator")) {
      variable <- if (internal == "primary") primary else moderator
      if (part == internal) return(variable)
      if (startsWith(part, internal)) {
        return(paste0(variable, " [", substring(part, nchar(internal) + 1), "]"))
      }
    }
    part
  }, character(1))
  paste(parts, collapse = " x ")
}

# Format p-values the APA way: no leading zero, and "< .001" for small values.
format_p <- function(p, with_sign = FALSE) {
  formatted <- ifelse(p < 0.001, "< .001", sub("^0", "", sprintf("%.3f", p)))
  if (with_sign) ifelse(p < 0.001, formatted, paste("=", formatted)) else formatted
}
