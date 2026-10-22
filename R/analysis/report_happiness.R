# Turn the result of analyse_happiness() into readable output ----

#' Make a readable coefficient table
#'
#' Replaces the internal model names (primary, moderator) with the chosen
#' variable names, marks the groups of categorical control variables and
#' rounds the numbers for display.
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return A data frame with one row per model term.
format_coefficients <- function(result) {
  coefficients <- result$all_coefficients
  data.frame(
    Term = vapply(coefficients$term, label_term, character(1),
                  primary = display_name(result, result$primary),
                  moderator = display_name(result, result$moderator),
                  controls = result$control_variables,
                  control_labels = display_name(result, result$control_variables)),
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
#' @param sd Optional SD of the predictor across all countries with data
#'   (variable_info$sd), to also give the effect per SD.
#'
#' @return A character vector with one sentence per element.
describe_result <- function(result, sd = NULL) {
  model_summary <- summary(result$model)
  primary <- display_name(result, result$primary)
  moderator <- display_name(result, result$moderator)
  sentences <- sprintf(
    "Based on %d countries (%d excluded because of missing data).",
    result$n_countries, result$n_excluded
  )
  for (variable in names(result$excluded_groups)) {
    groups <- result$excluded_groups[[variable]]
    sentences <- c(sentences, sprintf(
      "Left out: %s (%s), because %s fewer than %d countries with data.",
      paste(groups, collapse = ", "), tolower(display_name(result, variable)),
      if (length(groups) == 1) "it has" else "they have", result$min_group_size
    ))
  }
  controls <- result$control_variables
  if (length(controls) > 0) {
    sentences <- c(sentences, sprintf(
      "Controlling for %s: the results compare countries that are equal on %s.",
      paste(display_name(result, controls), collapse = ", "),
      if (length(controls) == 1) "it" else "these"
    ))
  }

  if (!result$moderation) {
    # Test all variables together against a model without them, with the
    # same covariance (HC3) as the coefficients.
    intercept_only <- stats::lm(happiness ~ 1, data = result$model_data)
    overall <- lmtest::waldtest(intercept_only, result$model, vcov = result$covariance, test = "F")
    model_test <- c(overall$F[2], overall$Df[2], overall$Res.Df[2])
    model_p <- overall$`Pr(>F)`[2]

    if (identical(result$shape, "curve")) {
      sentences <- c(sentences, describe_curve(result))
    } else if (is.numeric(result$model_data$primary)) {
      slope <- result$primary_coefficients
      step <- ten_times(result$primary)
      scale <- log_scale(result$primary)
      sentences <- c(sentences, sprintf(
        "Countries with %s have a %.2f points %s happiness score on average (95%% CI [%.2f, %.2f], p %s).",
        if (is.null(scale)) {
          paste("a one-unit higher", primary)
        } else {
          paste("10 times more", scale$name)
        },
        abs(slope$estimate * step),
        if (slope$estimate >= 0) "higher" else "lower",
        slope$ci_lower * step, slope$ci_upper * step,
        format_p(slope$p_value, with_sign = TRUE)
      ))
      per_sd <- per_sd_effect(result, sd)
      if (!is.null(per_sd)) {
        sentences <- c(sentences, sprintf(
          "Per standard deviation (%s, across all countries with data): %+.2f points (95%% CI [%.2f, %.2f]).",
          per_sd$size, per_sd$estimate, per_sd$ci_lower, per_sd$ci_upper
        ))
      }
    }
    sentences <- c(sentences, sprintf(
      "%s explain%s %.0f%% of the differences in happiness between countries (F(%d, %d) = %.2f, p %s).",
      if (length(controls) > 0) paste(primary, "and the control variables together") else primary,
      if (length(controls) > 0) "" else "s",
      100 * result$r_squared,
      as.integer(model_test[2]), as.integer(model_test[3]), model_test[1],
      format_p(model_p, with_sign = TRUE)
    ))
    # With controls, the total includes what the controls explain; also
    # give what the predictor adds on top of them.
    if (length(controls) > 0) {
      controls_only <- stats::lm(
        stats::reformulate(names(result$control_mapping), response = "happiness"),
        data = result$model_data
      )
      added <- lmtest::waldtest(controls_only, result$model, vcov = result$covariance, test = "F")
      sentences <- c(sentences, sprintf(
        "%s adds %.0f%% on top of the control variables (F(%d, %d) = %.2f, p %s).",
        primary, 100 * (result$r_squared - summary(controls_only)$r.squared),
        as.integer(added$Df[2]), as.integer(added$Res.Df[2]), added$F[2],
        format_p(added$`Pr(>F)`[2], with_sign = TRUE)
      ))
    }
  } else {
    test <- result$interaction_test
    significant <- test$p_value < 0.05
    sentences <- c(sentences, sprintf(
      "Adding the interaction between %s and %s %s the model (F(%d, %d) = %.2f, p %s).",
      primary, moderator,
      if (significant) "significantly improves" else "does not significantly improve",
      as.integer(test$df_added), as.integer(stats::df.residual(result$model)),
      test$f_value, format_p(test$p_value, with_sign = TRUE)
    ))
    sentences <- c(sentences, if (significant) {
      sprintf("So the association between %s and happiness differs depending on %s.",
              primary, moderator)
    } else {
      sprintf("There is no evidence that the association between %s and happiness depends on %s (which is not the same as evidence that it does not).",
              primary, moderator)
    })
  }

  curve <- curvature_test(result)
  if (!is.null(curve) && curve$misleading) {
    sentences <- c(sentences, describe_curvature(curve, result))
  }

  if (isTRUE(result$robust)) {
    sentences <- c(sentences, paste(
      "Standard errors, confidence intervals and p-values are robust (HC3):",
      "they do not assume that the spread around the line is the same for every country."
    ))
  }

  c(sentences, "These are country-level associations: they do not show causal effects or effects on individual people.")
}

#' Summarise the main effect as one number with its 95% CI
#'
#' The effect depends on the analysis:
#' - numeric predictor: the slope (change in happiness per unit);
#' - categorical predictor: the largest difference between two groups,
#'   computed from the fitted lm model, with a Bonferroni-adjusted CI and
#'   p-value (because the largest of many differences is picked out);
#' - with a moderator: the interaction estimate, followed by the effect of
#'   the predictor without the moderator on the same countries. With several
#'   interaction terms (a categorical variable with more than two groups),
#'   the clearest one is shown, Bonferroni-adjusted for picking it out.
#'
#' A logged predictor (see log_scale()) is shown per 10 times more instead
#' of per log unit, with an example from two real countries.
#'
#' @param result The list returned by analyse_happiness().
#' @param countries Optional country names in the order of the model
#'   observations, for the example of a logged predictor.
#' @param sd Optional SD of the predictor across all countries with data
#'   (variable_info$sd), for the effect per SD.
#'
#' @return A list with a title, the value, a detail line (CI and p-value)
#'   and, with a moderator, a baseline line with the effect without it. For
#'   a numeric predictor without a moderator, also the effect per SD (when
#'   sd is given) and, for a logged predictor, an example line.
key_effect <- function(result, countries = NULL, sd = NULL) {
  if (result$moderation) {
    terms <- result$interaction_coefficients
    n_terms <- nrow(terms)
    chosen <- terms[which.max(abs(terms$t_value)), ]
    critical_t <- stats::qt(1 - 0.05 / (2 * n_terms), stats::df.residual(result$model))
    # Per 10 times more for a logged predictor or moderator (only numeric
    # variables have a per-unit effect).
    step <- interaction_step(result)
    chosen[c("estimate", "std_error")] <- chosen[c("estimate", "std_error")] * step
    return(list(
      title = paste0(
        describe_interaction(chosen$term, result),
        if (n_terms > 1) sprintf(" (clearest of %d)", n_terms)
      ),
      value = sprintf("%+.2f", chosen$estimate),
      detail = sprintf(
        "95%% CI [%.2f, %.2f], p %s%s",
        chosen$estimate - critical_t * chosen$std_error,
        chosen$estimate + critical_t * chosen$std_error,
        format_p(min(1, chosen$p_value * n_terms), with_sign = TRUE),
        if (n_terms > 1) " (Bonferroni-adjusted)" else ""
      ),
      baseline = effect_without_moderator(result)
    ))
  }

  if (identical(result$shape, "curve")) {
    difference <- curve_difference(result)
    return(list(
      title = paste("Change in happiness from a low to a high",
                    primary_label(result), "(curve)"),
      value = sprintf("%+.2f", difference$estimate),
      detail = sprintf("95%% CI [%.2f, %.2f], p %s",
                       difference$ci_lower, difference$ci_upper,
                       format_p(difference$p_value, with_sign = TRUE)),
      example = if (!is.null(countries)) curve_example(result, difference, countries),
      # Low to high can be near 0 for a curve that goes up and down; this
      # line shows that the curve can still matter.
      baseline = sprintf(
        "The curve explains %.0f%%, a straight line %.0f%% (curve vs line: p %s)",
        100 * result$r_squared, 100 * result$line_r_squared,
        format_p(result$curve_test$p_value, with_sign = TRUE)
      )
    ))
  }

  if (is.numeric(result$model_data$primary)) {
    slope <- result$primary_coefficients
    step <- ten_times(result$primary)
    return(list(
      title = paste("Change in happiness",
                    per_step(result$primary, display_name(result, result$primary))),
      value = sprintf("%+.2f", slope$estimate * step),
      detail = sprintf("95%% CI [%.2f, %.2f], p %s",
                       slope$ci_lower * step, slope$ci_upper * step,
                       format_p(slope$p_value, with_sign = TRUE)),
      example = if (!is.null(countries)) log_example(result, countries),
      per_sd = {
        per_sd <- per_sd_effect(result, sd)
        if (!is.null(per_sd)) {
          sprintf("Per SD (%s): %+.2f, 95%% CI [%.2f, %.2f]",
                  per_sd$size, per_sd$estimate, per_sd$ci_lower, per_sd$ci_upper)
        }
      }
    ))
  }

  largest <- largest_difference(result$model, levels(result$model_data$primary), result$covariance)
  list(
    title = "Largest difference in happiness between groups",
    value = sprintf("%+.2f", largest$estimate),
    detail = sprintf("%s vs %s, 95%% CI [%.2f, %.2f], p %s (Bonferroni-adjusted for %d comparisons)",
                     largest$higher, largest$lower, largest$ci_lower, largest$ci_upper,
                     format_p(largest$p_value, with_sign = TRUE), largest$n_comparisons)
  )
}

# Say in plain words what an interaction coefficient means, e.g.
# "primary:moderatorAsia" -> "Difference in the effect of Social support:
# Asia vs Africa".
describe_interaction <- function(term, result) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  group <- sub("^(primary|moderator)", "", parts[parts != "primary" & parts != "moderator"])
  primary_numeric <- is.numeric(result$model_data$primary)
  moderator_numeric <- is.numeric(result$model_data$moderator)
  # "Social support", or "10 times more GDP per person" for a logged predictor.
  scale <- log_scale(result$primary)
  primary <- if (is.null(scale)) {
    display_name(result, result$primary)
  } else {
    paste("10 times more", scale$name)
  }
  moderator_step <- per_step(result$moderator, display_name(result, result$moderator))
  if (primary_numeric && moderator_numeric) {
    sprintf("Change in the effect of %s %s", primary, moderator_step)
  } else if (primary_numeric) {
    sprintf("Difference in the effect of %s: %s vs %s",
            primary, group, result$reference_categories$moderator)
  } else if (moderator_numeric) {
    sprintf("Change in the %s vs %s difference %s",
            group, result$reference_categories$primary, moderator_step)
  } else {
    paste("Interaction:", label_term(term, display_name(result, result$primary),
                                     display_name(result, result$moderator)))
  }
}

# The effect of the predictor without the moderator, on the same countries
# and with the same control variables as the moderated model, so the two
# can be compared.
effect_without_moderator <- function(result) {
  initial <- stats::lm(
    stats::reformulate(c("primary", names(result$control_mapping)), response = "happiness"),
    data = result$model_data
  )
  covariance <- model_covariance(initial, isTRUE(result$robust))
  if (is.numeric(result$model_data$primary)) {
    step <- ten_times(result$primary)
    interval <- lmtest::coefci(initial, vcov. = covariance)["primary", ] * step
    return(sprintf(
      "Without the moderator: %+.2f %s, 95%% CI [%.2f, %.2f]",
      stats::coef(initial)[["primary"]] * step,
      per_step(result$primary, display_name(result, result$primary)),
      interval[1], interval[2]
    ))
  }
  largest <- largest_difference(initial, levels(result$model_data$primary), covariance)
  sprintf("Without the moderator: largest group difference %+.2f (%s vs %s)",
          largest$estimate, largest$higher, largest$lower)
}

# The largest pairwise group difference, reported as higher vs lower group.
largest_difference <- function(model, groups, covariance = stats::vcov(model)) {
  differences <- group_differences(model, groups, covariance)
  largest <- differences[which.max(abs(differences$estimate)), ]
  if (largest$estimate < 0) {
    largest[c("higher", "lower")] <- largest[c("lower", "higher")]
    largest[c("estimate", "ci_lower", "ci_upper")] <-
      -largest[c("estimate", "ci_upper", "ci_lower")]
  }
  largest$n_comparisons <- nrow(differences)
  largest
}

#' All pairwise differences between the groups of the predictor
#'
#' Computed from the coefficients of the fitted lm model, so they take the
#' rest of the model (e.g. control variables) into account. With treatment
#' coding the first group is the reference (coefficient 0); the difference
#' between two other groups is the difference of their coefficients.
#' CIs and p-values are Bonferroni-adjusted for the number of pairs, so a
#' CI excludes 0 exactly when the adjusted p-value is below .05.
#'
#' @param model The fitted lm model, with the predictor named "primary".
#' @param groups The groups of the predictor; the first is the reference.
#' @param covariance The covariance of the coefficients; result$covariance
#'   (HC3) so the differences match the coefficient table.
#'
#' @return A data frame with one row per pair of groups.
group_differences <- function(model, groups, covariance = stats::vcov(model)) {
  coefficients <- stats::coef(model)
  pairs <- utils::combn(groups, 2)
  n_pairs <- ncol(pairs)
  critical_t <- stats::qt(1 - 0.05 / (2 * n_pairs), stats::df.residual(model))

  rows <- lapply(seq_len(n_pairs), function(i) {
    # Weights that turn the coefficients into "group 1 minus group 2".
    contrast <- stats::setNames(numeric(length(coefficients)), names(coefficients))
    first <- paste0("primary", pairs[1, i])
    second <- paste0("primary", pairs[2, i])
    if (first %in% names(contrast)) contrast[first] <- 1
    if (second %in% names(contrast)) contrast[second] <- -1
    estimate <- sum(contrast * coefficients)
    std_error <- sqrt(drop(t(contrast) %*% covariance %*% contrast))
    p_value <- 2 * stats::pt(-abs(estimate / std_error), stats::df.residual(model))
    data.frame(
      higher = pairs[1, i], lower = pairs[2, i], estimate = estimate,
      ci_lower = estimate - critical_t * std_error,
      ci_upper = estimate + critical_t * std_error,
      p_value = min(1, p_value * n_pairs)
    )
  })
  do.call(rbind, rows)
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
    return("Left out by your selection of continents.")
  }
  rows <- result$model_rows
  analysed_countries <- analysed_data[["Country name"]][rows]
  # Left out because its group (e.g. Oceania) has too few countries.
  for (variable in names(result$excluded_groups)) {
    group <- as.character(analysed_data[analysed_data[["Country name"]] == country, variable][[1]])
    if (group %in% result$excluded_groups[[variable]]) {
      return(sprintf("Not in this analysis: %s has fewer than %d countries with data.",
                     group, result$min_group_size))
    }
  }
  if (!country %in% analysed_countries) {
    variables <- c("happiness", result$primary, result$moderator, result$control_variables)
    values <- analysed_data[analysed_data[["Country name"]] == country, variables]
    # The same rule as analyse_happiness(): no NA, infinite or blank values.
    missing <- variables[vapply(values, function(value) {
      value <- value[[1]]
      if (is.numeric(value)) {
        !is.finite(value)
      } else {
        is.na(value) || trimws(as.character(value)) == ""
      }
    }, logical(1))]
    return(sprintf("Not in this analysis: missing data on %s.",
                   paste(display_name(result, missing), collapse = ", ")))
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
    # A logged predictor in its original units, e.g. GDP per person 19,600.
    scale <- log_scale(result$primary)
    sprintf("%s %s: number %d of %d countries.",
            if (is.null(scale)) display_name(result, result$primary) else scale$name,
            format_number(original_units(predictor[position], result$primary), 3),
            rank(-predictor, ties.method = "min")[position], n)
  } else {
    sprintf("%s: %s.", display_name(result, result$primary), predictor[position])
  })
  if (result$moderation) {
    moderator <- analysed_data[[result$moderator]][rows][position]
    # A logged moderator in its original units, like the predictor above:
    # Population 17,300,000 instead of its log2 value 24.1.
    scale <- log_scale(result$moderator)
    if (is.numeric(moderator)) {
      moderator <- format_number(original_units(moderator, result$moderator), 3)
    }
    sentences <- c(sentences, sprintf(
      "%s: %s.",
      if (is.null(scale)) display_name(result, result$moderator) else scale$name,
      moderator
    ))
  }
  c(sentences, sprintf(
    "%.2f points %s than the model predicts.",
    abs(residual), if (residual >= 0) "happier" else "unhappier"
  ))
}

# Curved associations ----
#' Test whether a straight line misleads about the association
#'
#' Adds the squared and cubed predictor (standardised first, so large
#' variables stay computable) to the model and tests whether that improves
#' the fit, with a Wald test (HC3 when the result is robust). The same
#' rule is used by check_assumptions() for the Methods section:
#' the line misleads when the curve fits significantly better and either
#' the line explains less than 80% of what the curve explains, or the curve
#' goes the other way than the line over more than 10% of the predictor's
#' range (5th to 95th percentile), i.e. the association goes up and down.
#' For a logged predictor the curve is on the log scale, the scale of the
#' model and the plot. With a moderator, the direction is judged from the
#' predictor's own terms.
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return NULL for a categorical predictor (or too few distinct values),
#'   otherwise a list with the F-test (f_value, df_added, df_residual,
#'   p_value), the variance explained by the line and by the curve, the
#'   share of the range where the curve goes against the line, and
#'   misleading (TRUE or FALSE).
curvature_test <- function(result) {
  model_data <- result$model_data
  primary <- model_data$primary
  # A curve is already fitted; curve_test compares it with the line.
  if (identical(result$shape, "curve")) return(NULL)
  if (!is.numeric(primary) || length(unique(primary)) < 5L) return(NULL)
  centre <- mean(primary)
  spread <- stats::sd(primary)
  model_data$curve_squared <- ((primary - centre) / spread)^2
  model_data$curve_cubed <- ((primary - centre) / spread)^3
  curved <- stats::lm(
    stats::update(stats::formula(result$model), . ~ . + curve_squared + curve_cubed),
    data = model_data
  )
  if (anyNA(stats::coef(curved)) || stats::df.residual(curved) < 1L) return(NULL)
  # Tested with the same kind of covariance as the model (HC3 when robust).
  curve_covariance <- tryCatch(model_covariance(curved, isTRUE(result$robust)),
                               error = function(error) NULL)
  if (is.null(curve_covariance)) return(NULL)
  test <- lmtest::waldtest(result$model, curved, vcov = curve_covariance, test = "F")

  # Direction of the curve along the predictor, from its own terms.
  grid <- seq(stats::quantile(primary, 0.05), stats::quantile(primary, 0.95), length.out = 50)
  z <- (grid - centre) / spread
  coefficients <- stats::coef(curved)
  steps <- diff(coefficients[["primary"]] * grid +
                  coefficients[["curve_squared"]] * z^2 + coefficients[["curve_cubed"]] * z^3)
  against <- mean(sign(steps) != sign(stats::coef(result$model)[["primary"]]))

  r_squared_curve <- summary(curved)$r.squared
  line_share <- if (r_squared_curve > 0.05) result$r_squared / r_squared_curve else NA
  p_value <- test$`Pr(>F)`[2]
  list(
    f_value = test$F[2],
    df_added = as.integer(test$Df[2]),
    df_residual = as.integer(test$Res.Df[2]),
    p_value = p_value,
    r_squared_line = result$r_squared,
    r_squared_curve = r_squared_curve,
    against = against,
    misleading = p_value < 0.05 && (is.na(line_share) || line_share < 0.8 || against > 0.1)
  )
}

# One sentence that says the straight line misleads, with the test.
describe_curvature <- function(curve, result) {
  sprintf(
    paste("The association with %s is curved%s: a curve explains %.0f%% of the differences in happiness instead of %.0f%%",
          "(F(%d, %d) = %.2f, p %s), so the straight line misses part of it. Look at the plot and the residuals."),
    display_name(result, result$primary), if (curve$against > 0.1) " and goes up and down" else "",
    100 * curve$r_squared_curve, 100 * curve$r_squared_line,
    curve$df_added, curve$df_residual, curve$f_value, format_p(curve$p_value, with_sign = TRUE)
  )
}

# Curves ----

# The predictor's name for a sentence: its label, or the original name of a
# logged predictor ("GDP per person"), whose values are shown unlogged.
primary_label <- function(result) {
  scale <- log_scale(result$primary)
  if (is.null(scale)) display_name(result, result$primary) else scale$name
}

#' Change in happiness along a curve, from a low to a high value
#'
#' A curve has no single slope, so it is summarised by the predicted
#' difference between the 25th and the 75th percentile of the predictor,
#' with the control variables held at their mean or reference group. The
#' CI and p-value use the result's covariance (HC3 when robust). For a
#' straight line this is the slope times the distance between the two. For
#' a curve that goes up and down it can be near 0, so the tile also says
#' how much more the curve explains than a straight line.
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return A list with the low and high value (model scale), the estimate,
#'   its CI and p-value.
curve_difference <- function(result) {
  model <- result$model
  values <- unname(stats::quantile(result$model_data$primary, c(0.25, 0.75)))
  grid <- hold_controls(data.frame(primary = values), result$model_data,
                        names(result$control_mapping))
  predictors <- stats::delete.response(stats::terms(model))
  frame <- stats::model.frame(predictors, grid, xlev = model$xlevels)
  design <- stats::model.matrix(predictors, frame, contrasts.arg = model$contrasts)
  contrast <- design[2, ] - design[1, ]
  covariance <- if (is.null(result$covariance)) stats::vcov(model) else result$covariance
  estimate <- sum(contrast * stats::coef(model))
  std_error <- sqrt(drop(t(contrast) %*% covariance %*% contrast))
  critical_t <- stats::qt(0.975, stats::df.residual(model))
  list(
    low = values[1], high = values[2], estimate = estimate,
    ci_lower = estimate - critical_t * std_error,
    ci_upper = estimate + critical_t * std_error,
    p_value = 2 * stats::pt(-abs(estimate / std_error), stats::df.residual(model))
  )
}

# What low and high mean, with the countries nearest to them, e.g. "Low =
# 25th percentile: 58 (e.g. Chile); high = 75th: 68 (e.g. Kenya)".
curve_example <- function(result, difference, countries) {
  values <- result$model_data$primary
  nearest <- function(value) countries[which.min(abs(values - value))]
  sprintf(
    "Low = 25th percentile: %s (e.g. %s); high = 75th: %s (e.g. %s)",
    format_number(original_units(difference$low, result$primary), 3), nearest(difference$low),
    format_number(original_units(difference$high, result$primary), 3), nearest(difference$high)
  )
}

# The sentences about a curve: the change from low to high, and how much
# better the curve fits than the straight line.
describe_curve <- function(result) {
  difference <- curve_difference(result)
  test <- result$curve_test
  c(
    sprintf(
      "Along the curve, countries with a high %s (75th percentile, %s) have a %.2f points %s happiness score than countries with a low one (25th percentile, %s) (95%% CI [%.2f, %.2f], p %s).",
      primary_label(result), format_number(original_units(difference$high, result$primary), 3),
      abs(difference$estimate), if (difference$estimate >= 0) "higher" else "lower",
      format_number(original_units(difference$low, result$primary), 3),
      difference$ci_lower, difference$ci_upper, format_p(difference$p_value, with_sign = TRUE)
    ),
    sprintf(
      "The curve explains %.0f%% of the differences in happiness, a straight line %.0f%%; the curve %s (F(%d, %d) = %.2f, p %s).",
      100 * result$r_squared, 100 * result$line_r_squared,
      if (test$p_value < 0.05) "fits significantly better" else "does not fit significantly better",
      as.integer(test$df_added), as.integer(stats::df.residual(result$model)),
      test$f_value, format_p(test$p_value, with_sign = TRUE)
    )
  )
}

# Logged predictors ----
# A slope per log unit is hard to read ("+0.45 per unit of log2(GDP per
# person)"). The model stays on the log scale; only what the reader sees is
# translated: per 10 times more, in the original units.

#' The log base and the original name of a logged variable
#'
#' "log2(...)" variables come from prepare_data.R; "Logged GDP per capita"
#' is the natural log of GDP per capita in the World Happiness Report.
#'
#' @param variable A variable name.
#'
#' @return A list with the base and the name in original units, or NULL
#'   when the variable is not logged.
log_scale <- function(variable) {
  if (identical(variable, "Logged GDP per capita")) {
    return(list(base = exp(1), name = "GDP per capita"))
  }
  if (length(variable) == 1 && grepl("^log2\\(.*\\)$", variable)) {
    return(list(base = 2, name = sub("^log2\\((.*)\\)$", "\\1", variable)))
  }
  NULL
}

# How many log units "10 times more" is. 1 for a variable that is not
# logged, so multiplying a per-unit effect by it changes nothing.
ten_times <- function(variable) {
  scale <- log_scale(variable)
  if (is.null(scale)) 1 else log(10, base = scale$base)
}

# "per unit of Social support", or "per 10 times more GDP per person".
# name: how to show a variable that is not logged (e.g. its label).
per_step <- function(variable, name = variable) {
  scale <- log_scale(variable)
  if (is.null(scale)) paste("per unit of", name) else paste("per 10 times more", scale$name)
}

# Values of a logged variable back in their original units.
original_units <- function(values, variable) {
  scale <- log_scale(variable)
  if (is.null(scale)) values else scale$base^values
}

# The factor that turns an interaction coefficient into the step that
# describe_interaction() names: per 10 times more for every logged numeric
# variable in the interaction.
interaction_step <- function(result) {
  step <- 1
  if (is.numeric(result$model_data$primary)) step <- step * ten_times(result$primary)
  if (is.numeric(result$model_data$moderator)) step <- step * ten_times(result$moderator)
  step
}

# An example of the slope of a logged predictor with two real countries:
# the countries nearest the 25th and 75th percentile, e.g. "E.g. GDP per
# person from 1,900 (India) to 19,600 (Chile): +1.52 points". NULL when the
# predictor is not logged.
log_example <- function(result, countries) {
  scale <- log_scale(result$primary)
  if (is.null(scale)) return(NULL)
  values <- result$model_data$primary
  low <- which.min(abs(values - stats::quantile(values, 0.25)))
  high <- which.min(abs(values - stats::quantile(values, 0.75)))
  slope <- result$primary_coefficients$estimate
  sprintf(
    "E.g. %s from %s (%s) to %s (%s): %+.2f points",
    scale$name,
    format_number(scale$base^values[low]), countries[low],
    format_number(scale$base^values[high]), countries[high],
    slope * (values[high] - values[low])
  )
}

# The slope of a numeric predictor per SD, with its CI. The SD is the one
# across all countries with data (variable_info$sd), so the same predictor
# gets the same step in every analysis. For a logged predictor the SD is on
# the log scale; its size is shown as a factor ("8.4 times more GDP per
# person"). NULL without an SD or for a categorical predictor.
per_sd_effect <- function(result, sd) {
  if (is.null(sd) || length(sd) != 1 || !is.finite(sd) ||
      !is.numeric(result$model_data$primary)) {
    return(NULL)
  }
  slope <- result$primary_coefficients
  scale <- log_scale(result$primary)
  list(
    size = if (is.null(scale)) {
      paste("1 SD =", format_number(sd))
    } else {
      sprintf("1 SD = %s times more %s", format_number(scale$base^sd), scale$name)
    },
    estimate = slope$estimate * sd,
    ci_lower = slope$ci_lower * sd,
    ci_upper = slope$ci_upper * sd
  )
}

# Numbers rounded to a few significant digits, with thousands separators.
# Formatted one by one, so 0.1 and 10 do not become "0.1" and "10.0".
format_number <- function(values, digits = 2) {
  vapply(values, function(value) {
    format(signif(value, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

# Helpers ----

#' The name to show for variables
#'
#' Their labels when the result has labels (result$labels, a named vector
#' that the app sets from variable_info), otherwise the variable names.
#' Calculations always use the variable names.
#'
#' @param result The list returned by analyse_happiness().
#' @param variables Variable names; NULL gives NULL.
#'
#' @return A character vector of the same length.
display_name <- function(result, variables) {
  if (is.null(variables)) return(NULL)
  labels <- result$labels
  vapply(variables, function(variable) {
    if (!is.null(labels) && variable %in% names(labels)) labels[[variable]] else variable
  }, character(1), USE.NAMES = FALSE)
}

# Replace the internal names in a model term, e.g.
# "primary:moderatorEurope" -> "Social support x continent [Europe]".
# Control variables already carry their real names; a group of a
# categorical control gets brackets too: "Official languageEnglish" ->
# "Official language [English]".
label_term <- function(term, primary, moderator, controls = character(0),
                       control_labels = controls) {
  if (term == "(Intercept)") return("Intercept")
  # Control variables are never part of an interaction, and their names can
  # contain ":" themselves, e.g. "Population: Labor force participation (%)".
  # The longest name first, so "CPI Change (%)" is not read as "CPI".
  for (i in order(-nchar(controls))) {
    control <- controls[i]
    if (term == control) return(control_labels[i])
    if (startsWith(term, control)) {
      return(paste0(control_labels[i], " [", substring(term, nchar(control) + 1), "]"))
    }
  }
  labels <- c(moderator = moderator, primary = primary)
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  parts <- vapply(parts, function(part) {
    # The spline columns of a curve: "ns(primary, df = 3)2".
    if (startsWith(part, "ns(primary")) {
      piece <- sub("^ns\\(primary, df = [0-9]+\\)", "", part)
      return(paste0(primary, if (piece == "") " (curve)" else paste0(" (curve, part ", piece, ")")))
    }
    for (internal in names(labels)) {
      if (part == internal) return(labels[[internal]])
      if (startsWith(part, internal)) {
        return(paste0(labels[[internal]], " [", substring(part, nchar(internal) + 1), "]"))
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
