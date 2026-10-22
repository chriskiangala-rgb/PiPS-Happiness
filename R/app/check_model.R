# Check the assumptions of the model from analyse_happiness() ----

#' Plot the residuals against the fitted values
#'
#' A curve in the smoothed line means the association is not straight; a
#' funnel shape means the spread of happiness differs between countries
#' (heteroskedasticity).
#'
#' @param result The list returned by analyse_happiness().
#' @param countries Country names in the row order of the analysed data,
#'   used for the hover text.
#'
#' @return A ggplot object.
plot_residuals <- function(result, countries) {
  checks <- data.frame(
    fitted = stats::fitted(result$model),
    residual = stats::residuals(result$model)
  )
  checks$tooltip <- paste0(
    "<b>", countries[as.integer(rownames(result$model_data))], "</b>",
    "<br>Predicted: ", sprintf("%.2f", checks$fitted),
    "<br>Residual: ", sprintf("%+.2f", checks$residual)
  )

  plot <- ggplot2::ggplot(checks, ggplot2::aes(fitted, residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    muffle_text_aesthetic(
      ggplot2::geom_point(ggplot2::aes(text = tooltip), colour = "grey50", alpha = 0.6, size = 2)
    )
  # A smoothed line needs enough countries to be meaningful.
  if (nrow(checks) >= 10L) {
    plot <- plot + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE)
  }
  plot +
    ggplot2::labs(x = "Predicted happiness", y = "Residual (observed - predicted)") +
    ggplot2::theme_minimal(base_size = 13)
}

#' QQ plot of the standardized residuals
#'
#' Points close to the diagonal mean the residuals are roughly normal. With
#' around 130 countries small deviations matter little.
#'
#' @inheritParams plot_residuals
#'
#' @return A ggplot object.
plot_qq <- function(result, countries) {
  residual <- stats::rstandard(result$model)
  order <- order(residual)
  checks <- data.frame(
    theoretical = stats::qnorm(stats::ppoints(length(residual))),
    observed = residual[order],
    country = countries[as.integer(rownames(result$model_data))][order]
  )
  checks$tooltip <- paste0(
    "<b>", checks$country, "</b><br>Standardized residual: ", sprintf("%+.2f", checks$observed)
  )

  ggplot2::ggplot(checks, ggplot2::aes(theoretical, observed)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey60") +
    muffle_text_aesthetic(
      ggplot2::geom_point(ggplot2::aes(text = tooltip), colour = "grey50", alpha = 0.6, size = 2)
    ) +
    ggplot2::labs(x = "Expected if normal", y = "Standardized residual") +
    ggplot2::theme_minimal(base_size = 13)
}

#' List the countries with the most influence on the result
#'
#' Uses Cook's distance: how much all predictions change when the country
#' is left out. Values above 4 / number of countries are a common flag.
#'
#' @inheritParams plot_residuals
#' @param n How many countries to list.
#'
#' @return A data frame with the n most influential countries.
influential_countries <- function(result, countries, n = 5) {
  distance <- stats::cooks.distance(result$model)
  threshold <- 4 / length(distance)
  top <- order(distance, decreasing = TRUE)[seq_len(min(n, length(distance)))]
  rows <- as.integer(rownames(result$model_data))[top]
  data.frame(
    Country = countries[rows],
    Happiness = sprintf("%.2f", result$model_data$happiness[top]),
    Residual = sprintf("%+.2f", stats::residuals(result$model)[top]),
    `Cook's distance` = sprintf("%.3f", distance[top]),
    Influential = ifelse(distance[top] > threshold, "Yes", "No"),
    check.names = FALSE,
    row.names = NULL
  )
}

#' Variance inflation per variable
#'
#' How much the uncertainty of each variable's estimate grows because it
#' overlaps with the other variables. Computed on the model without the
#' interaction, because an interaction term always overlaps with its own
#' main effects. For categorical variables this is the generalized VIF,
#' raised to the power 1 / number of coefficients so that the usual
#' threshold of 5 applies to every variable.
#'
#' @inheritParams plot_residuals
#'
#' @return A data frame with one row per variable, or NULL when the model
#'   has only one variable.
variance_inflation <- function(result) {
  model <- if (is.null(result$additive_model)) result$model else result$additive_model
  design <- stats::model.matrix(model)
  term_of_column <- attr(design, "assign")
  keep <- term_of_column > 0  # leave out the intercept
  term_of_column <- term_of_column[keep]
  correlation <- stats::cor(design[, keep, drop = FALSE])
  term_names <- attr(stats::terms(model), "term.labels")
  if (length(term_names) < 2L) return(NULL)

  vif <- vapply(seq_along(term_names), function(term) {
    in_term <- term_of_column == term
    generalized <- det(correlation[in_term, in_term, drop = FALSE]) *
      det(correlation[!in_term, !in_term, drop = FALSE]) / det(correlation)
    generalized^(1 / sum(in_term))
  }, numeric(1))

  data.frame(
    Variable = vapply(term_names, label_term, character(1),
                      primary = result$primary, moderator = result$moderator),
    VIF = sprintf("%.2f", vif),
    Concern = ifelse(vif > 5, "High overlap", "OK"),
    row.names = NULL
  )
}
