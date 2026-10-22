# Plot the result of analyse_happiness() ----

#' Plot happiness against the primary predictor, with the fitted model
#'
#' Shows every country as a point and the predictions of the fitted model
#' with 95% confidence bands. With a moderator, there is one line per group
#' (categorical moderator) or one line at a low, average and high value
#' (numeric moderator), so the plot shows exactly the model that was tested.
#' Control variables are held at their mean (numeric) or reference group
#' (categorical) in the lines; the points show the countries as they are.
#' A logged predictor (see log_scale()) gets an axis in its original units
#' on a log scale, so the line stays straight.
#'
#' @param result The list returned by analyse_happiness().
#' @param countries Optional country names, in the same row order as the data
#'   that was analysed. When given, each point gets a hover text (shown when
#'   the plot is made interactive with plotly::ggplotly(tooltip = "text")).
#' @param highlight Optional country name to mark with a circle and its
#'   name. Needs countries.
#'
#' @return A ggplot object.
plot_happiness <- function(result, countries = NULL, highlight = "") {
  model_data <- result$model_data
  predictions <- build_prediction_grid(model_data, result$moderation,
                                       names(result$control_mapping))
  predictions <- cbind(predictions, predict_with_ci(result, predictions))

  primary_is_numeric <- is.numeric(model_data$primary)
  scale <- if (primary_is_numeric) log_scale(result$primary)
  primary_name <- if (is.null(scale)) result$primary else scale$name

  # Colour the countries by group only when the moderator is categorical.
  colour_points <- result$moderation && !is.numeric(model_data$moderator)
  if (colour_points) model_data$group <- model_data$moderator

  # model_rows links each model observation to its original input-data row.
  has_tooltip <- !is.null(countries)
  if (has_tooltip) {
    model_rows <- result$model_rows

    if (is.null(model_rows) ||
        length(model_rows) != nrow(model_data) ||
        anyNA(model_rows) ||
        any(model_rows < 1 | model_rows > length(countries)) ||
        any(model_rows != floor(model_rows))) {
      stop("result$model_rows must identify valid rows in countries.")
    }

    country <- countries[model_rows]
    primary_value <- if (primary_is_numeric) {
      format_number(original_units(model_data$primary, result$primary), 3)
    } else {
      as.character(model_data$primary)
    }
    model_data$tooltip <- paste0(
      "<b>", country, "</b><br>Happiness: ", sprintf("%.2f", model_data$happiness),
      "<br>", primary_name, ": ", primary_value
    )
  }

  point_mapping <- list(x = quote(primary), y = quote(happiness))
  if (colour_points) point_mapping$colour <- quote(group)
  if (has_tooltip) point_mapping$text <- quote(tooltip)

  if (primary_is_numeric) {
    point_layer <- ggplot2::geom_point
    point_settings <- list(alpha = 0.6, size = 2)
  } else {
    point_layer <- ggplot2::geom_jitter
    point_settings <- list(alpha = 0.4, width = 0.15, height = 0)
  }
  if (!colour_points) point_settings$colour <- "grey50"

  points <- muffle_text_aesthetic(
    do.call(point_layer, c(
      list(data = model_data, mapping = do.call(ggplot2::aes, point_mapping)),
      point_settings
    ))
  )
  plot <- ggplot2::ggplot(predictions, ggplot2::aes(primary, fit)) + points

  if (primary_is_numeric) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lwr, ymax = upr, fill = group),
        alpha = 0.15, colour = NA
      ) +
      ggplot2::geom_line(ggplot2::aes(colour = group), linewidth = 1)
  } else {
    plot <- plot +
      ggplot2::geom_pointrange(
        ggplot2::aes(ymin = lwr, ymax = upr, colour = group),
        position = ggplot2::position_dodge(width = 0.5)
      ) +
      ggplot2::scale_x_discrete(labels = function(labels) gsub(" ", "\n", labels))
  }

  # Mark the highlighted country on top of everything else.
  if (has_tooltip && highlight %in% country) {
    marked <- model_data[country == highlight, , drop = FALSE]
    plot <- plot +
      muffle_text_aesthetic(ggplot2::geom_point(
        ggplot2::aes(primary, happiness, text = tooltip),
        data = marked, inherit.aes = FALSE,
        shape = 21, size = 4.5, stroke = 1.5, colour = "#1A1A1A", fill = NA
      )) +
      ggplot2::geom_text(
        ggplot2::aes(primary, happiness),
        data = marked, inherit.aes = FALSE,
        label = highlight, nudge_y = 0.4, size = 4
      )
  }

  if (!is.null(scale)) {
    breaks <- log_axis_breaks(range(model_data$primary), scale$base)
    plot <- plot + ggplot2::scale_x_continuous(
      breaks = log(breaks, base = scale$base), labels = format_number(breaks)
    )
  }

  # Keep the happiness scale (0-10) visible: with very small groups the
  # confidence intervals can be huge and would otherwise flatten the plot.
  plot <- plot + ggplot2::coord_cartesian(ylim = c(0, 10))

  legend_title <- if (!result$moderation) {
    NULL
  } else if (is.numeric(model_data$moderator)) {
    paste(result$moderator, "level")
  } else {
    result$moderator
  }

  plot +
    ggplot2::labs(
      x = if (is.null(scale)) result$primary else paste(scale$name, "(log scale)"),
      y = "Happiness (ladder score, 0-10)",
      colour = legend_title,
      fill = if (primary_is_numeric) legend_title,
      caption = plot_caption(result)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = if (result$moderation) "right" else "none")
}

# What the lines and bands show, and at which values the control variables
# are held. Also shown below the plot in the app, because plotly drops the
# caption of a ggplot.
plot_caption <- function(result, line_break = "\n") {
  paste0(
    result$n_countries, " countries. Lines and bands: model predictions with 95% CI",
    if (length(result$control_variables) > 0) {
      paste0(",", line_break, "with ", paste(result$control_variables, collapse = ", "),
             " held at the mean or reference group")
    },
    "."
  )
}

# Model predictions with a 95% CI from the result's covariance (HC3 when
# robust), so the bands agree with the coefficient table. predict() would
# use the normal standard errors.
predict_with_ci <- function(result, newdata) {
  model <- result$model
  predictors <- stats::delete.response(stats::terms(model))
  frame <- stats::model.frame(predictors, newdata, xlev = model$xlevels)
  design <- stats::model.matrix(predictors, frame, contrasts.arg = model$contrasts)
  covariance <- if (is.null(result$covariance)) stats::vcov(model) else result$covariance
  fit <- drop(design %*% stats::coef(model))
  std_error <- sqrt(rowSums((design %*% covariance) * design))
  critical_t <- stats::qt(0.975, stats::df.residual(model))
  data.frame(fit = fit, lwr = fit - critical_t * std_error, upr = fit + critical_t * std_error)
}

# Axis ticks for a logged predictor, in original units: the powers of 10
# within the range of the data, or 1, 2 and 5 times them when the data span
# too few powers of 10 to give at least three ticks.
log_axis_breaks <- function(log_range, base) {
  low <- base^log_range[1]
  high <- base^log_range[2]
  powers <- 10^(floor(log10(low)):ceiling(log10(high)))
  breaks <- powers[powers >= low & powers <= high]
  if (length(breaks) < 3) {
    breaks <- as.vector(outer(c(1, 2, 5), powers))
    breaks <- breaks[breaks >= low & breaks <= high]
  }
  breaks
}

# Build the values at which the model's predictions are plotted ----

build_prediction_grid <- function(model_data, moderation, controls = character(0)) {
  grid <- prediction_values(model_data, moderation)
  for (control in controls) {
    values <- model_data[[control]]
    held_at <- if (is.numeric(values)) {
      mean(values)
    } else {
      factor(levels(values)[1], levels = levels(values))
    }
    grid[[control]] <- rep(held_at, nrow(grid))
  }
  grid
}

# The predictor (and moderator) values to plot, before the control
# variables are added.
prediction_values <- function(model_data, moderation) {
  primary <- model_data$primary
  primary_values <- if (is.numeric(primary)) {
    seq(min(primary), max(primary), length.out = 100)
  } else {
    factor(levels(primary), levels = levels(primary))
  }

  if (!moderation) {
    return(data.frame(primary = primary_values, group = "All countries"))
  }

  moderator <- model_data$moderator

  # Numeric moderator: show the slope at one SD below the mean, the mean,
  # and one SD above the mean.
  if (is.numeric(moderator)) {
    moderator_values <- mean(moderator) + c(-1, 0, 1) * sd(moderator)
    group_labels <- c("Low (-1 SD)", "Average", "High (+1 SD)")
    grid <- expand.grid(primary = primary_values, group = group_labels,
                        stringsAsFactors = FALSE)
    grid$moderator <- moderator_values[match(grid$group, group_labels)]
    grid$group <- factor(grid$group, levels = group_labels)
    return(grid)
  }

  grid <- expand.grid(
    primary = primary_values,
    moderator = factor(levels(moderator), levels = levels(moderator))
  )
  grid$group <- grid$moderator

  # Draw each group's line only over the values observed in that group,
  # so the plot does not extrapolate beyond the data.
  if (is.numeric(primary)) {
    group_min <- tapply(primary, moderator, min)
    group_max <- tapply(primary, moderator, max)
    group_name <- as.character(grid$moderator)
    grid <- grid[grid$primary >= group_min[group_name] &
                   grid$primary <= group_max[group_name], , drop = FALSE]
  }
  grid
}

# ggplot2 does not use the "text" aesthetic itself and warns about it,
# but plotly reads it for the hover text. Evaluates a layer without that
# warning; other warnings still show.
muffle_text_aesthetic <- function(layer) {
  withCallingHandlers(
    layer,
    warning = function(warning) {
      if (grepl("unknown aesthetics: text", conditionMessage(warning))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
