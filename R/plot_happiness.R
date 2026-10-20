# Plot the result of analyse_happiness() ----

#' Plot happiness against the primary predictor, with the fitted model
#'
#' Shows every country as a point and the predictions of the fitted model
#' with 95% confidence bands. With a moderator, there is one line per group
#' (categorical moderator) or one line at a low, average and high value
#' (numeric moderator), so the plot shows exactly the model that was tested.
#'
#' @param result The list returned by analyse_happiness().
#'
#' @return A ggplot object.
plot_happiness <- function(result) {
  model_data <- result$model_data
  predictions <- build_prediction_grid(model_data, result$moderation)
  predictions <- cbind(
    predictions,
    predict(result$model, newdata = predictions, interval = "confidence")
  )

  # Colour the countries by group only when the moderator is categorical.
  colour_points <- result$moderation && !is.numeric(model_data$moderator)
  if (colour_points) model_data$group <- model_data$moderator

  primary_is_numeric <- is.numeric(model_data$primary)
  plot <- ggplot2::ggplot(predictions, ggplot2::aes(primary, fit))

  if (primary_is_numeric) {
    point_layer <- ggplot2::geom_point
    point_settings <- list(alpha = 0.6)
  } else {
    point_layer <- ggplot2::geom_jitter
    point_settings <- list(alpha = 0.4, width = 0.15, height = 0)
  }
  plot <- plot + if (colour_points) {
    do.call(point_layer, c(
      list(data = model_data, mapping = ggplot2::aes(primary, happiness, colour = group)),
      point_settings
    ))
  } else {
    do.call(point_layer, c(
      list(data = model_data, mapping = ggplot2::aes(primary, happiness), colour = "grey50"),
      point_settings
    ))
  }

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
      x = result$primary,
      y = "Happiness (ladder score, 0-10)",
      colour = legend_title,
      fill = if (primary_is_numeric) legend_title,
      caption = paste(
        result$n_countries, "countries. Lines and bands: model predictions with 95% CI."
      )
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = if (result$moderation) "right" else "none")
}

# Build the values at which the model's predictions are plotted ----

build_prediction_grid <- function(model_data, moderation) {
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
