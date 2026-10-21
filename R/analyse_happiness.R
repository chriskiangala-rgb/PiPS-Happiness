# Define happiness regression with an optional moderator ----

analyse_happiness <- function(
    primary_predictor,
    moderator = "",
    data = combined_data,
    predictors = available_predictors
) {
  # Validate selected predictors and outcome ----
  
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
  
  # Prepare model data and exclude missing or nonfinite values ----
  
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
  
  # Set categorical reference groups and check variable variation ----
  
  # Nonnumeric predictors become factors. Numeric-coded categories must
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
  
  # Fit the regression model and check estimability ----
  
  # * includes both main effects AND their interaction.
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
  
  # Extract coefficients, p-values, and confidence intervals ----
  
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
  
  # Separate primary and interaction coefficients ----
  
  # Separate main-effect coefficients from interaction coefficients.
  is_interaction <- grepl(":", coefficient_table$term, fixed = TRUE)
  primary_coefficients <- coefficient_table[
    startsWith(coefficient_table$term, "primary") & !is_interaction,
    , drop = FALSE
  ]
  interaction_coefficients <- coefficient_table[
    is_interaction, , drop = FALSE
  ]
  
  # Test the overall interaction against the additive model ----
  
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
  
  # Identify reference categories and return analysis results ----
  
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
