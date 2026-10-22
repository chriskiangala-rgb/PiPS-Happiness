# Define happiness regression with an optional moderator ----
# primary_predictor: the variable whose link with happiness you want to see.
# moderator: optional variable that may change that link ("" for none).
# control_variables: optional variables to adjust for (any number).
# selected_continents: optional, two or more continents to keep; countries on
#   other continents are left out. Needs continent as predictor or moderator.
# reference_continent: optional continent the other continents are compared
#   with. Defaults to the first of selected_continents, otherwise to the first
#   continent alphabetically. Needs continent as predictor or moderator.
# robust: HC3 standard errors, CIs and p-values (Long & Ervin, 2000), which
#   do not assume an equal spread of the residuals. FALSE gives the normal ones.
# min_group_size: groups of a categorical predictor or moderator with fewer
#   countries are left out, because they cannot get a reliable estimate of
#   their own (with HC3, a group of 2 makes every standard error unknown).
#   The result lists them in excluded_groups.
# shape: "line" (a straight line) or "curve": a natural spline with 3
#   degrees of freedom for a numeric predictor, which can bend where the
#   data bend. Not combined with a moderator: a curve per group needs too
#   many coefficients for groups of about ten countries. The result then
#   also compares the curve with the straight line (curve_test).

analyse_happiness <- function(
    primary_predictor,
    moderator = "",
    data = combined_data,
    predictors = available_predictors,
    reference_continent = "",
    control_variables = character(0),
    selected_continents = NULL,
    robust = TRUE,
    min_group_size = 5,
    shape = c("line", "curve")
) {
  shape <- match.arg(shape)
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
  if (!is.logical(robust) || length(robust) != 1L || is.na(robust)) {
    stop("robust must be TRUE or FALSE.")
  }
  if (!is.numeric(min_group_size) || length(min_group_size) != 1L ||
      is.na(min_group_size) || min_group_size < 1) {
    stop("min_group_size must be a single number of at least 1.")
  }
  if (shape == "curve" && moderator != "") {
    stop("A curve cannot be combined with a moderator: choose a straight line or no moderator.")
  }
  
  # Validate optional control variables, variable should not also be the predictor or moderator 
  if (is.null(control_variables)) control_variables <- character(0)
  if (!is.character(control_variables) || anyNA(control_variables)) {
    stop("Choose valid control variables or leave the selection empty.")
  }
  control_variables <- unique(control_variables[control_variables != ""])
  
  if (!all(control_variables %in% predictors)) {
    stop("A selected control variable is not available.")
  }
  if (any(control_variables %in% c(primary_predictor, moderator))) {
    stop("Control variables must differ from the primary predictor and moderator.")
  }
  
  selected_columns <- c("happiness", primary_predictor)
  if (moderator != "") selected_columns <- c(selected_columns, moderator)
  selected_columns <- c(selected_columns, control_variables)
  if (!all(selected_columns %in% names(data))) {
    stop("A selected column is missing from the supplied data.")
  }
  if (!is.numeric(data$happiness)) stop("Happiness must be numeric.")
  
  
  # Optionally select continents and a reference continent ----
  if (!is.character(reference_continent) ||
      length(reference_continent) != 1L ||
      is.na(reference_continent)) {
    stop("The reference continent must be a single text value.")
  }
  uses_continents <- !is.null(selected_continents) || reference_continent != ""
  if (uses_continents && !"continent" %in% c(primary_predictor, moderator)) {
    stop("Select continent as the predictor or moderator to compare continents.")
  }
  continent_options <- unique(as.character(data$continent))

  # Keep only countries from two or more selected continents.
  if (!is.null(selected_continents)) {
    if (!is.character(selected_continents) || anyNA(selected_continents)) {
      stop("Provide selected_continents as a character vector of continent names.")
    }
    selected_continents <- unique(selected_continents)
    if (length(selected_continents) < 2L) {
      stop("Select at least two different continents.")
    }
    if (!all(selected_continents %in% continent_options)) {
      stop("A selected continent is not present in the supplied data.")
    }
    if (reference_continent != "" &&
        !reference_continent %in% selected_continents) {
      stop("The reference continent must be among the selected continents.")
    }
    data <- data[data$continent %in% selected_continents, , drop = FALSE]

    # Default reference: the first selected continent.
    if (reference_continent == "") reference_continent <- selected_continents[1L]
  }

  if (reference_continent != "" && !reference_continent %in% continent_options) {
    stop("The reference continent is not present in the supplied data.")
  }
  
  # Prepare model data and exclude missing or nonfinite values ----
  
  # Internal names avoid problems with spaces in original column names.
  model_data <- data.frame(
    happiness = data$happiness,
    primary = data[[primary_predictor]]
  )
  if (moderator != "") model_data$moderator <- data[[moderator]]
  control_names <- if (length(control_variables) > 0) {
    paste0("control", seq_along(control_variables))
  } else {
    character(0)
  }
  
  for (i in seq_along(control_variables)) {
    model_data[[control_names[i]]] <- data[[control_variables[i]]]
  }

  # Real variable name of each internal name, for messages and labels.
  real_names <- c(primary = primary_predictor, setNames(control_variables, control_names))
  if (moderator != "") real_names["moderator"] <- moderator

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
  model_rows <- which(usable)
  model_data <- model_data[usable, , drop = FALSE]
  n_excluded <- nrow(data) - nrow(model_data)

  # Leave out groups that are too small for an estimate of their own ----

  # A group of 2 countries with its own slope is fitted exactly: its
  # leverage is 1 and HC3 cannot be computed for any coefficient.
  excluded_groups <- list()
  for (column in intersect(c("primary", "moderator"), names(model_data))) {
    if (is.numeric(model_data[[column]])) next
    group_sizes <- table(as.character(model_data[[column]]))
    too_small <- names(group_sizes)[group_sizes < min_group_size]
    if (length(too_small) == 0) next

    if (real_names[[column]] == "continent" && reference_continent %in% too_small) {
      stop(sprintf("The reference continent %s has fewer than %d countries with data. Choose another reference continent.",
                   reference_continent, min_group_size))
    }
    if (length(too_small) > length(group_sizes) - 2L) {
      stop(sprintf("Fewer than two groups of %s have at least %d countries with data, so there is nothing to compare.",
                   real_names[[column]], min_group_size))
    }
    keep <- !as.character(model_data[[column]]) %in% too_small
    model_data <- model_data[keep, , drop = FALSE]
    model_rows <- model_rows[keep]
    excluded_groups[[real_names[[column]]]] <- too_small
    if (real_names[[column]] == "continent" && !is.null(selected_continents)) {
      selected_continents <- setdiff(selected_continents, too_small)
    }
  }
  n_small_groups <- nrow(data) - n_excluded - nrow(model_data)
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
      stop(paste("No variation in", real_names[[column]], "among the available countries."))
    }
    if (is.factor(model_data[[column]])) {
      # Set the reference on the column that holds continent: the
      # primary predictor or the moderator.
      if (reference_continent != "" &&
          ((column == "primary" && primary_predictor == "continent") ||
           (column == "moderator" && moderator == "continent"))) {
        
        if (!all(
          c(reference_continent, selected_continents) %in%
            levels(model_data[[column]])
        )) {
          stop("Every selected continent needs usable data for this model.")
        }
        
        model_data[[column]] <- relevel(
          model_data[[column]],
          ref = reference_continent
        )
      }
      
      # Explicit treatment coding: first level is the reference category.
      contrasts(model_data[[column]]) <- contr.treatment(
        levels(model_data[[column]]), base = 1
      )
    }
  }
  if (length(unique(model_data$happiness)) < 2L) {
    stop("Happiness has no variation among the available countries.")
  }
  if (shape == "curve" && (!is.numeric(model_data$primary) ||
                           length(unique(model_data$primary)) < 5L)) {
    stop(paste("A curve needs a numeric predictor with at least 5 different values;",
               real_names[["primary"]], "is not one."))
  }
  
  # Fit the regression model and check estimability ----
  
  # * includes both main effects AND their interaction.
  # A curve is a natural spline of the predictor. ns() is found through
  # this function's environment (the formula's), also when the model
  # predicts new values later.
  ns <- splines::ns
  main_terms <- if (shape == "curve") {
    "ns(primary, df = 3)"
  } else if (moderator == "") {
    "primary"
  } else {
    "primary * moderator"
  }
  
  #include every selected control as an additive adjustment, whether or not you select a moderator.
  formula <- reformulate(
    termlabels = c(main_terms, control_names),
    response = "happiness"
  )
  model <- lm(formula, data = model_data)
  
  if (model$rank < ncol(model.matrix(model))) {
    stop("The model cannot estimate all coefficients independently. Choose different variables or groups.")
  }
  if (df.residual(model) < 1L) {
    stop("Too few countries for this model, possibly because there are too many categories or interactions.")
  }
  
  # Extract coefficients, p-values, and confidence intervals ----

  # HC3 does not assume an equal spread of the residuals; it cannot be
  # computed when a country has a coefficient of its own (leverage 1).
  covariance <- model_covariance(model, robust)
  model_summary <- summary(model)
  tests <- lmtest::coeftest(model, vcov. = covariance)
  intervals <- lmtest::coefci(model, vcov. = covariance)
  coefficient_table <- data.frame(
    term = rownames(tests),
    estimate = tests[, 1],
    std_error = tests[, 2],
    t_value = tests[, 3],
    p_value = tests[, 4],
    ci_lower = intervals[, 1],
    ci_upper = intervals[, 2],
    row.names = NULL
  )
  
  # Separate primary and interaction coefficients ----

  # Decide this on the internal names, before the real control names are put
  # back: a real name can contain a colon or start with "primary".
  is_interaction <- grepl(":", coefficient_table$term, fixed = TRUE)
  is_primary <- (startsWith(coefficient_table$term, "primary") |
                   startsWith(coefficient_table$term, "ns(primary")) & !is_interaction

  # Show the real names of the control variables instead of control1, control2, ...
  # Map each coefficient to its formula term instead of guessing from its name.
  design_matrix <- model.matrix(model)
  coefficient_assignment <- attr(design_matrix, "assign")
  formula_terms <- attr(terms(model), "term.labels")

  # The full-rank check above ensures that no coefficients were dropped,
  # so coefficient_table rows follow the model-matrix column order.
  original_terms <- coefficient_table$term

  for (i in seq_along(control_names)) {
    term_index <- match(control_names[i], formula_terms)
    coefficient_rows <- which(coefficient_assignment == term_index)

    # Preserve factor-level suffixes, including those beginning with digits.
    suffixes <- substring(
      original_terms[coefficient_rows],
      nchar(control_names[i]) + 1L
    )

    coefficient_table$term[coefficient_rows] <- paste0(
      control_variables[i],
      suffixes
    )
  }

  primary_coefficients <- coefficient_table[is_primary, , drop = FALSE]
  interaction_coefficients <- coefficient_table[
    is_interaction, , drop = FALSE
  ]
  
  # Test the overall interaction against the additive model ----
  
  # Overall moderation test: compare the additive and interaction models
  # using exactly the same countries. This tests all interaction terms jointly,
  # which is important when a categorical variable has more than two levels.
  # The Wald test uses the same covariance as the coefficients.
  interaction_test <- NULL
  additive_model <- NULL

  if (moderator != "") {
    additive_formula = reformulate(
      termlabels = c("primary", "moderator", control_names),
      response = "happiness"
    )
    additive_model = lm(additive_formula, data = model_data)
    comparison = lmtest::waldtest(additive_model, model, vcov = covariance, test = "F")
    interaction_test = data.frame(
      df_added = comparison$Df[2],
      f_value = comparison$F[2],
      p_value = comparison$`Pr(>F)`[2]
    )
  }
  
  # Compare the curve with the straight line ----

  # Both on the same countries and with the same controls; the Wald test
  # uses the curve's covariance (HC3 when robust). R squared of the line
  # for the comparison in the summary.
  curve_test <- NULL
  line_r_squared <- NULL
  if (shape == "curve") {
    line_model <- lm(reformulate(c("primary", control_names), response = "happiness"),
                     data = model_data)
    curve_test <- curve_wald_test(model, covariance, model_data$primary)
    line_r_squared <- summary(line_model)$r.squared
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
    control_variables = control_variables,
    control_mapping = setNames(control_variables, control_names),
    n_countries = nrow(model_data),
    n_excluded = n_excluded,
    n_small_groups = n_small_groups,
    excluded_groups = excluded_groups,
    min_group_size = min_group_size,
    primary_coefficients = primary_coefficients,
    interaction_coefficients = interaction_coefficients,
    interaction_test = interaction_test,
    all_coefficients = coefficient_table,
    reference_categories = reference_categories,
    r_squared = model_summary$r.squared,
    shape = shape,
    curve_test = curve_test,
    line_r_squared = line_r_squared,
    robust = robust,
    covariance = covariance,
    model = model,
    additive_model = additive_model,
    model_data = model_data,
    model_rows = model_rows
  )
}

# The covariance of the coefficients: HC3 when robust, otherwise the normal
# one. Stops with a clear message when HC3 cannot be computed.
model_covariance <- function(model, robust = TRUE) {
  if (!robust) return(stats::vcov(model))
  covariance <- sandwich::vcovHC(model, type = "HC3")
  if (any(!is.finite(diag(covariance)))) {
    stop("Robust standard errors cannot be computed: a country has a coefficient of its own. Choose fewer groups or variables.")
  }
  covariance
}

# Wald test of a curve against the straight line. The line is a special
# case of the natural spline (the spline can draw any straight line), but
# not by name, so lmtest::waldtest() cannot see that the models are nested.
# The predictor is an exact combination a of the spline columns (plus the
# intercept); the line is the curve whose spline coefficients are a
# multiple of a. The test asks whether the spline coefficients differ from
# that, in the df - 1 directions orthogonal to a.
curve_wald_test <- function(model, covariance, primary) {
  design <- stats::model.matrix(model)
  spline_columns <- grep("^ns\\(primary", colnames(design))
  a <- stats::lm.fit(cbind(1, design[, spline_columns, drop = FALSE]), primary)$coefficients[-1]
  directions <- qr.Q(qr(matrix(a)), complete = TRUE)[, -1, drop = FALSE]
  restrictions <- matrix(0, ncol(directions), ncol(design))
  restrictions[, spline_columns] <- t(directions)
  difference <- restrictions %*% stats::coef(model)
  wald <- drop(t(difference) %*% solve(restrictions %*% covariance %*% t(restrictions)) %*% difference)
  df_added <- nrow(restrictions)
  f_value <- wald / df_added
  data.frame(
    df_added = df_added,
    f_value = f_value,
    p_value = stats::pf(f_value, df_added, stats::df.residual(model), lower.tail = FALSE)
  )
}
