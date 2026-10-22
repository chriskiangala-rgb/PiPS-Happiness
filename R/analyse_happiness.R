# Define happiness regression with an optional moderator ----
# primary_predictor: the variable whose link with happiness you want to see.
# moderator: optional variable that may change that link ("" for none).
# control_variables: optional variables to adjust for (any number).
# selected_continents: optional, two or more continents to keep; countries on
#   other continents are left out. Needs continent as predictor or moderator.
# reference_continent: optional continent the other continents are compared
#   with. Defaults to the first of selected_continents, otherwise to the first
#   continent alphabetically. Needs continent as predictor or moderator.

analyse_happiness <- function(
    primary_predictor,
    moderator = "",
    data = combined_data,
    predictors = available_predictors,
    reference_continent = "",
    control_variables = character(0),
    selected_continents = NULL
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
  
  # Fit the regression model and check estimability ----
  
  # * includes both main effects AND their interaction.
  main_terms <- if (moderator == "") {
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

  # Decide this on the internal names, before the real control names are put
  # back: a real name can contain a colon or start with "primary".
  is_interaction <- grepl(":", coefficient_table$term, fixed = TRUE)
  is_primary <- startsWith(coefficient_table$term, "primary") & !is_interaction

  # Show the real names of the control variables instead of control1, control2, ...
  # (?![0-9]) stops control1 from also matching control10, control11, ...
  for (i in seq_along(control_names)) {
    coefficient_table$term <- sub(
      paste0("^", control_names[i], "(?![0-9])"), control_variables[i],
      coefficient_table$term, perl = TRUE
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
  interaction_test <- NULL
  additive_model <- NULL

  if (moderator != "") {
    additive_formula = reformulate(
      termlabels = c("primary", "moderator", control_names),
      response = "happiness"
    )
    additive_model = lm(additive_formula, data = model_data)
    comparison = anova(additive_model, model)
    interaction_test = data.frame(
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
    control_variables = control_variables,
    control_mapping = setNames(control_variables, control_names),
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

