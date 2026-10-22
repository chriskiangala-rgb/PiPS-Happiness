# Run from the project root: source("tests/app_checks.R")
for (file in list.files("R", recursive = TRUE, full.names = TRUE)) source(file)

# Checks for the Shiny app: its helpers, every output it builds from a
# result, and the app itself without a browser (shiny::testServer).

all_continents <- levels(combined_data$continent)
groups <- group_predictors(available_predictors)

# Check the menus

stopifnot(
  "Every predictor must be in exactly one menu group" =
    setequal(unlist(groups), available_predictors) && !anyDuplicated(unlist(groups)),
  "The moderator menu must start with None" =
    identical(moderator_choices(groups, "Social support")[[1]], ""),
  "The moderator menu must leave out the predictor" =
    !"Social support" %in% unlist(moderator_choices(groups, "Social support")),
  "The control menu must leave out the predictor and moderator" =
    !any(c("Social support", "continent") %in%
           unlist(control_choices(groups, c("Social support", "continent"))))
)

bad_groups <- try(
  happiness_app(combined_data, available_predictors, predictor_groups = list(A = "Social support")),
  silent = TRUE
)
stopifnot("Incomplete predictor_groups should cause an error" = inherits(bad_groups, "try-error"))

# Check that impossible selections get a message instead of an error

europe <- combined_data[combined_data$continent == "Europe", ]

stopifnot(
  "No continents selected should give a message" =
    !is.null(selection_problem(combined_data[0, ], "Social support")),
  "One continent while comparing continents should give a message" =
    !is.null(selection_problem(europe, "continent", compares_continents = TRUE)),
  "A variable without variation should give a message" =
    !is.null(selection_problem(europe, c("Social support", "continent"))),
  "A possible selection should give no message" =
    is.null(selection_problem(combined_data, "Social support"))
)

# Check every output the app builds from a result

result <- analyse_happiness("Social support", "continent",
                            control_variables = "Logged GDP per capita")
countries <- combined_data[["Country name"]]
model_countries <- countries[result$model_rows]

stopifnot(
  "The plot is not a ggplot" = inherits(plot_happiness(result, countries, "Finland"), "ggplot"),
  "The interactive plot is not a plotly plot" =
    inherits(interactive_plot(plot_happiness(result, countries)), "plotly"),
  "The residual plot is not a ggplot" =
    inherits(plot_residuals(result, model_countries), "ggplot"),
  "The Q-Q plot is not a ggplot" =
    inherits(plot_qq(result, model_countries), "ggplot"),
  "Influential countries should list 5 countries" =
    nrow(influential_countries(result, model_countries)) == 5,
  "VIF should have one row per variable" = nrow(variance_inflation(result)) == 3,
  "The coefficient table is empty" = nrow(format_coefficients(result)) > 0,
  "The summary is empty" = length(describe_result(result)) > 0,
  "The country profile is empty" =
    length(country_profile("Finland", result, combined_data, combined_data)) > 0
)

for (layer in c("happiness", "predictor", "residual")) {
  stopifnot(
    "A map layer is not a plotly map" =
      inherits(map_happiness(result, combined_data, layer, highlight = "Finland"), "plotly"),
    "A map note is empty" = length(map_note(result, combined_data, layer)) > 0
  )
}

# With some continents left out, the map note should name them as grey

three_continents <- combined_data[combined_data$continent %in% c("Africa", "Asia", "Europe"), ]
subset_result <- analyse_happiness("Social support", data = three_continents)
subset_note <- map_note(subset_result, three_continents, "happiness")

stopifnot(
  "The map note should name the continents that are not selected" =
    any(grepl("not selected (North America, Oceania, South America)", subset_note, fixed = TRUE)),
  "With all continents, the map note should not mention unselected continents" =
    !any(grepl("not selected", map_note(result, combined_data, "happiness")))
)

# Check Moran's I on made-up data: values that rise from south to north are
# clustered (I clearly positive); the same values shuffled are not

set.seed(14)
latitude <- runif(100, -60, 70)
longitude <- runif(100, -180, 180)
clustered <- morans_i(latitude, latitude, longitude)
shuffled <- morans_i(sample(latitude), latitude, longitude)

stopifnot(
  "Moran's I should find clustered values" = clustered$p_value < .001,
  "Moran's I should not find clustering in shuffled values" = shuffled$p_value > .01,
  "Moran's I should need at least 3 countries" =
    inherits(try(morans_i(1:2, 1:2, 1:2), silent = TRUE), "try-error")
)

# Check logged predictors: shown per 10 times more, in original units

logged <- analyse_happiness("log2(GDP per person)", control_variables = character(0))
logged_effect <- key_effect(logged, combined_data[["Country name"]][logged$model_rows])
slope <- logged$primary_coefficients$estimate

stopifnot(
  "log2 and the WHR's natural log should be recognised" =
    log_scale("log2(GDP per person)")$base == 2 &&
    log_scale("log2(GDP per person)")$name == "GDP per person" &&
    log_scale("Logged GDP per capita")$base == exp(1),
  "Other variables are not logged" = is.null(log_scale("Social support")),
  "10 times more is log2(10) log2 units" = isTRUE(all.equal(ten_times("log2(CPI)"), log2(10))),
  "The tile should give the effect per 10 times more" =
    logged_effect$value == sprintf("%+.2f", slope * log2(10)) &&
    grepl("per 10 times more GDP per person", logged_effect$title),
  "The tile should give an example with two countries" =
    grepl("^E.g. GDP per person from .* \\(.*\\) to .* \\(.*\\): [+-]", logged_effect$example),
  "A predictor that is not logged has no example" =
    is.null(key_effect(result, model_countries)$example),
  "The summary should use the same wording" =
    any(grepl("10 times more GDP per person", describe_result(logged))),
  "The plot axis should be in original units" =
    grepl("GDP per person (log scale)", plot_happiness(logged)$labels$x, fixed = TRUE),
  "Axis ticks should be powers of 10 when the data span enough of them" =
    identical(log_axis_breaks(log2(c(500, 120000)), 2), c(1e3, 1e4, 1e5)),
  "Axis ticks should add 2 and 5 times when they do not" =
    identical(log_axis_breaks(log2(c(90, 900)), 2), c(100, 200, 500)),
  "Numbers should be formatted one by one" =
    identical(format_number(c(0.1, 10, 19634)), c("0.1", "10", "20,000"))
)

# Check the test for curved associations

set.seed(2)
curved_data <- combined_data
curved_data$wave <- sin(seq(0, 3 * pi, length.out = nrow(curved_data)))
curved_data$happiness <- 5 + 2 * curved_data$wave^2 + rnorm(nrow(curved_data), sd = 0.2)
curved_result <- analyse_happiness("wave", data = curved_data, predictors = "wave")

stopifnot(
  "A clearly curved association should be flagged" = curvature_test(curved_result)$misleading,
  "The summary should mention the curve" =
    any(grepl("is curved", describe_result(curved_result))),
  "A categorical predictor gets no curve test" =
    is.null(curvature_test(analyse_happiness("continent")))
)

# Check the app itself: change the inputs as a user would and read the outputs

# Check the variable list and the effect per SD

social_sd <- variable_info$sd[variable_info$variable == "Social support"]
gdp_sd <- variable_info$sd[variable_info$variable == "log2(GDP per person)"]
social <- analyse_happiness("Social support")
labelled_menu <- menu_options(predictor_menu(groups), variable_info)

stopifnot(
  "variable_info should have one row per predictor" =
    setequal(variable_info$variable, available_predictors),
  "The SD should be the one across all countries" =
    isTRUE(all.equal(social_sd, sd(combined_data[["Social support"]], na.rm = TRUE))),
  "The effect per SD should be the slope times the SD" =
    grepl(sprintf("%+.2f", social$primary_coefficients$estimate * social_sd),
          key_effect(social, sd = social_sd)$per_sd, fixed = TRUE),
  "A logged predictor's SD should be shown as a factor" =
    grepl(sprintf("%s times more GDP per person", format_number(2^gdp_sd)),
          key_effect(logged, sd = gdp_sd)$per_sd, fixed = TRUE),
  "The summary should give the effect per SD" =
    any(grepl("Per standard deviation", describe_result(logged, gdp_sd))),
  "No SD, no effect per SD" = is.null(key_effect(logged)$per_sd),
  "The menu should keep the variable names as values" =
    setequal(unlist(labelled_menu$choices, use.names = FALSE), available_predictors),
  "The menu should show the labels" =
    any(grepl("Labour force participation (%)", names(unlist(labelled_menu$choices)), fixed = TRUE)),
  "Every menu entry should have a unit" =
    length(labelled_menu$options$subtext) == length(available_predictors) &&
    all(labelled_menu$options$subtext != "")
)

# Check that the labels replace the variable names in the text

labelled <- analyse_happiness("Population: Labor force participation (%)", "continent",
                              control_variables = "Agricultural Land( %)")
labelled$labels <- stats::setNames(variable_info$label, variable_info$variable)
labelled_text <- c(
  unlist(key_effect(labelled)), describe_result(labelled),
  format_coefficients(labelled)$Term, variance_inflation(labelled)$Variable,
  plot_happiness(labelled)$labels$x, plot_caption(labelled)
)

stopifnot(
  "Labels should replace the variable names" =
    !any(grepl("Labor force|Agricultural Land\\(", labelled_text)) &&
    any(grepl("Labour force participation (%)", labelled_text, fixed = TRUE)) &&
    any(grepl("Agricultural land (%)", labelled_text, fixed = TRUE)),
  "Without labels the variable names are shown" =
    identical(display_name(result, "Social support"), "Social support"),
  "A logged predictor keeps its per 10 times more wording" =
    grepl("per 10 times more GDP per person",
          key_effect(modifyList(logged, list(labels = labelled$labels)))$title, fixed = TRUE)
)

app <- happiness_app(combined_data, available_predictors, predictor_groups = groups,
                     predictor_notes = predictor_notes, variable_info = variable_info)

# Without a browser, thematic (the app's plot colours) cannot read the page
# colours and warns; only those warnings are muffled.
muffle_thematic <- function(warning) {
  if (grepl("thematic|Auto-theming", conditionMessage(warning))) invokeRestart("muffleWarning")
}

withCallingHandlers(shiny::testServer(app, {
  session$setInputs(primary = "Social support", moderator = "", controls = character(0),
                    continents = all_continents, reference_continent = "",
                    map_layer = "happiness", highlight_country = "")
  stopifnot("The app should analyse all countries" =
              output$n_countries == as.character(result()$n_countries))

  session$setInputs(moderator = "continent", reference_continent = "Europe")
  stopifnot("The app should compare continents with the chosen reference" =
              result()$reference_categories$moderator == "Europe")

  session$setInputs(continents = "Europe")
  stopifnot("One continent while comparing continents should give a message" =
              analysis_failed())

  session$setInputs(moderator = "", continents = all_continents, primary = "Minimum wage")
  stopifnot("A variable with a note should show it" =
              grepl("Minimum wage", output$variable_notes$html))

  session$setInputs(primary = "log2(GDP per person)")
  stopifnot("The tile should show the example for a logged predictor" =
              grepl("^E.g. GDP per person", output$effect_baseline),
            "The tile should show the effect per SD" =
              grepl("^Per SD \\(1 SD = .* times more GDP per person\\)", output$effect_sd))

  session$setInputs(continents = character(0))
  stopifnot("No continents selected should give a message" = analysis_failed())
}), warning = muffle_thematic)
thematic::thematic_off()

message("All app checks passed.")
