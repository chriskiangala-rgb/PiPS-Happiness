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
  "The residual plot is not a ggplot" =
  inherits(plot_residuals(result, model_countries), "ggplot"),
"The Q-Q plot is not a ggplot" =
  inherits(plot_qq(result, model_countries), "ggplot"),
"Influential countries should list 5 countries" =
  nrow(influential_countries(result, model_countries)) == 5,
  "Influential countries should list 5 countries" =
    nrow(influential_countries(result, countries)) == 5,
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

# Check the app itself: change the inputs as a user would and read the outputs

app <- happiness_app(combined_data, available_predictors, predictor_groups = groups)

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

  session$setInputs(moderator = "", continents = character(0))
  stopifnot("No continents selected should give a message" = analysis_failed())
}), warning = muffle_thematic)
thematic::thematic_off()

message("All app checks passed.")
