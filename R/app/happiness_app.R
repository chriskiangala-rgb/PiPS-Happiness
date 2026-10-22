# Shiny app to explore happiness and country characteristics ----

#' Create the happiness Shiny app
#'
#' Lets the user pick a predictor and an optional moderator, then shows key
#' numbers, an interactive plot (hover a point to see the country), a
#' plain-language summary and the coefficient table of analyse_happiness().
#'
#' Parts marked PLANNED are not built yet because they need something from
#' another file; the tag names what they need, and the commented code
#' shows where each part goes. Parts marked IDEA are undecided. All of these
#' are built or removed before the pull request.
#' "Coming soon" boxes keep the app running in the meantime.
#'
#' @param data The combined dataset (combined_data from prepare_data.R).
#' @param predictors The variable names the user can choose from.
#' @param predictor_groups Optional named list that splits predictors into
#'   groups for the menus, e.g. by data source. Every predictor must be in
#'   exactly one group. By default: numeric variables and categories.
#'
#' @return A Shiny app object; start it with shiny::runApp().
happiness_app <- function(data, predictors,
                          predictor_groups = group_by_type(data, predictors)) {
  if (!setequal(unlist(predictor_groups), predictors) ||
      anyDuplicated(unlist(predictor_groups))) {
    stop("predictor_groups must contain every predictor exactly once.")
  }
  default_predictor <- if ("Social support" %in% predictors) "Social support" else predictors[1]

  ui <- bslib::page_sidebar(
    title = "What goes together with happiness?",
    theme = app_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 320,
      bg = app_colours$sidebar,

      # PLANNED (needs a variable list in prepare_data.R):
      # Show readable names with units, e.g. "Tax revenue (% of GDP)",
      # taken from a variable list in prepare_data.R (name, label, unit,
      # transformation), so the menus, the plot axis and the "About the
      # data" tab share one source.
      shinyWidgets::pickerInput(
        "primary", "Predictor",
        choices = predictor_menu(predictor_groups), selected = default_predictor,
        options = list(`live-search` = TRUE, size = 10)
      ),
      shinyWidgets::pickerInput(
        "moderator", "Moderator (optional)",
        choices = moderator_choices(predictor_groups, default_predictor),
        options = list(`live-search` = TRUE, size = 10)
      ),
      shiny::helpText(
        "A moderator tests whether the association between the predictor",
        "and happiness differs between groups or levels of another variable."
      ),

      # PLANNED (needs PR 8 merged):
      # Control variables, at most 3, never the predictor or the
      # moderator. Needs the control_variables argument of
      # analyse_happiness() from PR 8.
      # shinyWidgets::pickerInput(
      #   "controls", "Control variables (optional, max. 3)",
      #   choices = setdiff(predictors, default_predictor),
      #   multiple = TRUE,
      #   options = list(`live-search` = TRUE, `max-options` = 3, size = 10)
      # ),
      # shiny::helpText(
      #   "Control variables are held constant: the effect shown is the",
      #   "association that remains between countries that are equal on them."
      # ),

      # PLANNED (needs PR 8 merged):
      # Compare two continents. Only shown when continent is the
      # predictor or the moderator; empty means all continents. Needs the
      # reference_continent and comparison_continent arguments from PR 8.
      # shiny::conditionalPanel(
      #   "input.primary == 'continent' || input.moderator == 'continent'",
      #   shinyWidgets::pickerInput(
      #     "reference_continent", "Reference continent",
      #     choices = c("All continents" = "", levels(data$continent))
      #   ),
      #   shinyWidgets::pickerInput(
      #     "comparison_continent", "Compared with",
      #     choices = c("All continents" = "", levels(data$continent))
      #   )
      # ),

      # Choose which continents are analysed, to check whether a result
      # holds in one part of the world. Single countries cannot be left out:
      # hand-picking which countries count biases the results (selection
      # bias) and invites dropping countries for who they are rather than
      # for a statistical reason. The tiles always show how many countries
      # were left out.
      # PLANNED (needs PR 8 merged):
      # Once the two-continent comparison above exists, it only
      # needs the reference continent, since this filter picks the continents.
      shinyWidgets::pickerInput(
        "continents", "Continents",
        choices = levels(data$continent), selected = levels(data$continent),
        multiple = TRUE,
        # The closed menu reads "All selected" when every continent is
        # selected and "None selected" when none is; otherwise it lists
        # the chosen continents.
        options = list(
          `actions-box` = TRUE,
          `select-all-text` = "Select all",
          `deselect-all-text` = "Select none",
          `selected-text-format` = paste("count >", nlevels(data$continent) - 1),
          `count-selected-text` = "All selected",
          `none-selected-text` = "None selected"
        )
      ),
      # Clicking a continent while all are selected shows only that continent,
      # instead of unticking it; after that, clicks add or remove continents.
      # Done in the browser, because an update from the server would close
      # the open menu.
      shiny::tags$script(shiny::HTML("
        $(document).on('changed.bs.select', '#continents',
          function(event, clickedIndex, isSelected, previousValue) {
            var allWereSelected = previousValue && previousValue.length === this.options.length;
            if (clickedIndex != null && !isSelected && allWereSelected) {
              $(this).selectpicker('val', [this.options[clickedIndex].value]);
            }
          });
      ")),

      # Highlight one country. Only changes what is shown, not the analysis.
      shinyWidgets::pickerInput(
        "highlight_country", "Highlight a country (optional)",
        choices = c("None" = "", sort(data[["Country name"]])),
        options = list(`live-search` = TRUE, size = 10)
      ),

      shiny::helpText(
        "Explore, don't conclude: with this many possible combinations some",
        "results will be significant by chance. All results compare countries,",
        "not individual people, and show associations, not causes."
      ),
      shiny::helpText("Data: World Happiness Report 2023 and Global Country Information 2023.")
    ),

    # Key numbers ----
    # Side by side on wide screens; on laptop screens the effect gets its
    # own row, so the tiles stay short and the map below stays in view.
    bslib::layout_columns(
      fill = FALSE,
      col_widths = bslib::breakpoints(sm = c(12, 6, 6), xl = c(6, 3, 3)),
      # PLANNED (needs standardized, robust results from analyse_happiness()):
      # For a numeric predictor, show the change in happiness points
      # per 1 SD of the predictor with a robust (HC3) 95% CI, so effects of
      # different predictors can be compared. For a categorical predictor,
      # the largest group difference from the model with the controls.
      # Changes go in key_effect() (report_happiness.R).
      bslib::value_box(
        title = shiny::textOutput("effect_title", inline = TRUE),
        value = shiny::textOutput("effect_value"),
        shiny::textOutput("effect_detail"),
        theme = tile_theme()
      ),
      # PLANNED (needs standardized, robust results from analyse_happiness()):
      # With control variables, show the variance the predictor adds
      # on top of the controls (R-squared change) instead of the total.
      bslib::value_box(
        title = "Variance explained",
        value = shiny::textOutput("r_squared"),
        theme = tile_theme()
      ),
      bslib::value_box(
        title = "Countries analysed",
        value = shiny::textOutput("n_countries"),
        shiny::textOutput("n_excluded"),
        theme = tile_theme()
      )
    ),

    # Profile of the highlighted country, only shown when one is chosen.
    shiny::uiOutput("country_profile"),

    # Main views, one tab each ----
    # The map is the first thing a visitor sees: it introduces the data
    # before the analysis.
    bslib::navset_card_tab(
      id = "main_view",
      full_screen = TRUE,
      bslib::nav_panel(
        "Map",
        shiny::radioButtons(
          "map_layer", NULL, inline = TRUE,
          choices = c(
            "Happiness" = "happiness",
            "Chosen predictor" = "predictor",
            "Happier or unhappier than predicted" = "residual"
          )
        ),
        # A world map is about twice as wide as it is high; sizing the box
        # the same way leaves no empty space above and below the map.
        shiny::div(
          style = "aspect-ratio: 2 / 1; max-height: 460px;",
          plotly::plotlyOutput("map", height = "100%")
        ),
        shiny::uiOutput("map_note")
      ),
      bslib::nav_panel(
        "Plot",
        # IDEA (undecided):
        # Option to add a curved (spline) line next to the straight
        # model line, to show whether the association bends.
        # shiny::checkboxInput("show_curve", "Also show a curved line", FALSE),
        plotly::plotlyOutput("plot", height = "460px")
      ),
      bslib::nav_panel(
        "Model checks",
        # Each check gets one line on what to look for, so users without a
        # statistics background can still read them.
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::div(
            shiny::tags$h6("Residuals against predictions"),
            plotly::plotlyOutput("residual_plot", height = "320px"),
            shiny::helpText(
              "Look for a pattern: a curved line means the association is not straight;",
              "a funnel shape means the spread differs between countries."
            )
          ),
          shiny::div(
            shiny::tags$h6("Normality of the residuals"),
            plotly::plotlyOutput("qq_plot", height = "320px"),
            shiny::helpText(
              "Points close to the dashed line mean roughly normal residuals.",
              "With this many countries, small deviations matter little."
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          shiny::div(
            shiny::tags$h6("Most influential countries"),
            reactable::reactableOutput("influential_countries"),
            shiny::helpText(
              "Cook's distance: how much the predictions change without this country.",
              "Above 4 / number of countries counts as influential: worth a look,",
              "but not a reason to drop the country."
            )
          ),
          shiny::div(
            shiny::tags$h6("Overlap between variables (VIF)"),
            shiny::uiOutput("vif"),
            shiny::helpText(
              "Above 5, the variables overlap so much that their separate",
              "effects are hard to estimate."
            )
          )
        )
      ),
      bslib::nav_panel(
        "About the data",
        shiny::tags$ul(
          shiny::tags$li(
            "Happiness and the WHR factors: World Happiness Report 2023",
            "(Helliwell et al., 2023), averages over 2020-2022 from the",
            "Gallup World Poll."
          ),
          shiny::tags$li(
            "Country facts: Global Country Information Dataset 2023 (Kaggle).",
            "Compiled from several sources and years, which are not named per variable."
          ),
          shiny::tags$li(
            "One row per country: every result describes countries, not individual people."
          )
        ),
        # PLANNED (needs a variable list in prepare_data.R):
        # A table of every variable with its meaning, unit, source
        # and transformation (log, per person), read from the variable list
        # in prepare_data.R.
        # reactable::reactableOutput("variable_table"),
        coming_soon("A table explaining every variable, its unit and how it was transformed.")
      )
    ),

    # Explanation and coefficients ----
    bslib::layout_columns(
      col_widths = c(5, 7),
      # PLANNED (needs standardized, robust results from analyse_happiness()):
      # Describe the effect per 1 SD and name the control variables;
      # with controls, also compare the effect without and with them on the
      # same countries. Changes go in describe_result().
      bslib::card(
        bslib::card_header("What does this mean?"),
        shiny::uiOutput("summary")
      ),
      # PLANNED (needs standardized, robust results from analyse_happiness()):
      # Standardized and unstandardized estimates side by side,
      # robust CIs, and the real names of the control variables.
      # Changes go in format_coefficients().
      bslib::card(
        bslib::card_header("Model coefficients"),
        reactable::reactableOutput("coefficients")
      )
    )
  )

  server <- function(input, output, session) {
    # The moderator cannot be the same variable as the predictor.
    shiny::observeEvent(input$primary, {
      current <- shiny::isolate(input$moderator)
      shinyWidgets::updatePickerInput(
        session, "moderator",
        choices = moderator_choices(predictor_groups, input$primary),
        selected = if (!identical(current, input$primary)) current else ""
      )
    })

    # PLANNED (needs PR 8 merged):
    # Keep the predictor and moderator out of the control menu.
    # shiny::observeEvent(list(input$primary, input$moderator), {
    #   taken <- c(input$primary, input$moderator)
    #   shinyWidgets::updatePickerInput(
    #     session, "controls",
    #     choices = setdiff(predictors, taken),
    #     selected = setdiff(shiny::isolate(input$controls), taken)
    #   )
    # })

    # Only countries of the chosen continents can be highlighted; a
    # highlighted country on a continent that is no longer chosen is cleared.
    shiny::observeEvent(input$continents, {
      countries <- sort(analysed_data()[["Country name"]])
      current <- shiny::isolate(input$highlight_country)
      shinyWidgets::updatePickerInput(
        session, "highlight_country",
        choices = c("None" = "", countries),
        selected = if (current %in% countries) current else ""
      )
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # The countries of the continents chosen in the sidebar. Everything
    # that links results back to countries uses analysed_data(), because
    # the results refer to its row numbers.
    analysed_data <- shiny::reactive({
      data[data$continent %in% input$continents, , drop = FALSE]
    })

    # Some combinations cannot be estimated (e.g. too few countries);
    # keep analyse_happiness()'s error so it can be shown instead of a crash.
    # PLANNED (needs PR 8 merged):
    # Also pass its arguments:
    #   control_variables = input$controls,
    #   reference_continent = input$reference_continent,
    #   comparison_continent = input$comparison_continent
    result <- shiny::reactive({
      shiny::req(input$primary)
      moderator <- if (is.null(input$moderator)) "" else input$moderator
      problem <- selection_problem(analysed_data(), c(input$primary, moderator))
      if (!is.null(problem)) return(simpleError(problem))
      tryCatch(
        analyse_happiness(input$primary, moderator,
                          data = analysed_data(), predictors = predictors),
        error = function(error) error
      )
    })
    analysis_failed <- shiny::reactive(inherits(result(), "error"))
    usable_result <- shiny::reactive({
      shiny::req(!analysis_failed())
      result()
    })

    # Key numbers ----
    output$n_countries <- shiny::renderText(usable_result()$n_countries)
    output$n_excluded <- shiny::renderText({
      left_out <- nrow(data) - nrow(analysed_data())
      paste0(
        sprintf("%d excluded for missing data", usable_result()$n_excluded),
        if (left_out > 0) sprintf(", %d on other continents", left_out)
      )
    })
    output$r_squared <- shiny::renderText(
      sprintf("%.0f%%", 100 * usable_result()$r_squared)
    )
    effect <- shiny::reactive(key_effect(usable_result()))
    output$effect_title <- shiny::renderText(effect()$title)
    output$effect_value <- shiny::renderText(effect()$value)
    output$effect_detail <- shiny::renderText(effect()$detail)

    # Plot, summary and table ----
    # The error message is shown once, in place of the plot.
    # PLANNED (needs standardized, robust results from analyse_happiness()):
    # In plot_happiness(), hold control variables at their mean or
    # reference group in the prediction lines, draw the bands from the
    # robust standard errors, keep the x-axis in the original units, and
    # add the curved line when input$show_curve is ticked.
    output$plot <- plotly::renderPlotly({
      if (analysis_failed()) shiny::validate(conditionMessage(result()))
      plot <- plot_happiness(result(), countries = analysed_data()[["Country name"]],
                             highlight = input$highlight_country)
      # A single model line gets the app's purple; groups keep the
      # colour-blind friendly palette so they stay easy to tell apart.
      if (!result()$moderation) {
        plot <- plot + ggplot2::scale_colour_manual(
          values = app_colours$primary, aesthetics = c("colour", "fill")
        )
      }
      interactive_plot(plot)
    })
    output$summary <- shiny::renderUI(
      shiny::tags$ul(lapply(describe_result(usable_result()), shiny::tags$li))
    )
    output$coefficients <- reactable::renderReactable(
      reactable::reactable(
        format_coefficients(usable_result()),
        compact = TRUE, striped = TRUE, highlight = TRUE, pagination = FALSE,
        columns = list(Term = reactable::colDef(minWidth = 200))
      )
    )

    # Highlighted country ----
    output$country_profile <- shiny::renderUI({
      shiny::req(input$highlight_country != "")
      bslib::card(
        bslib::card_header(input$highlight_country),
        shiny::tags$ul(
          class = "mb-0",
          lapply(
            country_profile(input$highlight_country, usable_result(), data, analysed_data()),
            shiny::tags$li
          )
        )
      )
    })

    # Map ----
    output$map <- plotly::renderPlotly({
      if (analysis_failed()) shiny::validate(conditionMessage(result()))
      map <- map_happiness(result(), analysed_data(), layer = input$map_layer,
                           highlight = input$highlight_country)
      map <- plotly::layout(map, font = list(family = paste(app_fonts, collapse = ", ")))
      # Without scrollZoom = FALSE, scrolling the page over the map zooms it.
      plotly::config(map, displaylogo = FALSE, scrollZoom = FALSE)
    })
    output$map_note <- shiny::renderUI(
      shiny::tags$ul(
        class = "small text-muted mt-2",
        lapply(map_note(usable_result(), analysed_data(), input$map_layer), shiny::tags$li)
      )
    )

    # Model checks ----
    output$residual_plot <- plotly::renderPlotly(
      interactive_plot(plot_residuals(usable_result(), analysed_data()[["Country name"]]))
    )
    output$qq_plot <- plotly::renderPlotly(
      interactive_plot(plot_qq(usable_result(), analysed_data()[["Country name"]]))
    )
    output$influential_countries <- reactable::renderReactable(
      reactable::reactable(
        influential_countries(usable_result(), analysed_data()[["Country name"]]),
        compact = TRUE, pagination = FALSE
      )
    )
    output$vif <- shiny::renderUI({
      vif <- variance_inflation(usable_result())
      if (is.null(vif)) {
        return(shiny::p(class = "text-muted", "Needs at least two variables: choose a moderator."))
      }
      reactable::reactable(vif, compact = TRUE, pagination = FALSE)
    })

    # About the data ----
    # PLANNED (needs a variable list in prepare_data.R):
    # output$variable_table <- reactable::renderReactable(
    #   reactable::reactable(variable_info)
    # )
  }

  # plotly plots do not pick up the theme colours by themselves.
  thematic::thematic_shiny(accent = app_colours$primary)
  shiny::shinyApp(ui, server)
}

# Helpers ----

# Group predictors as numeric variables and categories, the default for
# the menus when no other grouping is given.
group_by_type <- function(data, predictors) {
  is_numeric <- vapply(data[predictors], is.numeric, logical(1))
  list(Numeric = predictors[is_numeric], Categories = predictors[!is_numeric])
}

# The menu of predictors: one group per heading, sorted alphabetically
# within each group. as.list() keeps a group with a single variable a
# group instead of turning it into one option.
predictor_menu <- function(predictor_groups) {
  predictor_groups <- predictor_groups[lengths(predictor_groups) > 0]
  lapply(predictor_groups, function(group) {
    group <- group[order(tolower(group))]
    stats::setNames(as.list(group), group)
  })
}

# The moderator menu: "None" plus every predictor except the chosen one.
moderator_choices <- function(predictor_groups, primary) {
  remaining <- lapply(predictor_groups, setdiff, primary)
  c(list(None = ""), predictor_menu(remaining))
}

# Explain why a selection cannot be analysed, before analysing it: no
# continents chosen, or too few countries with data on every chosen
# variable (analyse_happiness() needs at least 3). NULL when it can be.
selection_problem <- function(data, variables) {
  if (nrow(data) == 0) {
    return("No continents selected. Choose at least one continent in the sidebar.")
  }
  if (nrow(data) < 3) {
    return(sprintf(
      "The selected continents have only %d countries. The analysis needs at least 3: select more continents.",
      nrow(data)
    ))
  }
  variables <- variables[variables != ""]
  complete <- sum(stats::complete.cases(data[c("happiness", variables)]))
  if (complete < 3) {
    return(sprintf(
      paste("Only %d of the %d countries in the selected continents have data on %s.",
            "The analysis needs at least 3: select more continents or other variables."),
      complete, nrow(data), paste(variables, collapse = " and ")
    ))
  }
  NULL
}

# Stands in for a part of the app that is planned but not built yet.
coming_soon <- function(...) {
  shiny::div(
    class = "text-muted p-4",
    shiny::tags$strong("Coming soon. "),
    paste(...)
  )
}

# The font of the app and the plot: the computer's own system font.
app_fonts <- c("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")

# The colours of the app.
app_colours <- list(
  title_bar = "#7E5BB5",  # lilac purple
  primary = "#6B46A8",    # buttons, links and the model line in the plot
  sidebar = "#F4EFFA",    # very light lavender
  tile = "#EDE5F7",       # light lavender
  tile_text = "#3D2766"   # dark purple
)

# The theme of the app (Bootstrap 5 via bslib).
app_theme <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    primary = app_colours$primary,
    "navbar-bg" = app_colours$title_bar,
    base_font = do.call(bslib::font_collection, as.list(app_fonts))
  )
  # Space and a soft purple shadow between the blocks of the page; the
  # profile card sits inside its output placeholder, so it gets its own rule. The number tiles get less
  # padding and a smaller number, so they take little height. The 'None'
  # moderator is a real choice, so it is not greyed out.
  bslib::bs_add_rules(theme, "
    .bslib-page-main > :not(:empty):not(:last-child),
    .bslib-page-main > .shiny-html-output > .card { margin-bottom: 1.5rem; }
    .bslib-page-main .card { box-shadow: 0 2px 10px rgba(61, 39, 102, 0.12); }
    .bslib-sidebar-layout .bslib-value-box > .card-body { padding: 0; }
    .bslib-sidebar-layout .bslib-value-box .value-box-area { padding: 0.75rem 1rem; }
    .bslib-sidebar-layout .bslib-value-box .value-box-value { font-size: 1.75rem; }
    .bslib-sidebar-layout .bootstrap-select > .dropdown-toggle.bs-placeholder,
    .bslib-sidebar-layout .bootstrap-select > .dropdown-toggle.bs-placeholder:hover,
    .bslib-sidebar-layout .bootstrap-select > .dropdown-toggle.bs-placeholder:focus {
      color: var(--bs-body-color);
    }
  ")
}

# The colours of the number tiles at the top.
tile_theme <- function() {
  bslib::value_box_theme(bg = app_colours$tile, fg = app_colours$tile_text)
}

# Turn the ggplot into a plotly plot that shows the country on hover.
interactive_plot <- function(plot) {
  interactive <- plotly::ggplotly(plot, tooltip = "text")

  # Clean up the legend: ggplotly names traces like "(Africa,1)" and gives
  # the line and its band separate legend entries.
  seen <- character(0)
  for (i in seq_along(interactive$x$data)) {
    trace <- interactive$x$data[[i]]
    name <- sub("^\\((.*?)(,\\d+)?\\)$", "\\1", if (is.null(trace$name)) "" else trace$name)
    interactive$x$data[[i]]$name <- name
    interactive$x$data[[i]]$legendgroup <- name
    interactive$x$data[[i]]$showlegend <- isTRUE(trace$showlegend) && !name %in% seen
    if (isTRUE(trace$showlegend)) seen <- c(seen, name)
    # Only the countries get a hover box, not the model lines and bands.
    if (is.null(trace$text) || all(trace$text == "")) {
      interactive$x$data[[i]]$hoverinfo <- "skip"
    }
  }
  # ggplotly gives every text element ggplot's empty font name (''), which
  # browsers show as a serif font. Remove it so the app's font is used.
  interactive$x$layout <- remove_empty_fonts(interactive$x$layout)
  interactive <- plotly::layout(interactive, font = list(family = paste(app_fonts, collapse = ", ")))
  plotly::config(interactive, displaylogo = FALSE)
}

remove_empty_fonts <- function(settings) {
  if (!is.list(settings)) return(settings)
  if (!is.null(settings$family) && settings$family %in% c("", "''")) settings$family <- NULL
  lapply(settings, remove_empty_fonts)
}

# What the PLANNED parts need from other files ----
# None of these exist yet.
#
# From prepare_data.R:
#   variable_info        Data frame: name, label, unit, group, source,
#                        transformation. One row per available predictor.
#   predictor_menu(variable_info, predictors)
#                        Grouped, labelled choices for the pickers.
#
# From analyse_happiness.R (in the list it returns):
#   z-scored numeric predictors (with their mean and SD in result$scaling),
#   robust (HC3) standard errors, the robust interaction test, and the
#   R-squared the predictor adds on top of the controls.
#
# From PR 8 (already open), in analyse_happiness.R:
#   the control_variables, reference_continent and comparison_continent
#   arguments.
