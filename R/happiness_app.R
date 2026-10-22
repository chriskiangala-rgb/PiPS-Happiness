# Shiny app to explore happiness and country characteristics ----

#' Create the happiness Shiny app
#'
#' Lets the user pick a predictor and an optional moderator, then shows key
#' numbers, an interactive plot (hover a point to see the country), a
#' plain-language summary and the coefficient table of analyse_happiness().
#'
#' Parts marked PLANNED are not built yet. They describe what the app should
#' do once the data, analysis and report functions support it; the commented
#' code shows where each part goes. "Coming soon" boxes stand in for them, so
#' the app keeps running in the meantime.
#'
#' @param data The combined dataset (combined_data from prepare_data.R).
#' @param predictors The variable names the user can choose from.
#'
#' @return A Shiny app object; start it with shiny::runApp().
happiness_app <- function(data, predictors) {
  default_predictor <- if ("Social support" %in% predictors) "Social support" else predictors[1]

  ui <- bslib::page_sidebar(
    title = "What goes together with happiness?",
    theme = app_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 320,
      bg = app_colours$sidebar,

      # PLANNED: group the choices into "WHR factors", "Country facts" and
      # "Categories", and show readable names with units, e.g.
      # "Tax revenue (% of GDP)". Both come from a variable list in
      # prepare_data.R (name, label, unit, group, transformation), so the
      # menus, the plot axis and the "About the data" tab share one source:
      #   choices = predictor_menu(variable_info, predictors)
      shinyWidgets::pickerInput(
        "primary", "Predictor",
        choices = predictors, selected = default_predictor,
        options = list(`live-search` = TRUE, size = 10)
      ),
      shinyWidgets::pickerInput(
        "moderator", "Moderator (optional)",
        choices = moderator_choices(predictors, default_predictor),
        options = list(`live-search` = TRUE, size = 10)
      ),
      shiny::helpText(
        "A moderator tests whether the association between the predictor",
        "and happiness differs between groups or levels of another variable."
      ),

      # PLANNED: control variables, at most 3, never the predictor or the
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

      # PLANNED: compare two continents. Only shown when continent is the
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

      # PLANNED: choose which countries are analysed, to check whether a
      # result depends on a few countries or on one part of the world.
      # - Continents: all selected by default. This generalises the two-
      #   continent comparison above; once it exists, that comparison only
      #   needs the reference continent.
      # - Exclude countries: e.g. the influential countries from the Model
      #   checks tab.
      # The tiles and the plot caption always show how many countries are
      # included and which were left out, so cherry-picking stays visible.
      # The filtering happens before analyse_happiness() is called, see
      # analysed_data() in the server.
      # shinyWidgets::pickerInput(
      #   "continents", "Continents",
      #   choices = levels(data$continent), selected = levels(data$continent),
      #   multiple = TRUE,
      #   options = list(`actions-box` = TRUE)
      # ),
      # shinyWidgets::pickerInput(
      #   "excluded_countries", "Leave out countries (optional)",
      #   choices = sort(data[["Country name"]]),
      #   multiple = TRUE,
      #   options = list(`live-search` = TRUE, size = 10)
      # ),

      # Highlight one country. Only changes what is shown, not the analysis.
      # PLANNED: also highlight it in the plot and show its profile (see the
      # country card below the tiles).
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
    bslib::layout_columns(
      fill = FALSE,
      col_widths = c(6, 3, 3),
      # PLANNED: for a numeric predictor, show the change in happiness points
      # per 1 SD of the predictor with a robust (HC3) 95% CI, so effects of
      # different predictors can be compared. For a categorical predictor,
      # the largest group difference from the model with the controls.
      # Changes go in key_effect() (report_happiness.R).
      bslib::value_box(
        title = shiny::textOutput("effect_title", inline = TRUE),
        value = shiny::textOutput("effect_value"),
        shiny::textOutput("effect_detail"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme = tile_theme()
      ),
      # PLANNED: with control variables, show the variance the predictor adds
      # on top of the controls (R-squared change) instead of the total.
      bslib::value_box(
        title = "Variance in happiness explained",
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

    # PLANNED: profile card of the highlighted country, shown only when one
    # is chosen: its happiness and rank, its value on the predictor (and
    # moderator) with rank, and how much happier or unhappier it is than
    # the model predicts. If it was left out of the analysis, say why
    # (missing data or excluded by the user).
    # shiny::uiOutput("country_profile"),

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
        plotly::plotlyOutput("map", height = "460px"),
        shiny::uiOutput("map_note")
      ),
      bslib::nav_panel(
        "Plot",
        # PLANNED: option to add a curved (spline) line next to the straight
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
              "Above 4 / number of countries counts as influential; try leaving",
              "these countries out to see whether the result holds."
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
        # PLANNED: a table of every variable with its meaning, unit, source
        # and transformation (log, per person), read from the variable list
        # in prepare_data.R.
        # reactable::reactableOutput("variable_table"),
        coming_soon("A table explaining every variable, its unit and how it was transformed.")
      )
    ),

    # Explanation and coefficients ----
    bslib::layout_columns(
      col_widths = c(5, 7),
      # PLANNED: describe the effect per 1 SD and name the control variables;
      # with controls, also compare the effect without and with them on the
      # same countries. Changes go in describe_result().
      bslib::card(
        bslib::card_header("What does this mean?"),
        shiny::uiOutput("summary")
      ),
      # PLANNED: standardized and unstandardized estimates side by side,
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
        choices = moderator_choices(predictors, input$primary),
        selected = if (!identical(current, input$primary)) current else ""
      )
    })

    # PLANNED: keep the predictor and moderator out of the control menu.
    # shiny::observeEvent(list(input$primary, input$moderator), {
    #   taken <- c(input$primary, input$moderator)
    #   shinyWidgets::updatePickerInput(
    #     session, "controls",
    #     choices = setdiff(predictors, taken),
    #     selected = setdiff(shiny::isolate(input$controls), taken)
    #   )
    # })

    # PLANNED: the countries chosen in the sidebar. analyse_happiness(),
    # the map and the plot then use analysed_data() instead of data.
    # analysed_data <- shiny::reactive({
    #   keep <- data$continent %in% input$continents &
    #     !data[["Country name"]] %in% input$excluded_countries
    #   data[keep, , drop = FALSE]
    # })

    # Some combinations cannot be estimated (e.g. too few countries);
    # keep analyse_happiness()'s error so it can be shown instead of a crash.
    # PLANNED: also pass the PR 8 arguments:
    #   control_variables = input$controls,
    #   reference_continent = input$reference_continent,
    #   comparison_continent = input$comparison_continent
    result <- shiny::reactive({
      shiny::req(input$primary)
      moderator <- if (is.null(input$moderator)) "" else input$moderator
      tryCatch(
        analyse_happiness(input$primary, moderator,
                          data = data, predictors = predictors),
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
    output$n_excluded <- shiny::renderText(
      sprintf("%d excluded because of missing data", usable_result()$n_excluded)
    )
    output$r_squared <- shiny::renderText(
      sprintf("%.0f%%", 100 * usable_result()$r_squared)
    )
    effect <- shiny::reactive(key_effect(usable_result()))
    output$effect_title <- shiny::renderText(effect()$title)
    output$effect_value <- shiny::renderText(effect()$value)
    output$effect_detail <- shiny::renderText(effect()$detail)

    # Plot, summary and table ----
    # The error message is shown once, in place of the plot.
    # PLANNED (in plot_happiness()): hold control variables at their mean or
    # reference group in the prediction lines, draw the bands from the
    # robust standard errors, keep the x-axis in the original units, and
    # add the curved line when input$show_curve is ticked. Draw the
    # highlighted country (input$highlight_country) larger and labelled.
    output$plot <- plotly::renderPlotly({
      if (analysis_failed()) shiny::validate(conditionMessage(result()))
      plot <- plot_happiness(result(), countries = data[["Country name"]])
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
    # PLANNED: see the country card above the tabs for what it shows.
    # output$country_profile <- shiny::renderUI({
    #   shiny::req(input$highlight_country != "")
    #   country_profile(input$highlight_country, usable_result(), data)
    # })

    # Map ----
    output$map <- plotly::renderPlotly({
      if (analysis_failed()) shiny::validate(conditionMessage(result()))
      map <- map_happiness(result(), data, layer = input$map_layer,
                           highlight = input$highlight_country)
      map <- plotly::layout(map, font = list(family = paste(app_fonts, collapse = ", ")))
      plotly::config(map, displaylogo = FALSE)
    })
    output$map_note <- shiny::renderUI(
      shiny::tags$ul(
        class = "small text-muted mt-2",
        lapply(map_note(usable_result(), data, input$map_layer), shiny::tags$li)
      )
    )

    # Model checks ----
    output$residual_plot <- plotly::renderPlotly(
      interactive_plot(plot_residuals(usable_result(), data[["Country name"]]))
    )
    output$qq_plot <- plotly::renderPlotly(
      interactive_plot(plot_qq(usable_result(), data[["Country name"]]))
    )
    output$influential_countries <- reactable::renderReactable(
      reactable::reactable(
        influential_countries(usable_result(), data[["Country name"]]),
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
    # PLANNED: output$variable_table <- reactable::renderReactable(
    #   reactable::reactable(variable_info)
    # )
  }

  # plotly plots do not pick up the theme colours by themselves.
  thematic::thematic_shiny(accent = app_colours$primary)
  shiny::shinyApp(ui, server)
}

# Helpers ----

# The moderator menu: "None" plus every predictor except the chosen one.
moderator_choices <- function(predictors, primary) {
  c("None" = "", setdiff(predictors, primary))
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
  # The 'None' moderator is a real choice, so do not grey it out.
  bslib::bs_add_rules(theme, "
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

# Planned helpers ----
# Used by the PLANNED parts above; none of these exist yet.
#
# In prepare_data.R:
#   variable_info        Data frame: name, label, unit, group, source,
#                        transformation. One row per available predictor.
#   predictor_menu(variable_info, predictors)
#                        Grouped, labelled choices for the pickers.
#
# In analyse_happiness.R (results stored in the list it returns):
#   z-scored numeric predictors (with their mean and SD in result$scaling),
#   robust (HC3) standard errors, the robust interaction test, and the
#   R-squared the predictor adds on top of the controls.
#
# In R/report_happiness.R:
#   country_profile(country, result, data)
#                                        Card with the country's values, ranks
#                                        and residual, or why it was left out.
