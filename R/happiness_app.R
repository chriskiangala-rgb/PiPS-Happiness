# Shiny app to explore happiness and country characteristics ----

#' Create the happiness Shiny app
#'
#' Lets the user pick a predictor and an optional moderator, then shows key
#' numbers, an interactive plot (hover a point to see the country), a
#' plain-language summary and the coefficient table of analyse_happiness().
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
      shiny::helpText("Data: World Happiness Report 2023 and Global Country Information 2023.")
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "Countries analysed",
        value = shiny::textOutput("n_countries"),
        showcase = bsicons::bs_icon("globe-europe-africa")
      ),
      bslib::value_box(
        title = "Variance in happiness explained",
        value = shiny::textOutput("r_squared"),
        showcase = bsicons::bs_icon("pie-chart")
      ),
      bslib::value_box(
        title = shiny::textOutput("p_label", inline = TRUE),
        value = shiny::textOutput("p_value"),
        showcase = bsicons::bs_icon("clipboard-data")
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Happiness and the chosen predictor"),
      plotly::plotlyOutput("plot", height = "460px")
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("What does this mean?"),
        shiny::uiOutput("summary")
      ),
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

    # Some combinations cannot be estimated (e.g. too few countries);
    # keep analyse_happiness()'s error so it can be shown instead of a crash.
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
    output$r_squared <- shiny::renderText(
      sprintf("%.0f%%", 100 * usable_result()$r_squared)
    )
    output$p_label <- shiny::renderText({
      if (usable_result()$moderation) "p-value of the interaction" else "p-value of the model"
    })
    output$p_value <- shiny::renderText(format_p(key_p_value(usable_result())))

    # Plot, summary and table ----
    # The error message is shown once, in place of the plot.
    output$plot <- plotly::renderPlotly({
      if (analysis_failed()) shiny::validate(conditionMessage(result()))
      interactive_plot(plot_happiness(result(), countries = data[["Country name"]]))
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
  }

  thematic::thematic_shiny()
  shiny::shinyApp(ui, server)
}

# Helpers ----

# The moderator menu: "None" plus every predictor except the chosen one.
moderator_choices <- function(predictors, primary) {
  c("None" = "", setdiff(predictors, primary))
}

# The font of the app and the plot: the computer's own system font.
app_fonts <- c("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")

# The colours and fonts of the app (Bootstrap 5 via bslib).
app_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly", base_font = do.call(bslib::font_collection, as.list(app_fonts)))
}

# Without a moderator: the p-value of the whole model.
# With a moderator: the p-value of the joint interaction test.
key_p_value <- function(result) {
  if (result$moderation) return(result$interaction_test$p_value)
  model_test <- summary(result$model)$fstatistic
  unname(pf(model_test[1], model_test[2], model_test[3], lower.tail = FALSE))
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
