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
      bg = app_colours$sidebar,
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
      col_widths = c(6, 3, 3),
      bslib::value_box(
        title = shiny::textOutput("effect_title", inline = TRUE),
        value = shiny::textOutput("effect_value"),
        shiny::textOutput("effect_detail"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme = tile_theme()
      ),
      bslib::value_box(
        title = "Variance in happiness explained",
        value = shiny::textOutput("r_squared"),
        theme = tile_theme()
      ),
      bslib::value_box(
        title = "Countries analysed",
        value = shiny::textOutput("n_countries"),
        theme = tile_theme()
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
    effect <- shiny::reactive(key_effect(usable_result()))
    output$effect_title <- shiny::renderText(effect()$title)
    output$effect_value <- shiny::renderText(effect()$value)
    output$effect_detail <- shiny::renderText(effect()$detail)

    # Plot, summary and table ----
    # The error message is shown once, in place of the plot.
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
