# Shiny app to explore happiness and country characteristics ----

#' Create the happiness Shiny app
#'
#' Lets the user pick a predictor and an optional moderator, then shows the
#' plot, a plain-language summary and the coefficient table of
#' analyse_happiness().
#'
#' @param data The combined dataset (combined_data from prepare_data.R).
#' @param predictors The variable names the user can choose from.
#'
#' @return A Shiny app object; start it with shiny::runApp().
happiness_app <- function(data, predictors) {
  default_predictor <- if ("Social support" %in% predictors) "Social support" else predictors[1]

  ui <- shiny::fluidPage(
    shiny::titlePanel("What goes together with happiness?"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("primary", "Predictor", choices = predictors,
                           selected = default_predictor),
        shiny::selectInput("moderator", "Moderator (optional)",
                           choices = moderator_choices(predictors, default_predictor)),
        shiny::helpText(
          "A moderator tests whether the association between the predictor",
          "and happiness differs between groups or levels of another variable."
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot", height = "450px"),
        shiny::uiOutput("summary"),
        shiny::tableOutput("coefficients")
      )
    )
  )

  server <- function(input, output, session) {
    # The moderator cannot be the same variable as the predictor.
    shiny::observeEvent(input$primary, {
      current <- shiny::isolate(input$moderator)
      shiny::updateSelectInput(
        session, "moderator",
        choices = moderator_choices(predictors, input$primary),
        selected = if (current != input$primary) current else ""
      )
    })

    # Some combinations cannot be estimated (e.g. too few countries);
    # keep analyse_happiness()'s error so it can be shown instead of a crash.
    result <- shiny::reactive({
      shiny::req(input$primary)
      tryCatch(
        analyse_happiness(input$primary, input$moderator,
                          data = data, predictors = predictors),
        error = function(error) error
      )
    })
    analysis_failed <- shiny::reactive(inherits(result(), "error"))

    # The error message is shown once, in place of the plot.
    output$plot <- shiny::renderPlot({
      shiny::validate(shiny::need(!analysis_failed(), conditionMessage(result())))
      plot_happiness(result())
    })
    output$summary <- shiny::renderUI({
      shiny::req(!analysis_failed())
      shiny::tags$ul(lapply(describe_result(result()), shiny::tags$li))
    })
    output$coefficients <- shiny::renderTable({
      shiny::req(!analysis_failed())
      format_coefficients(result())
    })
  }

  shiny::shinyApp(ui, server)
}

# The moderator menu: "None" plus every predictor except the chosen one.
moderator_choices <- function(predictors, primary) {
  c("None" = "", setdiff(predictors, primary))
}
