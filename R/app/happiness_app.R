# Shiny app to explore happiness and country characteristics ----

#' Create the happiness Shiny app
#'
#' Lets the user pick a predictor, an optional moderator, control variables
#' and the continents to analyse, then shows key numbers, a world map, an
#' interactive plot (hover a point to see the country), model checks, a
#' plain-language summary and the coefficient table of analyse_happiness().
#'
#' @param data The combined dataset (combined_data from prepare_data.R).
#' @param predictors The variable names the user can choose from.
#' @param predictor_groups Optional named list that splits predictors into
#'   groups for the menus, e.g. by data source. Every predictor must be in
#'   exactly one group. By default: numeric variables and categories.
#' @param predictor_notes Optional named character vector with a caution per
#'   variable (predictor_notes from prepare_data.R). The note is shown when
#'   that variable is chosen as predictor, moderator or control.
#' @param variable_info Optional data frame with one row per predictor
#'   (variable_info from prepare_data.R): variable, label, unit, source,
#'   transformation, n and sd. Gives readable menus with units, the variable
#'   table under "About the data" and the effect per SD.
#'
#' @return A Shiny app object; start it with shiny::runApp().
happiness_app <- function(data, predictors,
                          predictor_groups = group_by_type(data, predictors),
                          predictor_notes = NULL, variable_info = NULL) {
  if (!setequal(unlist(predictor_groups), predictors) ||
      anyDuplicated(unlist(predictor_groups))) {
    stop("predictor_groups must contain every predictor exactly once.")
  }
  # Menus with readable labels and the unit under each variable.
  menu <- function(choices) menu_options(choices, variable_info)
  default_predictor <- if ("Social support" %in% predictors) "Social support" else predictors[1]

  ui <- bslib::page_sidebar(
    title = "What goes together with happiness?",
    theme = app_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 320,
      bg = app_colours()$sidebar,

      shinyWidgets::pickerInput(
        "primary", "Predictor",
        choices = menu(predictor_menu(predictor_groups))$choices, selected = default_predictor,
        choicesOpt = menu(predictor_menu(predictor_groups))$options,
        options = list(`live-search` = TRUE, size = 10)
      ),
      shinyWidgets::pickerInput(
        "moderator", "Moderator (optional)",
        choices = menu(moderator_choices(predictor_groups, default_predictor))$choices,
        choicesOpt = menu(moderator_choices(predictor_groups, default_predictor))$options,
        options = list(`live-search` = TRUE, size = 10)
      ),
      shiny::helpText(
        "A moderator tests whether the association between the predictor",
        "and happiness differs between groups or levels of another variable."
      ),

      # Control variables, at most 3, never the predictor or the moderator.
      shinyWidgets::pickerInput(
        "controls", "Control variables (optional, max. 3)",
        choices = menu(control_choices(predictor_groups, default_predictor))$choices,
        choicesOpt = menu(control_choices(predictor_groups, default_predictor))$options,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE, `max-options` = 3, size = 10,
          `none-selected-text` = "None"
        )
      ),
      shiny::helpText(
        "Control variables are held constant: the effect shown is the",
        "association that remains between countries that are equal on them."
      ),
      # Cautions about the chosen variables, e.g. much missing data.
      shiny::uiOutput("variable_notes"),

      # Choose which continents are analysed, to check whether a result
      # holds in one part of the world. Single countries cannot be left out:
      # hand-picking which countries count biases the results (selection
      # bias) and invites dropping countries for who they are rather than
      # for a statistical reason. The tiles always show how many countries
      # were left out.
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

      # The continent the other continents are compared with. Only shown
      # when continent is the predictor or the moderator; the menu above
      # picks which continents are compared.
      shiny::conditionalPanel(
        "input.primary == 'continent' || input.moderator == 'continent'",
        shinyWidgets::pickerInput(
          "reference_continent", "Reference continent",
          choices = levels(data$continent), selected = levels(data$continent)[1]
        )
      ),

      # Highlight one country. Only changes what is shown, not the analysis.
      shinyWidgets::pickerInput(
        "highlight_country", "Highlight a country (optional)",
        choices = c("None" = "", sort(data[["Country name"]])),
        options = list(`live-search` = TRUE, size = 10)
      ),

      # A straight line, or a curve (natural spline) for a numeric predictor
      # whose association bends; not with a moderator. Lower in the sidebar:
      # the line is the default, and the curve warning points here.
      shiny::radioButtons(
        "shape", "Shape", inline = TRUE,
        choices = c("Straight line" = "line", "Curve" = "curve")
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
      bslib::value_box(
        title = shiny::textOutput("effect_title", inline = TRUE),
        value = shiny::textOutput("effect_value"),
        shiny::textOutput("effect_detail"),
        shiny::textOutput("effect_sd"),
        shiny::textOutput("effect_baseline"),
        theme = tile_theme()
      ),
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
        plotly::plotlyOutput("plot", height = "460px"),
        shiny::uiOutput("plot_note")
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
        if (!is.null(variable_info)) {
          shiny::tagList(
            shiny::tags$h6("The variables"),
            shiny::helpText(
              "Countries: how many have data on the variable. SD: the standard deviation",
              "across those countries (on the log scale for logged variables), the step",
              "used for the effect per SD."
            ),
            reactable::reactableOutput("variable_table")
          )
        }
      )
    ),

    # Explanation and coefficients ----
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
      choices <- menu(moderator_choices(predictor_groups, input$primary))
      shinyWidgets::updatePickerInput(
        session, "moderator",
        choices = choices$choices, choicesOpt = choices$options,
        selected = if (!identical(current, input$primary)) current else ""
      )
    })

    # Keep the predictor and moderator out of the control menu.
    shiny::observeEvent(list(input$primary, input$moderator), {
      taken <- c(input$primary, input$moderator)
      choices <- menu(control_choices(predictor_groups, taken))
      shinyWidgets::updatePickerInput(
        session, "controls",
        choices = choices$choices, choicesOpt = choices$options,
        selected = setdiff(shiny::isolate(input$controls), taken)
      )
    }, ignoreInit = TRUE)

    # Only a chosen continent can be the reference; keep the current one
    # while it is still chosen.
    shiny::observeEvent(input$continents, {
      current <- shiny::isolate(input$reference_continent)
      chosen <- if (length(input$continents) > 0) input$continents else levels(data$continent)
      shinyWidgets::updatePickerInput(
        session, "reference_continent",
        choices = chosen,
        selected = if (isTRUE(current %in% chosen)) current else chosen[1]
      )
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
    # When continent is the predictor or the moderator, the chosen continents
    # and the reference continent are passed on; analyse_happiness() does not
    # accept them otherwise, so then the sidebar filter alone picks the
    # countries. The data is filtered either way, so the results refer to
    # the row numbers of analysed_data().
    result <- shiny::reactive({
      shiny::req(input$primary)
      moderator <- if (is.null(input$moderator)) "" else input$moderator
      controls <- setdiff(input$controls, c(input$primary, moderator))
      compares_continents <- "continent" %in% c(input$primary, moderator)
      problem <- selection_problem(analysed_data(), c(input$primary, moderator, controls),
                                   compares_continents)
      if (!is.null(problem)) return(simpleError(problem))
      chosen_reference <- isTRUE(input$reference_continent %in% input$continents)
      reference <- if (compares_continents && chosen_reference) {
        input$reference_continent
      } else {
        ""
      }
      result <- tryCatch(
        analyse_happiness(
          input$primary, moderator,
          data = analysed_data(), predictors = predictors,
          control_variables = controls,
          selected_continents = if (compares_continents) input$continents,
          reference_continent = reference,
          shape = if (is.null(input$shape)) "line" else input$shape
        ),
        error = function(error) error
      )
      # The tiles, summary, plot and tables show the readable labels.
      if (!inherits(result, "error") && !is.null(variable_info)) {
        result$labels <- stats::setNames(variable_info$label, variable_info$variable)
      }
      result
    })
    analysis_failed <- shiny::reactive(inherits(result(), "error"))
    usable_result <- shiny::reactive({
      shiny::req(!analysis_failed())
      result()
    })
    # Country names in exactly the same order as the model observations.
    model_countries <- shiny::reactive({
      analysed_data()[["Country name"]][usable_result()$model_rows]
    })

    # Key numbers ----
    output$n_countries <- shiny::renderText(usable_result()$n_countries)
    output$n_excluded <- shiny::renderText({
      left_out <- nrow(data) - nrow(analysed_data())
      small <- usable_result()$n_small_groups
      paste0(
        sprintf("%d excluded for missing data", usable_result()$n_excluded),
        if (isTRUE(small > 0)) {
          sprintf(", %d in groups of fewer than %d (%s)", small, usable_result()$min_group_size,
                  paste(unlist(usable_result()$excluded_groups), collapse = ", "))
        },
        if (left_out > 0) sprintf(", %d on other continents", left_out)
      )
    })
    output$r_squared <- shiny::renderText(
      sprintf("%.0f%%", 100 * usable_result()$r_squared)
    )
    # The SD of the predictor across all countries, for the effect per SD.
    predictor_sd <- shiny::reactive({
      if (is.null(variable_info)) return(NULL)
      variable_info$sd[match(usable_result()$primary, variable_info$variable)]
    })
    effect <- shiny::reactive(key_effect(usable_result(), model_countries(), predictor_sd()))
    output$effect_sd <- shiny::renderText(effect()$per_sd)
    output$effect_title <- shiny::renderText(effect()$title)
    output$effect_value <- shiny::renderText(effect()$value)
    output$effect_detail <- shiny::renderText(effect()$detail)
    # The effect without the moderator, the curve against the line, and the
    # example of a logged predictor or a curve.
    output$effect_baseline <- shiny::renderText(
      paste(c(effect()$baseline, effect()$example), collapse = ". ")
    )

    # Notes on the chosen variables ----
    output$variable_notes <- shiny::renderUI({
      chosen <- unique(c(input$primary, input$moderator, input$controls))
      chosen <- chosen[chosen %in% names(predictor_notes)]
      if (length(chosen) == 0) return(NULL)
      shiny::div(
        class = "small alert alert-warning py-2 px-3",
        lapply(chosen, function(variable) {
          shiny::p(class = "mb-1", shiny::tags$b(paste0(variable, ":")),
                   predictor_notes[[variable]])
        })
      )
    })

    # Plot, summary and table ----
    # The error message is shown once, in place of the plot.
    output$plot <- plotly::renderPlotly({
      if (analysis_failed()) shiny::validate(conditionMessage(result()))
      plot <- plot_happiness(result(), countries = analysed_data()[["Country name"]],
                             highlight = input$highlight_country)
      # A single model line gets the app's purple; groups keep the
      # colour-blind friendly palette so they stay easy to tell apart.
      if (!result()$moderation) {
        plot <- plot + ggplot2::scale_colour_manual(
          values = app_colours()$primary, aesthetics = c("colour", "fill")
        )
      }
      interactive_plot(plot)
    })
    # Below the plot: what it shows, and a warning when a curve fits
    # clearly better than the straight line.
    output$plot_note <- shiny::renderUI({
      curve <- curvature_test(usable_result())
      shiny::tagList(
        shiny::p(class = "small text-muted mt-2 mb-0", plot_caption(usable_result(), " ")),
        if (!is.null(curve) && curve$misleading) {
          shiny::p(class = "small alert alert-warning mt-2 mb-0 py-2 px-3",
                   describe_curvature(curve, usable_result()),
                   "Choose Shape: Curve in the sidebar to fit a curve.")
        }
      )
    })
    output$summary <- shiny::renderUI(
      shiny::tags$ul(lapply(describe_result(usable_result(), predictor_sd()), shiny::tags$li))
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
      map <- plotly::layout(map, font = list(family = paste(app_fonts(), collapse = ", ")))
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
      interactive_plot(
        plot_residuals(usable_result(), model_countries())
      )
    )

    output$qq_plot <- plotly::renderPlotly(
      interactive_plot(
        plot_qq(usable_result(), model_countries())
      )
    )

    output$influential_countries <- reactable::renderReactable(
      reactable::reactable(
        influential_countries(usable_result(), model_countries()),
        compact = TRUE,
        pagination = FALSE
      )
    )
    output$variable_table <- reactable::renderReactable({
      shiny::req(variable_info)
      table <- data.frame(
        Variable = variable_info$label,
        Unit = variable_info$unit,
        Source = variable_info$source,
        Transformation = variable_info$transformation,
        Countries = variable_info$n,
        SD = ifelse(is.na(variable_info$sd), "", format_number(variable_info$sd)),
        check.names = FALSE
      )
      reactable::reactable(
        table[order(tolower(table$Variable)), ],
        compact = TRUE, striped = TRUE, pagination = FALSE, searchable = TRUE,
        columns = list(
          Variable = reactable::colDef(minWidth = 180),
          Unit = reactable::colDef(minWidth = 180)
        )
      )
    })
    output$vif <- shiny::renderUI({
      vif <- variance_inflation(usable_result())
      if (is.null(vif)) {
        return(shiny::p(
          class = "text-muted",
          "Needs at least two variables: choose a moderator or a control variable."
        ))
      }
      reactable::reactable(vif, compact = TRUE, pagination = FALSE)
    })

  }

  # plotly plots do not pick up the theme colours by themselves.
  thematic::thematic_shiny(accent = app_colours()$primary)
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

# Give a menu readable labels and show each variable's unit (and
# transformation) below it. The values stay the variable names. Without
# variable_info the menu is returned as it is.
menu_options <- function(choices, variable_info) {
  if (is.null(variable_info)) return(list(choices = choices, options = NULL))
  label_of <- function(variable) {
    row <- match(variable, variable_info$variable)
    if (is.na(row)) variable else variable_info$label[row]
  }
  # Sorted by label within each group, as the menu is read.
  choices <- lapply(choices, function(group) {
    if (!is.list(group)) return(group)
    group <- unlist(group, use.names = FALSE)
    labels <- vapply(group, label_of, character(1))
    order <- order(tolower(labels))
    stats::setNames(as.list(group[order]), labels[order])
  })
  values <- unlist(choices, use.names = FALSE)
  rows <- match(values, variable_info$variable)
  subtext <- ifelse(
    is.na(rows), "",
    ifelse(variable_info$transformation[rows] %in% c("none", ""),
           variable_info$unit[rows],
           paste0(variable_info$unit[rows], "; ", variable_info$transformation[rows]))
  )
  # tokens: live search also finds the variable by its original name.
  list(choices = choices, options = list(subtext = subtext, tokens = values))
}

# The moderator menu: "None" plus every predictor except the chosen one.
moderator_choices <- function(predictor_groups, primary) {
  remaining <- lapply(predictor_groups, setdiff, primary)
  c(list(None = ""), predictor_menu(remaining))
}

# The control menu: every predictor except the predictor and moderator.
control_choices <- function(predictor_groups, taken) {
  predictor_menu(lapply(predictor_groups, setdiff, taken))
}

# Explain why a selection cannot be analysed, before analysing it: no
# continents chosen, a single continent while continents are compared, too
# few countries with data on every chosen variable (analyse_happiness()
# needs at least 3), or a variable that is the same for all of them. NULL
# when it can be.
selection_problem <- function(data, variables, compares_continents = FALSE) {
  if (nrow(data) == 0) {
    return("No continents selected. Choose at least one continent in the sidebar.")
  }
  if (compares_continents && length(unique(data$continent)) < 2) {
    return("Continent is the predictor or moderator: select at least two continents to compare.")
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
  # E.g. continent as a control variable while one continent is selected.
  complete_rows <- stats::complete.cases(data[c("happiness", variables)])
  for (variable in variables) {
    if (length(unique(data[[variable]][complete_rows])) < 2) {
      return(sprintf(
        "%s is the same for every country in the selected continents, so its effect cannot be estimated: choose another variable or more continents.",
        variable
      ))
    }
  }
  NULL
}

# The font of the app and the plot: the computer's own system font.
app_fonts <- function() {
  c("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")
}

# The colours of the app.
app_colours <- function() {
  list(
    title_bar = "#7E5BB5",  # lilac purple
    primary = "#6B46A8",    # buttons, links and the model line in the plot
    sidebar = "#F4EFFA",    # very light lavender
    tile = "#EDE5F7",       # light lavender
    tile_text = "#3D2766"   # dark purple
  )
}

# The theme of the app (Bootstrap 5 via bslib).
app_theme <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    primary = app_colours()$primary,
    "navbar-bg" = app_colours()$title_bar,
    base_font = do.call(bslib::font_collection, as.list(app_fonts()))
  )
  # Space and a soft purple shadow between the blocks of the page; the
  # profile card sits inside its output placeholder, so it gets its own
  # rule. The number tiles get less padding and a smaller number, so they
  # take little height. The 'None'
  # moderator is a real choice, so it is not greyed out. The unit under
  # each menu entry gets its own line, so it is not cut off.
  bslib::bs_add_rules(theme, "
    .bootstrap-select .dropdown-menu li a .text-muted {
      display: block; white-space: normal; font-size: 0.75rem; margin-left: 0;
    }
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
  bslib::value_box_theme(bg = app_colours()$tile, fg = app_colours()$tile_text)
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
  interactive <- plotly::layout(
    interactive, font = list(family = paste(app_fonts(), collapse = ", "))
  )
  plotly::config(interactive, displaylogo = FALSE)
}

# Remove every empty font name from a nested list of plotly layout settings,
# so the font set for the whole plot is used instead.
remove_empty_fonts <- function(settings) {
  if (!is.list(settings)) return(settings)
  if (!is.null(settings$family) && settings$family %in% c("", "''")) settings$family <- NULL
  lapply(settings, remove_empty_fonts)
}
