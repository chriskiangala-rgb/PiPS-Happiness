# World map of the result of analyse_happiness() ----

#' Draw a world map coloured by happiness, the predictor or the residuals
#'
#' Only the countries in the analysis are coloured; all other land is grey.
#' The residual layer shows how much happier (blue) or unhappier (red) each
#' country is than the model predicts, so regional patterns the model misses
#' become visible.
#'
#' @param result The list returned by analyse_happiness().
#' @param data The data that was passed to analyse_happiness(); its row
#'   numbers link the analysed countries back to their names.
#' @param layer "happiness", "predictor" or "residual".
#' @param highlight Optional country name to outline on the map.
#'
#' @return A plotly map.
map_happiness <- function(result, data, layer = c("happiness", "predictor", "residual"),
                          highlight = "") {
  layer <- match.arg(layer)
  rows <- as.integer(rownames(result$model_data))
  countries <- data[["Country name"]][rows]
  codes <- country_codes(data)[rows]
  happiness <- data$happiness[rows]
  # Taken from the data, not the model, so the map shows the original units.
  predictor <- data[[result$primary]][rows]
  residual <- stats::residuals(result$model)

  hover <- paste0(
    "<b>", countries, "</b>",
    "<br>Happiness: ", sprintf("%.2f", happiness),
    "<br>", result$primary, ": ", format_map_value(predictor),
    "<br>Compared with the model: ", sprintf("%+.2f", residual)
  )

  map <- plotly::plot_ly()
  if (layer == "predictor" && !is.numeric(predictor)) {
    map <- add_group_layers(map, codes, predictor, hover)
  } else {
    values <- switch(layer, happiness = happiness, predictor = predictor, residual = residual)
    title <- switch(layer,
      happiness = "Happiness",
      predictor = result$primary,
      residual = "Compared with the model"
    )
    map <- plotly::add_trace(
      map,
      type = "choropleth",
      locations = codes,
      z = values,
      text = hover,
      hovertemplate = "%{text}<extra></extra>",
      # Residuals: red (unhappier than predicted) - white - blue (happier).
      # Other layers: light lavender to dark purple, matching the app.
      colorscale = if (layer == "residual") {
        list(c(0, "#B2182B"), c(0.5, "#F7F7F7"), c(1, "#2166AC"))
      } else {
        list(c(0, "#EDE5F7"), c(1, "#3D2766"))
      },
      zmid = if (layer == "residual") 0,
      marker = list(line = list(color = "white", width = 0.5)),
      colorbar = list(title = list(text = paste(strwrap(title, 14), collapse = "<br>")), len = 0.7)
    )
  }

  if (!identical(highlight, "") && highlight %in% data[["Country name"]]) {
    map <- add_highlight(map, country_codes(data)[data[["Country name"]] == highlight])
  }

  plotly::layout(
    map,
    geo = list(
      projection = list(type = "natural earth"),
      showframe = FALSE,
      showcoastlines = FALSE,
      showland = TRUE,
      landcolor = "#D9D9D9",
      showcountries = TRUE,
      countrycolor = "white",
      lataxis = list(range = c(-58, 85))
    ),
    legend = list(orientation = "h"),
    margin = list(l = 0, r = 0, t = 0, b = 0)
  )
}

#' Explain the map in a few sentences
#'
#' Always says what grey means and that large countries dominate visually.
#' For the residual layer it adds Moran's I: are neighbouring countries more
#' alike than the model predicts? If so, the countries are not independent
#' and the model's p-values are likely too small.
#'
#' @inheritParams map_happiness
#'
#' @return A character vector with one sentence per element.
map_note <- function(result, data, layer) {
  sentences <- c(
    "Grey countries are not in this analysis (no data).",
    "Large countries stand out on a map, but every country counts once in the analysis."
  )
  if (layer != "residual") return(sentences)

  rows <- as.integer(rownames(result$model_data))
  clustering <- morans_i(
    stats::residuals(result$model), data$Latitude[rows], data$Longitude[rows]
  )
  c(
    "Blue countries are happier than the model predicts, red countries unhappier.",
    sprintf(
      "Neighbouring countries %s (Moran's I = %.2f, p %s, %d countries).",
      if (clustering$p_value < 0.05) {
        "are more alike than the model predicts"
      } else {
        "are not clearly more alike than the model predicts"
      },
      clustering$statistic, format_p(clustering$p_value, with_sign = TRUE), clustering$n
    ),
    if (clustering$p_value < 0.05) {
      "So the countries are not fully independent, and the p-values of the model are likely too small."
    },
    sentences
  )
}

#' Moran's I: are nearby countries more alike than distant ones?
#'
#' Uses inverse great-circle distance between country centres as weights
#' and the normal approximation for the p-value. The test is one-sided:
#' it asks whether nearby countries are more alike, not less.
#'
#' @param values Numeric values per country, e.g. model residuals.
#' @param latitude,longitude Country coordinates in degrees, same order.
#'
#' @return A list with the statistic, the one-sided p-value and the number
#'   of countries with coordinates.
morans_i <- function(values, latitude, longitude) {
  usable <- is.finite(values) & is.finite(latitude) & is.finite(longitude)
  values <- values[usable]
  n <- length(values)
  if (n < 3L) stop("Moran's I needs at least 3 countries with coordinates.")

  latitude <- latitude[usable] * pi / 180
  longitude <- longitude[usable] * pi / 180
  # Distance along the earth's surface, in km. pmin guards against
  # rounding just above 1, which acos() cannot take.
  cosine <- outer(sin(latitude), sin(latitude)) +
    outer(cos(latitude), cos(latitude)) * cos(outer(longitude, longitude, "-"))
  distance <- 6371 * acos(pmin(cosine, 1))
  weights <- 1 / distance
  weights[!is.finite(weights)] <- 0  # the country itself, and shared centres
  diag(weights) <- 0

  deviation <- values - mean(values)
  s0 <- sum(weights)
  statistic <- n / s0 * sum(weights * outer(deviation, deviation)) / sum(deviation^2)

  s1 <- sum((weights + t(weights))^2) / 2
  s2 <- sum((rowSums(weights) + colSums(weights))^2)
  expected <- -1 / (n - 1)
  variance <- (n^2 * s1 - n * s2 + 3 * s0^2) / ((n^2 - 1) * s0^2) - expected^2

  list(
    statistic = statistic,
    p_value = stats::pnorm((statistic - expected) / sqrt(variance), lower.tail = FALSE),
    n = n
  )
}

# Helpers ----

# Three-letter country codes, which plotly uses to find each country.
# Uses the iso3 column when prepare_data.R provides one.
country_codes <- function(data) {
  if ("iso3" %in% names(data)) return(data$iso3)
  countrycode::countrycode(
    data[["Country name"]], origin = "country.name", destination = "iso3c",
    custom_match = c("Kosovo" = "XKX"), warn = FALSE
  )
}

# One layer per group, so a categorical predictor gets a legend entry per
# group in the colour-blind friendly palette the plot also uses.
add_group_layers <- function(map, codes, groups, hover) {
  groups <- factor(groups)
  colours <- thematic::okabe_ito(nlevels(groups))
  for (i in seq_len(nlevels(groups))) {
    in_group <- groups == levels(groups)[i]
    map <- plotly::add_trace(
      map,
      type = "choropleth",
      locations = codes[in_group],
      z = rep(1, sum(in_group)),
      text = hover[in_group],
      hovertemplate = "%{text}<extra></extra>",
      colorscale = list(c(0, colours[i]), c(1, colours[i])),
      showscale = FALSE,
      name = levels(groups)[i],
      showlegend = TRUE,
      marker = list(line = list(color = "white", width = 0.5))
    )
  }
  map
}

# A thick outline around one country, with a transparent fill.
add_highlight <- function(map, code) {
  plotly::add_trace(
    map,
    type = "choropleth",
    locations = code,
    z = 1,
    colorscale = list(c(0, "rgba(0,0,0,0)"), c(1, "rgba(0,0,0,0)")),
    showscale = FALSE,
    showlegend = FALSE,
    hoverinfo = "skip",
    marker = list(line = list(color = "#1A1A1A", width = 2.5))
  )
}

format_map_value <- function(values) {
  if (is.numeric(values)) format(signif(values, 3), big.mark = ",", trim = TRUE) else as.character(values)
}
