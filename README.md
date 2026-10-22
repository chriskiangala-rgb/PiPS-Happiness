# PiPS Happiness

## 1. Student Info and Project Summary
### A Project by:
Diemithry Kloppenburg, Tian Lin & Christian Kiangala
## 2. Citation
Kloppenburg, D., Lin, T., & Kiangala, C. B. (2026, October 22). PiPS-Happiness. Github.com. Retrieved October 22, 2026, from https://github.com/chriskiangala-rgb/PiPS-Happiness
## 3. Explanation of the App Goal

Why are people happier in some countries than in others? Many country characteristics go together with happiness, such as wealth, health, social support and freedom. These characteristics also go together with each other. The app lets you explore these associations yourself, without writing code.

**Who it's for:** students and researchers in psychology who want to explore which country characteristics go together with happiness, and get ideas for a research question. It is also a teaching tool: you can see what a regression, a moderator or a control variable does to a result.

**What you can do:**
- **Choose a predictor** from 37 variables: those of the World Happiness Report 2023 and the Global Country Information Dataset 2023, plus each country's continent. Then see how strongly it goes together with happiness.
- **Add a moderator** to test whether the association differs between groups, for example between continents.
- **Add up to 3 control variables** to see whether the association remains between countries that are equal on them.
- **Select continents** to compare, and choose the reference continent.
- **Highlight a country** to see where it stands and how much happier or unhappier it is than the model predicts.

**How the results are shown:** a world map, an interactive plot (hover a point to see the country), model checks (residuals, influential countries and overlap between variables), and a summary in plain language next to the coefficient table.

**What it is not:** a way to draw conclusions. Every analysis compares countries, not individual people, and shows associations, not causes. With this many possible combinations, some results will be significant by chance. The app says this in its sidebar, and the model checks show when a model's assumptions are doubtful. Use the app to find patterns worth testing, not to confirm them.
## 4. App Preview

The app opens on a world map of happiness. The tiles at the top show the effect of the chosen predictor, the variance explained and the number of countries analysed.

![The app's Map tab: a world map coloured by happiness, with Social support as predictor](images/app_map.png)

With a moderator, the Plot tab shows a separate line per group. Here the effect of social support differs per continent, and the Netherlands is highlighted with its own profile above the plot.

![The app's Plot tab: happiness against social support with a line per continent and the Netherlands highlighted](images/app_plot.png)
## 5. Repository Structure

```
main.Rmd                 Main deliverable: run its app chunk to start the app
install.R                Installs the required packages (run once)
R/prepare_data.R         Loads, merges and cleans the data; creates combined_data and available_predictors
R/analyse_happiness.R    analyse_happiness(): regression of happiness on a predictor, with optional moderator, control variables and continent selection
R/plot_happiness.R       plot_happiness(): plot of the data with the fitted model
R/report_happiness.R     Readable coefficient table, plain-language summary and country profile of a result
R/app/happiness_app.R    happiness_app(): the Shiny app
R/app/map_happiness.R    World map of happiness, the predictor or the residuals, with Moran's I
R/app/check_model.R      Model checks: residual plots, influential countries and VIF
R/app/predictor_groups.R Groups the predictors by theme for the app's menus
examples/examples.R      Example analyses with explanation: source("examples/examples.R")
tests/error_checks.R     Checks that the data and analysis work: source("tests/error_checks.R")
tests/app_checks.R       Checks that the app and its outputs work: source("tests/app_checks.R")
data/WHR2023.csv         World Happiness Report 2023
data/world-data-2023.csv Global country information 2023
images/                  Screenshots of the app for this README
```

Run everything with the repo root as working directory.

## 6. Dependencies

Run `source("install.R")` once; it installs any required package you do not have yet. The list of packages lives in `install.R`; add new packages there.

## 7. Step-by-step Explanation

## 8. Methods

We regress each country’s happiness—the **World Happiness Report 2023 ladder score**—on one predictor at a time, optionally including a moderator and control variables. The number of countries included depends on the variables selected and the available data.

- **Data transformations:** When data cleanup is enabled, country totals are converted to per-person values and skewed variables are log2-transformed. This helps distinguish relative differences between countries from differences in country size.
- **Statistical inference:** All inference uses HC3 heteroskedasticity-consistent standard errors (Long & Ervin, 2000). Moderation is assessed using a joint Wald test with the same covariance matrix.
- **Small groups:** Categories of a predictor or moderator containing fewer than five countries are excluded from that analysis because estimates for these groups may be unreliable.
- **Nonlinear relationships:** For every numeric predictor, we test whether adding squared and cubed terms improves the fit compared with a straight-line relationship.

### 1. Assumption checks

We checked the regression assumptions for **[number checked] predictors**.

- **Unequal residual variance:** The Breusch–Pagan test flagged unequal residual variance for [number flagged] predictors. HC3 robust standard errors were at least 20% larger than conventional standard errors for [number with larger SEs] predictors, reaching [largest ratio] times the conventional estimate for [predictor]. We therefore use HC3 by default rather than selecting it based on a preliminary test. HC3 changed the statistical significance conclusion for [number changed] of [number checked] predictors.
- **Residual normality:** Residuals deviated from normality for [number flagged] predictors. Models included [minimum sample size] to [maximum sample size] countries. Larger samples can reduce concerns about non-normality, although outliers and influential observations may still affect inference.
- **Linearity:** A straight line was a reasonable summary for [number adequately summarised] of [number numeric] numeric predictors. For [number misleading] predictors, the relationship was clearly curved or non-monotonic, so a linear slope alone did not adequately describe it. For example, for [predictor], a linear model explained [linear percentage]% of the variation in happiness, compared with [curve percentage]% for a model including squared and cubed terms.
- **Spatial dependence:** For social support, residuals from nearby countries were similar (Moran’s I = [value], p = [value]). After controlling for continent, Moran’s I decreased to [value] (p = [value]), indicating [smaller but still significant / no longer statistically significant] spatial dependence. HC3 standard errors do not account for this dependence.
- **Continent-clustered inference:** With standard errors clustered by continent and t-tests using five degrees of freedom, the association between social support and happiness remained statistically significant (p = [value]). Because there are few continent clusters, this result should be interpreted cautiously.

### 2. Limitations

- **Countries, not individuals:** Results describe differences between countries. They cannot establish why individual people are happier; drawing individual-level conclusions from country-level associations risks the ecological fallacy.
- **Associations, not causes:** The data are cross-sectional and do not establish causality. For example, social support may contribute to happiness, but happier countries may also develop stronger social support.
- **Omitted variables:** Culture, history, institutions and other unmeasured factors may explain both a predictor and happiness.
- **Countries are not independent:** Nearby countries have similar residuals, as indicated by Moran’s I. Controlling for continent reduces this dependence, but any remaining dependence may affect standard errors and p-values. HC3 does not account for spatial dependence; spatial regression is a possible next step.
- **Nonlinear relationships:** For [number of predictors] predictors, the relationship is clearly curved or non-monotonic. Their linear slopes alone do not adequately describe the relationship.
- **Measurement and missing data:** The country-level variables come from a Kaggle dataset combining several sources and years, without source information for each variable. Some variables have substantial missing data. For example, minimum-wage data are available for [number with data] of [total countries] countries. Missing data may affect which countries are represented and introduce selection bias.
- **Exploratory analysis:** Testing many predictors and combinations increases the chance of statistically significant findings occurring by chance. Results should be treated as hypotheses for further testing rather than definitive conclusions.
  
## 9. Student Roles
Diemithry (...)  
Tian (...)  
Christian was the project leader and was responsible for the README and the repository structure. He also created an error check file and reviewed code.
## 10. AI statement
Diemithry (...)  
Tian (...)  
Christian used LLMs (UvA AI Chat and Claude) to help write and review code and to get feedback on the repository and README structure. 
## 11. References
Elgiriyewithana, N. (2023). Global Country Information Dataset 2023. https://www.kaggle.com/. Retrieved October 22, 2026, from https://www.kaggle.com/datasets/nelgiriyewithana/countries-of-the-world-2023  
Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent standard errors in the linear regression model. The American Statistician, 54(3), 217. https://doi.org/10.1080/00031305.2000.10474549  
Singh, A. (2023). World Happiness Report 2023. Retrieved October 16, 2026, from https://www.kaggle.com/datasets/ajaypalsinghlo/world-happiness-report-2023
