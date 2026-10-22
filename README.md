# PiPS mood
This is the shared repository for Group 14 of the PiPS course at UvA

Current plan: A shiny app that, based on real data, compares country variables (e.g, GDP, tax rate) with their happiness score. The app will also visualise the data.

Readme structure

## 1. Student info, Project Name, Short Project Summary
## 2. Citation
## 3. Explanation of the app goal

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
## 4. App preview or demo

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
## 9. Student Roles (Explaining what each student contributed to the project)
## 10. AI statement (Describing how and where AI was used)
## 11. References
