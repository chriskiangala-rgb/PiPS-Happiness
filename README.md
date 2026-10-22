# PiPS mood
This is the shared repository for Group 14 of the PiPS course at UvA

Current plan: A shiny app that, based on real data, compares country variables (e.g, GDP, tax rate) with their happiness score. The app will also visualise the data.

Readme structure

## 1. Student info, Project Name, Short Project Summary
## 2. Citation
## 3. Explanation of the app goal
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
