# PiPS mood
This is the shared repository for Group 14 of the PiPS course at UvA

Current plan: A shiny app that, based on real data, compares country variables (e.g, GDP, tax rate) with their happiness score. The app will also visualise the data.

Readme structure

## 1. Student info, Project Name, Short Project Summary
## 2. Citation
## 3. Explanation of the app goal
## 4. App preview or demo
## 5. Repository Structure

```
install.R                Installs the required packages (run once)
R/prepare_data.R         Loads, merges and cleans the data; creates combined_data and available_predictors
R/analyse_happiness.R    analyse_happiness(): regression of happiness on a predictor, with optional moderator
examples/examples.R      Example analyses with explanation: source("examples/examples.R")
tests/error_checks.R     Checks that the data and analysis work: source("tests/error_checks.R")
data/WHR2023.csv         World Happiness Report 2023
data/world-data-2023.csv Global country information 2023
```

Run everything with the repo root as working directory.

## 6. Dependencies

Run `source("install.R")` once; it installs any required package you do not have yet. The list of packages lives in `install.R`; add new packages there.

## 7. Step-by-step Explanation
## 8. Methods
## 9. Student Roles (Explaining what each student contributed to the project)
## 10. AI statement (Describing how and where AI was used)
## 11. References
