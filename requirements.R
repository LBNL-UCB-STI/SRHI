#!/usr/bin/env Rscript
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages(c(
  "RColorBrewer", "dplyr", "ggplot2", "httr", "jsonlite", "leaflet",
  "purrr", "readr", "sf", "stringr", "tibble", "tidyr", "tidyverse",
  "tigris", "vroom"
))


# missing files:
# data/raw/simulated_population/sfbay-tr_capacity_1_5-20230608_activitysim_data_persons.csv