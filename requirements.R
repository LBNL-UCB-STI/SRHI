#!/usr/bin/env Rscript
# requirements.R
# This script installs all R packages used in the scanned project.
# Run it with: Rscript requirements.R 2>&1 | tee install.log

options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages(c("stringr", "dplyr", "vroom", "httr", "tidyverse", "leaflet", "RColorBrewer", "tigris", "sf", "tibble", "jsonlite", "tidyr", "purrr", "readr", "ggplot2"))

