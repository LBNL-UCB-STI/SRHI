# =============================================================================
# Combine all cleaned baseline health outcome datasets
# for input into the BREATHE health impact assessment pipeline
# =============================================================================

library(tidyverse)

source("config.R")

# =============================================================================
# A. Shared col_types spec — defined once and reused for all files
# =============================================================================

outcome_col_types <- cols(
  geoid    = col_character(),
  geolevl  = col_character(),
  lctn_nm  = col_character(),
  age_grp  = col_character(),
  sex_grp  = col_character(),
  race_grp = col_character(),
  otcm_nm  = col_character(),
  year = col_character(),
  source   = col_character(),
  mx_name  = col_character(),
  mx       = col_double(),
  mx_lower = col_double(),
  mx_upper = col_double(),
  q_flag   = col_double()
)

# =============================================================================
# B. Load all datasets
# =============================================================================

ac1   <- read_csv("data/processed/ac_mortality_county_2019_IHME.csv",              col_types = outcome_col_types)
ac2   <- read_csv("data/processed/ac_mortality_ct_tract_2020_USALEEP.csv",         col_types = outcome_col_types)
ihd   <- read_csv("data/processed/ihd_mortality_county_state_2019_CDC.csv",        col_types = outcome_col_types)
lcan  <- read_csv("data/processed/lcan_mortality_county_2019_IHME.csv",            col_types = outcome_col_types)
asth0 <- read_csv("data/processed/adult_currentasthma_ctract_county_2019_CDCPlaces.csv", col_types = outcome_col_types)
asth1 <- read_csv("data/processed/adult_currentasthma_state_county_2021_2022_CHIS.csv",  col_types = outcome_col_types)
asth2 <- read_csv("data/processed/child_currentasthma_state_county_2021_2022_CHIS.csv",  col_types = outcome_col_types)
alri  <- read_csv("data/processed/alri_county_2019_HCAI.csv",                      col_types = outcome_col_types)

# =============================================================================
# C. Combine and clean
# =============================================================================

all_outcomes <- bind_rows(ac1, ac2, ihd, lcan, asth0, asth1, asth2, alri) |>
  # drop rows with missing rates
  filter(!is.na(mx)) |>
  # standardize mx_name labels
  mutate(
    mx_name = recode(mx_name,
                     "person-year at risk"  = "deaths/person-year",
                     "person-years at risk" = "deaths/person-year",
                     "prevalence"           = "prevalence"
    )
  ) |>
  # substitute mx for missing CIs (before any unit conversion)
  mutate(
    mx_lower = if_else(is.na(mx_lower), mx, mx_lower),
    mx_upper = if_else(is.na(mx_upper), mx, mx_upper)
  ) |>
  # exclude "All ages" aggregate rows
  filter(!age_grp %in% c("Age-standardized"))


# =============================================================================
# D. QA checks
# =============================================================================

all_outcomes |>
  count(otcm_nm, source, geolevl, year, age_grp) |>
  print(n = Inf)

message(sprintf("Total rows: %d", nrow(all_outcomes)))
message(sprintf("Missing mx: %d", sum(is.na(all_outcomes$mx))))
message(sprintf("Missing mx_lower: %d", sum(is.na(all_outcomes$mx_lower))))
message(sprintf("Missing mx_upper: %d", sum(is.na(all_outcomes$mx_upper))))

# check prevalence ranges — values should be 0-1 (proportions) not 0-100
all_outcomes |>
  filter(mx_name == "prevalence") |>
  summarize(min = min(mx), max = max(mx), mean = mean(mx))

all_outcomes |>
  count(otcm_nm, source, geolevl, year, age_grp) |>
  print(n = Inf)

message(sprintf("Total rows: %d", nrow(all_outcomes)))
message(sprintf("Missing mx: %d", sum(is.na(all_outcomes$mx))))
message(sprintf("Missing mx_lower: %d", sum(is.na(all_outcomes$mx_lower))))
message(sprintf("Missing mx_upper: %d", sum(is.na(all_outcomes$mx_upper))))

# =============================================================================
# E. Write output
# =============================================================================

write_csv(all_outcomes, "data/processed/all_baseline_outcomes.csv", append = FALSE)
rm(ac1, ac2, asth0, asth1, asth2, ihd, lcan, alri)
gc()
