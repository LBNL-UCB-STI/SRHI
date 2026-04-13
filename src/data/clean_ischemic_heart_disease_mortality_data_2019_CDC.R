# =============================================================================
# Clean 2019 CDC Wonder IHD mortality data
# State and county level, stratified by age, sex and race/ethnicity
#
# Sources:
#   State by age:          http://wonder.cdc.gov/controller/saved/D76/D474F974
#   County by age/sex/race: http://wonder.cdc.gov/controller/saved/D76/D476F903
#
# q_flag:
#   0 = CDC estimate (reliable)
#   1 = suppressed death count (1-9), imputed as 5 — CANNOT BE REPORTED
#   2 = death count < 20, rate flagged as unreliable — use with caution
#
# Note: q_flag 1 and 2 rows retained with imputed rates for sensitivity
#       analyses but excluded from main analysis outputs
# =============================================================================

library(tidyverse)
library(stringr)

source("config.R")

# =============================================================================
# A. Shared cleaning function
# =============================================================================

clean_cdc_wonder <- function(path,
                             geolevl,
                             keep_rows    = NULL,
                             keep_cols,
                             has_hispanic = FALSE,
                             fixed_sex    = NULL,
                             fixed_race   = NULL,
                             fixed_age    = NULL,   # add this
                             keep_qflags  = 0L) {

  raw <- read_csv(path, show_col_types = FALSE)

  # limit rows — either by index or by filtering metadata (NA in County/State col)
  if (!is.null(keep_rows)) {
    raw <- raw[keep_rows, ]
  } else {
    raw <- raw |> filter(!is.na(raw[[1]]))
  }

  # select and rename columns
  raw <- raw |> select(all_of(keep_cols))

  # recode Hispanic origin into race column
  if (has_hispanic) {
    raw <- raw |>
      mutate(race_grp = if_else(`Hispanic Origin` == "Hispanic or Latino",
                                "Hispanic", race_grp)) |>
      select(-`Hispanic Origin`)
  }

  # add fixed columns where not stratified
  if (!is.null(fixed_age))  raw <- raw |> mutate(age_grp  = fixed_age)
  if (!is.null(fixed_sex))  raw <- raw |> mutate(sex_grp  = fixed_sex)
  if (!is.null(fixed_race)) raw <- raw |> mutate(race_grp = fixed_race)

  raw |>
    filter(!Population %in% c("Not Applicable", "Suppressed")) |>
    # coerce all potentially mixed-type columns to character
    # (some files read as dbl when no suppressed values present)
    mutate(
      across(c(Deaths, `Crude Rate`,
               `Crude Rate Lower 95% Confidence Interval`,
               `Crude Rate Upper 95% Confidence Interval`),
             as.character)
    ) |>
    mutate(
      `Crude Rate Lower 95% Confidence Interval` = if_else(
        `Crude Rate Lower 95% Confidence Interval` %in% c("Suppressed", "Not Applicable", "Unreliable"),
        NA_character_, `Crude Rate Lower 95% Confidence Interval`
      ),
      `Crude Rate Upper 95% Confidence Interval` = if_else(
        `Crude Rate Upper 95% Confidence Interval` %in% c("Suppressed", "Not Applicable", "Unreliable"),
        NA_character_, `Crude Rate Upper 95% Confidence Interval`
      )
    ) |>
    mutate(
      Population = as.numeric(Population),
      Deaths     = as.numeric(if_else(Deaths == "Suppressed", "5", Deaths)),
      # quality flag
      q_flag = case_when(
        Deaths == 5 & is.na(as.numeric(
          if_else(`Crude Rate` %in% c("Unreliable", "Suppressed"),
                  NA_character_, `Crude Rate`)))               ~ 1L,
        `Crude Rate` == "Unreliable"                           ~ 2L,
        TRUE                                                   ~ 0L
      ),

      # impute rates for suppressed/unreliable (for sensitivity analysis)
      `Crude Rate` = case_when(
        `Crude Rate` %in% c("Unreliable", "Suppressed") ~ as.character(Deaths / Population * 100000),
        TRUE ~ `Crude Rate`
      ),
      `Crude Rate` = as.numeric(`Crude Rate`),
      # impute CIs for suppressed counts
      `Crude Rate Lower 95% Confidence Interval` = case_when(
        q_flag == 1 ~ as.numeric(1 / Population * 100000),
        TRUE ~ as.numeric(`Crude Rate Lower 95% Confidence Interval`)
      ),
      `Crude Rate Upper 95% Confidence Interval` = case_when(
        q_flag == 1 ~ as.numeric(9 / Population * 100000),
        TRUE ~ as.numeric(`Crude Rate Upper 95% Confidence Interval`)
      ),
      # convert per 100k → per person-year
      mx       = `Crude Rate`                                    / 100000,
      mx_lower = `Crude Rate Lower 95% Confidence Interval`      / 100000,
      mx_upper = `Crude Rate Upper 95% Confidence Interval`      / 100000,
      # constant columns
      geolevl  = geolevl,
      otcm_nm  = "Ischemic heart disease mortality",
      year     = "2019",
      source   = "CDC Wonder",
      mx_name  = "person-years at risk"
    ) |>
    filter(q_flag %in% keep_qflags) |>
    mutate(geoid = str_pad(as.character(geoid), width = ifelse(geolevl == "state", 2, 5),
                           side = "left", pad = "0")) |>
    select(geoid, geolevl, lctn_nm, age_grp, sex_grp, race_grp,
           otcm_nm, year, source, mx_name, mx, mx_lower, mx_upper, q_flag)
}

# =============================================================================
# B. Load and clean each dataset
# =============================================================================

# State — age only
st_a <- clean_cdc_wonder(
  path       = "data/raw/baseline_health_outcomes/ihd_mortality_state_2019_CDC_age.csv",
  geolevl    = "state",
  keep_rows  = c(1:11),
  keep_cols  = c(geoid = "State Code", lctn_nm = "State",
                 age_grp = "Ten-Year Age Groups",
                 Deaths = "Deaths", Population = "Population",
                 "Crude Rate",
                 "Crude Rate Lower 95% Confidence Interval",
                 "Crude Rate Upper 95% Confidence Interval"),
  fixed_sex  = "Both", fixed_race = "Total", keep_qflags = 0L
)

# State — age + sex
st_s <- clean_cdc_wonder(
  path       = "data/raw/baseline_health_outcomes/ihd_mortality_state_2019_CDC_age_sex.csv",
  geolevl    = "state",
  keep_rows  = 1:22,
  keep_cols  = c(geoid = "State Code", lctn_nm = "State",
                 age_grp = "Ten-Year Age Groups", sex_grp = "Sex",
                 Deaths = "Deaths", Population = "Population",
                 "Crude Rate",
                 "Crude Rate Lower 95% Confidence Interval",
                 "Crude Rate Upper 95% Confidence Interval"),
  fixed_race = "Total", keep_qflags = 0L
)

# State — age + sex + race
st_ihd <- clean_cdc_wonder(
  path         = "data/raw/baseline_health_outcomes/ihd_mortality_state_2019_CDC_age_sex_race.csv",
  geolevl      = "state",
  keep_rows    = 1:288,
  keep_cols    = c(geoid = "State Code", lctn_nm = "State",
                   age_grp = "Ten-Year Age Groups", sex_grp = "Sex",
                   race_grp = "Race", "Hispanic Origin",
                   Deaths = "Deaths", Population = "Population",
                   "Crude Rate",
                   "Crude Rate Lower 95% Confidence Interval",
                   "Crude Rate Upper 95% Confidence Interval"),
  has_hispanic = TRUE, keep_qflags = 0L
)

# County — age + sex (no race)
ihdb <- clean_cdc_wonder(
  path       = "data/raw/baseline_health_outcomes/ihd_mortality_county_2019_CDC_age_sex.csv",
  geolevl    = "county",
  keep_rows  = 1:216,
  keep_cols  = c(geoid = "County Code", lctn_nm = "County",
                 age_grp = "Ten-Year Age Groups", sex_grp = "Sex",
                 Deaths = "Deaths", Population = "Population",
                 "Crude Rate",
                 "Crude Rate Lower 95% Confidence Interval",
                 "Crude Rate Upper 95% Confidence Interval"),
  fixed_race = "Total", keep_qflags = 0L
) |> mutate(lctn_nm = str_remove(lctn_nm, " County, CA"))

# County — age only
ihdc <- clean_cdc_wonder(
  path       = "data/raw/baseline_health_outcomes/ihd_mortality_county_2019_CDC_age.csv",
  geolevl    = "county",
  keep_rows  = 1:108,
  keep_cols  = c(geoid = "County Code", lctn_nm = "County",
                 age_grp = "Ten-Year Age Groups",
                 Deaths = "Deaths", Population = "Population",
                 "Crude Rate",
                 "Crude Rate Lower 95% Confidence Interval",
                 "Crude Rate Upper 95% Confidence Interval"),
  fixed_sex  = "Both", fixed_race = "Total", keep_qflags = 0L
) |> mutate(lctn_nm = str_remove(lctn_nm, " County, CA"))

# County — age + sex + race
ihda <- clean_cdc_wonder(
  path         = "data/raw/baseline_health_outcomes/ihd_mortality_county_2019_CDC.csv",
  geolevl      = "county",
  keep_rows    = 1:2592,
  keep_cols  = c(geoid = "County Code", lctn_nm = "County",
                 age_grp = "Ten-Year Age Groups", sex_grp = "Sex",
                 race_grp = "Race", "Hispanic Origin",
                 Deaths = "Deaths", Population = "Population",
                 "Crude Rate",
                 "Crude Rate Lower 95% Confidence Interval",
                 "Crude Rate Upper 95% Confidence Interval"),
  has_hispanic = TRUE, keep_qflags = 0L
) |> mutate(lctn_nm = str_remove(lctn_nm, " County, CA"))

# County - unstratified
ihd_total <- clean_cdc_wonder(
  path        = "data/raw/baseline_health_outcomes/ihd_mortality_county_2019_CDC_all_ages.csv",
  geolevl     = "county",
  keep_rows   = 1:9,
  keep_cols   = c(geoid = "County Code", lctn_nm = "County",
                  Deaths = "Deaths", Population = "Population",
                  "Crude Rate",
                  "Crude Rate Lower 95% Confidence Interval",
                  "Crude Rate Upper 95% Confidence Interval"),
  fixed_age   = "All ages",
  fixed_sex   = "Both",
  fixed_race  = "Total",
  keep_qflags = 0L
) |>
  mutate(lctn_nm = str_remove(lctn_nm, " County, CA"))
# =============================================================================
# C. Combine and recode age groups
# =============================================================================

ihd <- bind_rows(st_a, st_s, st_ihd, ihda, ihdb, ihdc, ihd_total) |>
  mutate(
    age_grp = recode(age_grp,
                     "25-34 years" = "25 to 34",
                     "35-44 years" = "35 to 44",
                     "45-54 years" = "45 to 54",
                     "55-64 years" = "55 to 64",
                     "65-74 years" = "65 to 74",
                     "75-84 years" = "75 to 84",
                     "85+ years"   = "85 plus",
                     "All ages"    = "All ages"
    )
  )

# =============================================================================
# D. QA checks
# =============================================================================

message(sprintf("Total rows: %d", nrow(ihd)))
message(sprintf("State rows: %d", sum(ihd$geolevl == "state")))
message(sprintf("County rows: %d", sum(ihd$geolevl == "county")))
message(sprintf("Missing rates: %d", sum(is.na(ihd$mx))))
summary(ihd$mx)

# =============================================================================
# E. Write output
# =============================================================================

write_csv(ihd, "data/processed/ihd_mortality_county_state_2019_CDC.csv", append = FALSE)
rm(st_a, st_s, st_ihd, ihda, ihdb, ihdc, ihd)
gc()
