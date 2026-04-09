
# =============================================================================
# General Health Impact Assessment (HIA) Workflow
#
# Log-linear model:
#   y_h,i,a = m_h,i,a x P_i,a x (1 - exp(-beta_h,a x delta_x))
#
#   y       = attributable cases (deaths or prevalent cases)
#   m       = baseline rate per person (mx)
#   P       = population count per cell (geoid x age x sex x race)
#   beta    = ln(RR) / concentration increment
#   delta_x = change in pollutant concentration
#
# Inputs:
#   merged       — person-level data from exposure_population.R
#                  columns: geoid, ind_id, hsld_id, isrm, age, sex, race,
#                           NO2, BC, TotalPM25
#
#   all_outcomes — unified baseline health outcome dataframe with columns:
#                  geoid, geolevl, age_grp, sex_grp, race_grp, otcm_nm,
#                  year, source, mx_name, mx, mx_lower, mx_upper, q_flag
#
# Analysis structure:
#   PRIMARY (unstratified): uses All ages / Both sexes / Total race baseline
#   rates to focus on spatial heterogeneity in exposure as the key driver
#   of differential health impacts across census tracts.
#
#   SENSITIVITY (stratified): uses age / age+sex / age+race strata to
#   explore how demographic composition interacts with exposure heterogeneity.
#   Note: a uniform CRF is applied across all strata — stratified results
#   reflect differential baseline burden, not differential susceptibility.
#
# Primary runs (unstratified):
#   Run U01: IHME       | county | All-cause mortality       | All ages
#   Run U02: CDC Wonder | county | IHD mortality             | All ages
#   Run U03: IHME       | county | Lung cancer mortality     | All ages
#
# Sensitivity runs (stratified):
#
#   All-cause mortality (USALEEP tract, IHME county):
#     Run 01: USALEEP | tract  | age
#     Run 02: IHME    | county | age
#     Run 03: IHME    | county | age + race
#
#   Asthma — adults (CDC Places + CHIS; NO2 only):
#     Run 04: CDC Places | tract  | age (18+)
#     Run 05: CDC Places | county | age (18+)
#     Run 06: CHIS       | county | age granular (18-64, 65+)
#     Run 07: CHIS       | county | age aggregate (18+)
#     Run 08: CHIS       | state  | age granular
#     Run 09: CHIS       | state  | age aggregate (18+)
#
#   Asthma — children (CHIS only; NO2 + BC):
#     Run 10: CHIS | county | age granular (0-4, 5-17)
#     Run 11: CHIS | county | age aggregate (0-17)
#     Run 12: CHIS | state  | age granular
#     Run 13: CHIS | state  | age aggregate (0-17)
#
#   IHD mortality (CDC Wonder; NO2 + BC + PM2.5):
#     Run 14: CDC Wonder | county | age
#     Run 15: CDC Wonder | county | age + sex
#     Run 16: CDC Wonder | state  | age
#     Run 17: CDC Wonder | state  | age + sex
#
#   Lung cancer mortality (IHME; NO2 + BC + PM2.5):
#     Run 18: IHME | county | age
#     Run 19: IHME | county | age + sex
#     Run 20: IHME | county | age + race
#     Run 21: IHME | county | age + sex + race
#
#   ALRI children (HCAi; NO2 + BC):
#     Run 22: HCAi | county | age (0-17)
#     Run 23: HCAi | county | age + sex
#     Run 24: HCAi | county | age + sex + race
#
# Notes:
#   - No PM2.5 or BC RR available for adult asthma
#   - No PM2.5 RR available for ALRI or child asthma
#   - Asthma onset RRs applied to prevalence — acknowledged limitation
#   - IHD county data only available for ages 45+ (suppression below)
#   - Child CHIS county estimates sparse — state-level more reliable
#   - q_flag 2 (unstable) retained for child asthma sensitivity analyses
# =============================================================================


library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# =============================================================================
# A. Load data
# =============================================================================

all_outcomes <- read_csv("data/processed/all_baseline_outcomes.csv",
                         col_types = cols(year = col_character()))

merged <- readRDS("data/processed/exposure_population.rds")

# =============================================================================
# B. Age lookup tables
#    Maps single-year ages (0-99) to the age group labels used by each source.
#    Join key is "age" to match merged$age (single-year integer).
#    NA = age range not covered by this outcome (person excluded from that run).
#    One lookup per source; stored in named list keyed by source string.
# =============================================================================

age_lookup_usaleep <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age == 0                 ~ "<1 year",
    age >= 1  & age <= 4     ~ "1 to 4",
    age >= 5  & age <= 14    ~ "5 to 14",
    age >= 15 & age <= 24    ~ "15 to 24",
    age >= 25 & age <= 34    ~ "25 to 34",
    age >= 35 & age <= 44    ~ "35 to 44",
    age >= 45 & age <= 54    ~ "45 to 54",
    age >= 55 & age <= 64    ~ "55 to 64",
    age >= 65 & age <= 74    ~ "65 to 74",
    age >= 75 & age <= 84    ~ "75 to 84",
    age >= 85                ~ "85 to 99"
  ))

age_lookup_ihme <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age == 0                 ~ "<1 year",
    age >= 1  & age <= 4     ~ "1 to 4",
    age >= 5  & age <= 9     ~ "5 to 9",
    age >= 10 & age <= 14    ~ "10 to 14",
    age >= 15 & age <= 19    ~ "15 to 19",
    age >= 20 & age <= 24    ~ "20 to 24",
    age >= 25 & age <= 29    ~ "25 to 29",
    age >= 30 & age <= 34    ~ "30 to 34",
    age >= 35 & age <= 39    ~ "35 to 39",
    age >= 40 & age <= 44    ~ "40 to 44",
    age >= 45 & age <= 49    ~ "45 to 49",
    age >= 50 & age <= 54    ~ "50 to 54",
    age >= 55 & age <= 59    ~ "55 to 59",
    age >= 60 & age <= 64    ~ "60 to 64",
    age >= 65 & age <= 69    ~ "65 to 69",
    age >= 70 & age <= 74    ~ "70 to 74",
    age >= 75 & age <= 79    ~ "75 to 79",
    age >= 80 & age <= 84    ~ "80 to 84",
    age >= 85                ~ "85 plus"
  ))

age_lookup_cdcwonder <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age >= 25 & age <= 34    ~ "25 to 34",  # state only (suppressed at county)
    age >= 35 & age <= 44    ~ "35 to 44",  # state only (suppressed at county)
    age >= 45 & age <= 54    ~ "45 to 54",
    age >= 55 & age <= 64    ~ "55 to 64",
    age >= 65 & age <= 74    ~ "65 to 74",
    age >= 75 & age <= 84    ~ "75 to 84",
    age >= 85                ~ "85 plus",
    TRUE                     ~ NA_character_ # <25 not in IHD data
  ))

age_lookup_places <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age >= 18                ~ "18 plus",
    TRUE                     ~ NA_character_ # children excluded
  ))

# CHIS granular: non-overlapping groups used as main CHIS runs
age_lookup_chis_granular <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age >= 0  & age <= 4     ~ "0 to 4",
    age >= 5  & age <= 17    ~ "5 to 17",
    age >= 18 & age <= 64    ~ "18 to 64",
    age >= 65                ~ "65 plus"
  ))

# CHIS aggregate: matches CDC Places "18 plus" for like-for-like source comparison
age_lookup_chis_aggregate <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age >= 0  & age <= 17    ~ "0 to 17",
    age >= 18                ~ "18 plus"
  ))

age_lookup_hcai <- tibble(age = 0:99) %>%
  mutate(age_grp = case_when(
    age >= 0  & age <= 17    ~ "0 to 17",
    TRUE                     ~ NA_character_ # adults excluded from ALRI
  ))

# Named list keyed by source string as it appears in all_outcomes$source
# Special keys for CHIS variants (granular vs aggregate)
age_lookups <- list(
  "USALEEP"        = age_lookup_usaleep,
  "IHME"           = age_lookup_ihme,
  "CDC Wonder"     = age_lookup_cdcwonder,
  "CDC Places"     = age_lookup_places,
  "CHIS_granular"  = age_lookup_chis_granular,
  "CHIS_aggregate" = age_lookup_chis_aggregate,
  "HCAi"           = age_lookup_hcai
)

# =============================================================================
# C. Risk Ratio (RR) table
#    One row per exposure x outcome combination.
#    Source: HEI Special Report (see study methods for full citations)
#    Increments: 10 ug/m3 for NO2, 1 ug/m3 for BC, 5 ug/m3 for PM2.5
#    Notes:
#      - No PM2.5 or BC RR available for adult asthma
#      - No PM2.5 RR available for ALRI children
#      - Asthma onset RRs applied to prevalence — state limitation
# =============================================================================

rr_table <- tribble(
  ~exposure,   ~otcm_nm,                                   ~rr_central, ~rr_lower, ~rr_upper, ~increment,
  # All-cause mortality
  "NO2",       "All-cause mortality",                             1.04,      1.01,      1.06,         10,
  "BC",        "All-cause mortality",                             1.02,      1.00,      1.04,          1,
  "TotalPM25", "All-cause mortality",                             1.03,      1.01,      1.05,          5,
  # Ischemic heart disease mortality
  "NO2",       "Ischemic heart disease mortality",                1.05,      1.03,      1.08,         10,
  "BC",        "Ischemic heart disease mortality",                1.05,      0.99,      1.11,          1,
  "TotalPM25", "Ischemic heart disease mortality",                1.07,      1.04,      1.10,          5,
  # Lung cancer mortality
  "NO2",       "Lung cancer mortality",                           1.04,      1.01,      1.07,         10,
  "BC",        "Lung cancer mortality",                           1.02,      0.88,      1.19,          1,
  "TotalPM25", "Lung cancer mortality",                           1.06,      0.99,      1.13,          5,
  # Current asthma prevalence — adults (NO2 only; no PM2.5 or BC RR available)
  "NO2",       "Current asthma prevalence (adults)",              1.09,      1.03,      1.16,         10,
  # Current asthma prevalence — children (NO2 + BC; no PM2.5 RR available)
  "NO2",       "Current asthma prevalence (children)",            1.05,      0.99,      1.12,         10,
  "BC",        "Current asthma prevalence (children)",            1.11,      0.94,      1.31,          1,
  # ALRI children (NO2 + BC; no PM2.5 RR available)
  "NO2", "Acute lower respiratory infection (children)",          1.09,      1.03,      1.16,         10,
  "BC",  "Acute lower respiratory infection (children)",          1.30,      0.78,      2.18,          1
) %>%
  mutate(
    beta_central = log(rr_central) / increment,
    beta_lower   = log(rr_lower)   / increment,
    beta_upper   = log(rr_upper)   / increment
  )

# =============================================================================
# D. Run specifications
#    Each row defines one complete HIA run.
#    age_lookup_key: key into age_lookups list
#    strata:        sex_grp and/or race_grp columns to join on (in addition
#                   to age_grp which is always included)
#    q_flag_max:    0 = reliable only; 1 = include imputed; 2 = include unstable
#    note:          documents any analytical caveats for this run
# =============================================================================
run_specs <- tribble(
  ~run_id,  ~otcm_nm,                                        ~source,      ~geolevl, ~geo_col, ~age_lookup_key,  ~strata,                        ~q_flag_max, ~unstratified, ~label,                                              ~note,
  # ── Unstratified primary analyses ──────────────────────────────────────────
  "runU01", "All-cause mortality",                           "IHME",       "county", "geoid",  "IHME",           list(character(0)),             0L,          TRUE,          "PRIMARY: All-cause mortality | IHME | county | All ages",                 "All ages / Both / Total",
  "runU02", "Ischemic heart disease mortality",              "CDC Wonder", "county", "geoid",  "CDC Wonder",     list(character(0)),             0L,          TRUE,          "PRIMARY: IHD mortality | CDC Wonder | county | All ages",           "All ages / Both / Total",
  "runU03", "Lung cancer mortality",                         "IHME",       "county", "geoid",  "IHME",           list(character(0)),             0L,          TRUE,          "PRIMARY: Lung cancer mortality | IHME lcan | county | All ages",            "All ages / Both / Total",
  # ── Stratified sensitivity analyses ────────────────────────────────────────
  "run01",  "All-cause mortality",                           "USALEEP",    "tract",  "geoid",  "USALEEP",        list(character(0)),             0L,          FALSE,         "All-cause mortality | USALEEP | tract | age",       "Tract-level; no sex/race strata",
  "run02",  "All-cause mortality",                           "IHME",       "county", "geoid",  "IHME",           list(character(0)),             0L,          FALSE,         "All-cause mortality | IHME | county | age",                               "",
  "run03",  "All-cause mortality",                           "IHME",       "county", "geoid",  "IHME",           list("race_grp"),               0L,          FALSE,         "All-cause mortality | IHME | county | age + race",                        "",
  "run04",  "Current asthma prevalence (adults)",           "CDC Places", "tract",  "geoid",  "CDC Places",     list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CDC Places | tract | age (18+)",                    "Adults only; tract level",
  "run05",  "Current asthma prevalence (adults)",           "CDC Places", "county", "geoid",  "CDC Places",     list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CDC Places | county | age (18+)",                   "Adults only",
  "run06",  "Current asthma prevalence (adults)",           "CHIS",       "county", "geoid",  "CHIS_granular",  list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | county | age adults (granular)",             "Non-overlapping: 18-64, 65+",
  "run07",  "Current asthma prevalence (adults)",           "CHIS",       "county", "geoid",  "CHIS_aggregate", list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | county | age adults (18+ aggregate)",        "Comparable to CDC Places run05",
  "run08",  "Current asthma prevalence (adults)",           "CHIS",       "state",  "geoid",  "CHIS_granular",  list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | state | age adults (granular)",              "",
  "run09",  "Current asthma prevalence (adults)",           "CHIS",       "state",  "geoid",  "CHIS_aggregate", list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | state | age adults (18+ aggregate)",         "",
  "run10",  "Current asthma prevalence (children)",         "CHIS",       "county", "geoid",  "CHIS_granular",  list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | county | age children (granular)",           "Non-overlapping: 0-4, 5-17",
  "run11",  "Current asthma prevalence (children)",         "CHIS",       "county", "geoid",  "CHIS_aggregate", list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | county | age children (0-17 aggregate)",     "",
  "run12",  "Current asthma prevalence (children)",         "CHIS",       "state",  "geoid",  "CHIS_granular",  list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | state | age children (granular)",            "",
  "run13",  "Current asthma prevalence (children)",         "CHIS",       "state",  "geoid",  "CHIS_aggregate", list(character(0)),             0L,          FALSE,         "Current asthma prevalence (adults) | CHIS | state | age children (0-17 aggregate)",      "",
  "run14",  "Ischemic heart disease mortality",              "CDC Wonder", "county", "geoid",  "CDC Wonder",     list(character(0)),             0L,          FALSE,         "IHD mortality | CDC Wonder | county | age",                         "Age groups 45+ only",
  "run15",  "Ischemic heart disease mortality",              "CDC Wonder", "county", "geoid",  "CDC Wonder",     list("sex_grp"),                0L,          FALSE,         "IHD mortality | CDC Wonder | county | age + sex",                   "Age groups 45+ only",
  "run16",  "Ischemic heart disease mortality",              "CDC Wonder", "state",  "geoid",  "CDC Wonder",     list(character(0)),             0L,          FALSE,         "IHD mortality | CDC Wonder | state | age",                          "Age groups 25+ at state level",
  "run17",  "Ischemic heart disease mortality",              "CDC Wonder", "state",  "geoid",  "CDC Wonder",     list("sex_grp"),                0L,          FALSE,         "IHD mortality | CDC Wonder | state | age + sex",                    "",
  "run18",  "Lung cancer mortality",                         "IHME",       "county", "geoid",  "IHME",           list(character(0)),             0L,          FALSE,         "Lung cancer mortality | IHME | county | age",                               "",
  "run19",  "Lung cancer mortality",                         "IHME",       "county", "geoid",  "IHME",           list("sex_grp"),                0L,          FALSE,         "Lung cancer mortality | IHME | county | age + sex",                         "",
  "run20",  "Lung cancer mortality",                         "IHME",       "county", "geoid",  "IHME",           list("race_grp"),               0L,          FALSE,         "Lung cancer mortality | IHME | county | age + race",                        "",
  "run21",  "Lung cancer mortality",                         "IHME",       "county", "geoid",  "IHME",           list(c("sex_grp", "race_grp")), 0L,          FALSE,         "Lung cancer mortality | IHME | county | age + sex + race",                  "",
  "run22",  "Acute lower respiratory infection (children)",  "HCAi",       "county", "geoid",  "HCAi",           list(character(0)),             0L,          FALSE,         "Acute lower respiratory infection (children) | HCAi | county | age (0-17)",                        "Children only",
  "run23",  "Acute lower respiratory infection (children)",  "HCAi",       "county", "geoid",  "HCAi",           list("sex_grp"),                0L,          FALSE,         "Acute lower respiratory infection (children) | HCAi | county | age + sex",                         "Children only",
  #"run24",  "Acute lower respiratory infection (children)",  "HCAi",       "county", "geoid",  "HCAi",           list(c("sex_grp", "race_grp")), 0L,          FALSE,         "Acute lower respiratory infection (children) | HCAi | county | age + sex + race",                  "Children only"

) |>
  mutate(strata = map(strata, ~ .x[[1]]))

# =============================================================================
# E. Core HIA function
#    Called once per run x exposure combination.
#
#    Steps:
#      E1. Filter outcomes to source + geolevl + otcm_nm + q_flag
#      E2. Join age lookup to assign age_grp to each person (join key: "age")
#      E3. Determine active strata (exclude single-value columns)
#      E4. Aggregate persons to population cells
#          sex → sex_grp; race → race_grp where needed for strata join
#      E5. Join baseline rates onto population cells
#      E6. Apply log-linear HIA formula with dual uncertainty propagation
# =============================================================================

run_hia <- function(merged_data, all_outcomes, run_row, rr_row, exposure_col) {
  
  # Coerce all scalars 
  run_id      <- as.character(run_row$run_id)
  otcm_nm     <- as.character(run_row$otcm_nm)
  source      <- as.character(run_row$source)
  geolevl     <- as.character(run_row$geolevl)
  geo_col     <- as.character(run_row$geo_col)
  age_lkp_key <- as.character(run_row$age_lookup_key)
  q_flag_max  <- as.integer(run_row$q_flag_max)
  label       <- as.character(run_row$label)
  note        <- as.character(run_row$note)
  unstratified <- isTRUE(run_row$unstratified)

  # Force strata to plain character vector regardless of nesting depth
  strata <- unlist(run_row$strata, recursive = TRUE, use.names = FALSE)
  strata <- as.character(strata)
  strata <- strata[!is.na(strata)]   # remove any NA entries
  
  # --- E1. Filter outcome data ------------------------------------------------

  outcome <- all_outcomes %>%
    filter(
      otcm_nm == !!otcm_nm,
      source  == !!source,
      geolevl == !!geolevl,
      q_flag  <= q_flag_max
    )
  
  # For primary unstratified runs: filter to All ages / Both / Total
  if (unstratified) {
    outcome <- outcome %>%
      filter(age_grp == "All ages", sex_grp == "Both", race_grp == "Total")
  }

  if (nrow(outcome) == 0) {
    message(sprintf("[%s] No outcome data found — skipping.", run_id))
    return(NULL)
  }
  

  # --- E2. Assign age groups / filter valid exposures ------------------------

  age_lkp <- age_lookups[[age_lkp_key]]
  
  if (unstratified) {
    pop <- merged_data |>
      filter(!is.na(.data[[exposure_col]])) |>
      mutate(geoid = case_when(
        geolevl == "county" ~ str_sub(geoid, 1, 5),
        geolevl == "state"  ~ str_sub(geoid, 1, 2),
        TRUE                ~ geoid
      ))
    group_cols    <- geo_col
    active_strata <- character(0)
    strata_used   <- "none (All ages / Both / Total)"

  } else {
    age_lkp <- age_lookups[[age_lkp_key]]
    pop <- merged_data |>
      left_join(age_lkp, by = "age") |>
      filter(!is.na(age_grp),
             !is.na(.data[[exposure_col]])) |>
      mutate(geoid = case_when(
        geolevl == "county" ~ str_sub(geoid, 1, 5),
        geolevl == "state"  ~ str_sub(geoid, 1, 2),
        TRUE                ~ geoid
      ))
    group_cols    <- geo_col
    active_strata <- character(0)
    strata_used   <- "none (All ages / Both / Total)"


    # -- E3. Determine active strata ------------------------------
    active_strata <- if (length(strata) == 0) {
      character(0)
    } else {
      strata[sapply(strata, function(s) {
        s %in% colnames(outcome) && n_distinct(outcome[[s]]) > 1
      })]
    }
    group_cols  <- c(geo_col, "age_grp", active_strata)
    strata_used <- if (length(active_strata) == 0) "age only"
    else paste(c("age", active_strata), collapse = " + ")

    # ------ E4. Rename sex/race where needed ------------------------
    if ("sex_grp" %in% active_strata && !"sex_grp" %in% colnames(pop))
      pop <- pop |> rename(sex_grp = sex)
    if ("race_grp" %in% active_strata && !"race_grp" %in% colnames(pop))
      pop <- pop |> rename(race_grp = race)
  }


  if (nrow(pop) == 0) {
    message(sprintf("[%s] No persons matched — skipping.", run_id))
    return(NULL)
  }
  merge_group_cols <- intersect(group_cols, colnames(pop))
  
  pop_cells <- pop %>%
    group_by(across(all_of(merge_group_cols))) %>%
    summarise(
      P       = n(),
      delta_x = mean(.data[[exposure_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- E5. Join baseline rates ------------------------------------------------
  # all_outcomes$geoid contains the geography-appropriate FIPS code
  # (11-digit tract, 5-digit county, 2-digit state)
  # geo_col in merged contains the matching FIPS value for this run's geography
  outcome_slim <- outcome %>%
    rename(!!geo_col := geoid) %>%
    select(all_of(c(geo_col, "age_grp", active_strata,
                    "mx", "mx_lower", "mx_upper", "q_flag", "year"))) %>%
    rename(outcome_year = year)
  
  pop_cells <- pop_cells %>%
    left_join(outcome_slim, by = group_cols)
  
  
  # --- E6. Apply log-linear HIA formula ---------------------------------------
  #    Dual uncertainty: y_lower = low baseline rate x low RR beta
  #                      y_upper = high baseline rate x high RR beta
  result <- pop_cells %>%
    filter(!is.na(mx)) %>%
    mutate(
      af_central = 1 - exp(-rr_row$beta_central * delta_x),
      af_lower   = 1 - exp(-rr_row$beta_lower   * delta_x),
      af_upper   = 1 - exp(-rr_row$beta_upper   * delta_x),
      
      y_central  = mx       * P * af_central,
      y_lower    = mx_lower * P * af_lower,
      y_upper    = mx_upper * P * af_upper,
      
      # Run metadata
      run_id      = run_id,
      run_label   = label,
      run_note    = note,
      otcm_nm     = otcm_nm,
      source      = source,
      exposure    = exposure_col,
      geo_col     = geo_col,
      strata_used = strata_used,
      geolevl     = geolevl,   # ADD THIS
      analysis    = if_else(unstratified, "primary", "sensitivity")
    )

  if (nrow(result) == 0) {
    message(sprintf("[%s | %s] No rows after mx filter — skipping.",
                    run_id, exposure_col))
    return(NULL)
  }

  return(result)


}

# =============================================================================
# G. Execute all runs
#    Outer pmap: iterates over run_specs rows
#    Inner pmap: iterates over matching RR rows (one per exposure)
# =============================================================================

hia_results <- pmap(run_specs, function(run_id, otcm_nm, source, geolevl,
                                        geo_col, age_lookup_key, strata,
                                        q_flag_max, unstratified, label, note) {
  run_id       <- as.character(run_id)
  otcm_nm      <- as.character(otcm_nm)
  source       <- as.character(source)
  geolevl      <- as.character(geolevl)
  geo_col      <- as.character(geo_col)
  age_lkp_key  <- as.character(age_lookup_key)
  q_flag_max   <- as.integer(q_flag_max)
  unstratified <- isTRUE(unstratified)
  label        <- as.character(label)
  note         <- as.character(note)
  strata       <- strata
  
  rr_rows <- rr_table %>% filter(otcm_nm == !!otcm_nm)
  
  if (nrow(rr_rows) == 0) {
    message(sprintf("[%s] No RR found for %s — skipping.", run_id, otcm_nm))
    return(NULL)
  }
  
  message(sprintf("\n=== %s: %s ===", run_id, label))
  
  result <- pmap(rr_rows, function(exposure, beta_central, beta_lower, beta_upper, ...) {
    rr_row <- tibble(
      beta_central = beta_central,
      beta_lower   = beta_lower,
      beta_upper   = beta_upper
    )
    run_hia(
      merged_data  = merged,
      all_outcomes = all_outcomes,
      run_row      = list(
        run_id         = run_id,
        otcm_nm        = otcm_nm,
        source         = source,
        geolevl        = geolevl,
        geo_col        = geo_col,
        age_lookup_key = age_lkp_key,
        strata         = strata,
        q_flag_max     = q_flag_max,
        unstratified   = unstratified,
        label          = label,
        note           = note
      ),
      rr_row       = rr_row,
      exposure_col = exposure
    )
  }) %>% compact()
  
  # guard against empty results before flatten()
  if (length(result) == 0) {
    message(sprintf("[%s] No results returned — skipping.", run_id))
    return(NULL)
  }


  result

}) %>%
  compact() %>%
  list_flatten() %>%   # replaces flatten(); requires purrr >= 1.0.0
  bind_rows()          # combine all per-run/per-exposure data frames into one


# =============================================================================
# H. Summary tables
# =============================================================================

# --- H1. All-ages total per run x outcome x exposure x geography -------------
#    Aggregated from age-specific cells — correctly reflects study population
#    age structure rather than using a pre-aggregated "All ages" rate
hia_totals <- hia_results %>%
  group_by(run_id, run_label, run_note, otcm_nm, source, exposure,
           geo_col, strata_used, outcome_year) %>%
  summarise(
    n_cells   = n(),
    P_total   = sum(P,         na.rm = TRUE), # number of simulated population
    y_central = sum(y_central, na.rm = TRUE), # represents change from baseline health outcome
    y_lower   = sum(y_lower,   na.rm = TRUE),
    y_upper   = sum(y_upper,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  arrange(otcm_nm, exposure, run_id)

print(hia_totals %>%
        select(run_id, run_label, otcm_nm, exposure,
               y_central, y_lower, y_upper, P_total, n_cells))

# --- H2. By age group ---------------------------------------------------------
hia_by_age <- hia_results %>%
  group_by(run_id, run_label, otcm_nm, source, exposure,
           geo_col, strata_used, age_grp) %>%
  summarise(
    P         = sum(P,         na.rm = TRUE),
    y_central = sum(y_central, na.rm = TRUE),
    y_lower   = sum(y_lower,   na.rm = TRUE),
    y_upper   = sum(y_upper,   na.rm = TRUE),
    .groups   = "drop"
  )

# --- H3. By geography cell (for mapping) -------------------------------------
hia_by_geo <- hia_results %>%
  group_by(run_id, run_label, otcm_nm, source, exposure,
           geo_col, strata_used, .data[[hia_results$geo_col[1]]]) %>%
  summarise(
    P         = sum(P,         na.rm = TRUE),
    y_central = sum(y_central, na.rm = TRUE),
    y_lower   = sum(y_lower,   na.rm = TRUE),
    y_upper   = sum(y_upper,   na.rm = TRUE),
    .groups   = "drop"
  )

# --- H4. Source sensitivity comparison ---------------------------------------
#    Side-by-side totals for outcomes with multiple data sources
hia_source_comparison <- hia_totals %>%
  filter(otcm_nm %in% c(
    "All-cause mortality",
    "Current asthma prevalence (adults)",
    "Current asthma prevalence (children)"
  )) %>%
  select(run_id, run_label, otcm_nm, source, exposure,
         geo_col, y_central, y_lower, y_upper) %>%
  arrange(otcm_nm, exposure, geo_col, run_id)

print(hia_source_comparison)

# --- H5. Stratification sensitivity comparison --------------------------------
#    Shows how totals shift as strata are added within source x geography
hia_strata_comparison <- hia_totals %>%
  select(run_id, run_label, otcm_nm, source, geo_col,
         strata_used, exposure, y_central, y_lower, y_upper) %>%
  arrange(otcm_nm, source, geo_col, strata_used)

print(hia_strata_comparison)

# =============================================================================
# I. Write outputs
# =============================================================================

write_csv(hia_results,           "data/output/hia_results_runs_long.csv", append = FALSE)
write_csv(hia_totals,            "data/output/hia_totals_by_run.csv", append = FALSE)
write_csv(hia_by_age,            "data/output/hia_by_age.csv", append = FALSE)
write_csv(hia_by_geo,            "data/output/hia_by_geography.csv", append = FALSE)
write_csv(hia_source_comparison, "data/output/hia_source_sensitivity.csv", append = FALSE)
write_csv(hia_strata_comparison, "data/output/hia_strata_sensitivity.csv", append = FALSE)

message("\nHIA workflow complete.")
message(sprintf("Total runs completed: %d", n_distinct(hia_results$run_id)))
message(sprintf("Total output rows:    %d", nrow(hia_results)))
