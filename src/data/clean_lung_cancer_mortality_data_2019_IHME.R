# This script prepares 2019 IHME county-level, lung cancer mortality by sex, race and age group data for analysis
# Dataset used may be downloaded from https://ghdx.healthdata.org/record/ihme-data/us-lung-cancer-county-race-ethnicity-2000-2019
# County cartographic boundary shapefiles (2024) may be downloaded from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html 


# 1. IHME Lung cancer mortality 2019

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(stringr)

# load county and census tract shapefiles
counties <- st_read("data/raw/census_boundaries/cartographic_shapefiles/2024/cb_2024_06_tract_500k.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[, 2]
colnames(counties) <- c("fips", "geometry")

# load list of counties
load("data/raw/sfba.RData")

# read in the female, male and both sexes lc cancer mortality data
lcan_b <- read_csv("data/raw/baseline_health_outcomes/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_BOTH_Y2025M06D15.csv")
lcan_b <- lcan_b[grep("California", lcan_b$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_b <- as.data.frame(lcan_b)

lcan_f <- read_csv("data/raw/baseline_health_outcomes/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_FEMALE_Y2025M06D15.csv")
lcan_f <- lcan_f[grep("California", lcan_f$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_f <- as.data.frame(lcan_f)

lcan_m <- read_csv("data/raw/baseline_health_outcomes/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_MALE_Y2025M06D15.csv")
lcan_m <- lcan_m[grep("California", lcan_m$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_m <- as.data.frame(lcan_m)

# rbind the datasets
lcan <- bind_rows(lcan_b, lcan_f, lcan_m)

rm(lcan_b, lcan_f, lcan_m)

#recode county names and limit to census tracts in the SFBA of interest
lcan$location_name <- sub(" County (California)", "", lcan$location_name, fixed = TRUE)

lcan <- lcan[lcan$location_name %in% sfba, ] 

# recode race names to ensure consistency across datasets
lcan[lcan$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
lcan[lcan$race_name == "Asian", 'race_name'] <- "Asian / Pacific Islander"
lcan[lcan$race_name == "Latino", 'race_name'] <- "Hispanic"

lcan$race_name <- factor(lcan$race_name, levels = c("American Indian / Alaskan Native", "Black", "White", "Hispanic", "Asian / Pacific Islander", "Total"))  

lcan[lcan$age_name == "All Ages", "age_name"] <- "All ages"

lcan$age_name <- factor(lcan$age_name, levels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",        
                                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",        
                                                  "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",        
                                                  "85 plus", "All ages", "Age-standardized"))  
lcan$cause_name <- "Lung cancer mortality"


lcan <- lcan[, c(2:12)]
colnames(lcan) <- c("location_name",
                    "fips",
                    "race_name",
                    "sex_name",
                    "age_name",
                    "outcome_name", 
                    "year",          
                    "metric_name",
                    "mx",
                    "uCI",          
                    "lCI"
)

lcan$fips <- str_pad(lcan$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(lcan, "data/processed/lcan_mortality_county_2019_IHME.csv", append = FALSE)


# make shapefiles
lcan <- merge(lcan, counties)

st_write(lcan, "data/processed/lcan_mortality_county_2019_IHME.shp", append = FALSE)

rm(counties, lcan)
gc()

# =============================================================================
# Clean 2019 IHME county-level lung cancer mortality
# Stratified by sex, race/ethnicity and age group
#
# Source: https://ghdx.healthdata.org/record/ihme-data/us-lung-cancer-county-race-ethnicity-2000-2019
# =============================================================================

library(tidyverse)
library(stringr)

source("config.R")

# =============================================================================
# A. Load and combine male, female and both sexes files
# =============================================================================

lcan_files <- c(
  "data/raw/baseline_health_outcomes/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_BOTH_Y2025M06D15.csv",
  "data/raw/baseline_health_outcomes/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_FEMALE_Y2025M06D15.csv",
  "data/raw/baseline_health_outcomes/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_MALE_Y2025M06D15.csv"
)

lcan <- lcan_files |>
  map(\(f) {
    read_csv(f, show_col_types = FALSE) |>
      filter(str_detect(location_name, "California")) |>
      select(fips, location_name, age_name, sex_name,
             race_name, cause_name, year, metric_name, val, upper, lower)
  }) |>
  bind_rows()

# =============================================================================
# B. Clean and recode
# =============================================================================
lcan <- lcan_files |>
  map(\(f) {
    read_csv(f, show_col_types = FALSE) |>
      filter(str_detect(location_name, "California")) |>
      select(fips, location_name, age_name, sex_name,
             race_name, cause_name, year, metric_name, val, upper, lower)
  }) |>
  bind_rows() |>
  mutate(
    location_name = str_remove(location_name, fixed(" County (California)")),
    fips = case_when(
      location_name == "California" ~ str_pad(as.character(fips), width = 2, side = "left", pad = "0"), # pad based on geography level
      TRUE                          ~ str_pad(as.character(fips), width = 5, side = "left", pad = "0")
    ),
    geolevl = if_else(location_name == "California", "state", "county"),
    race_name = recode(race_name,
                       "AIAN"   = "American Indian / Alaskan Native",
                       "Asian"  = "Asian / Pacific Islander",
                       "Latino" = "Hispanic"
    ),
    age_name   = if_else(age_name == "All Ages", "All ages", age_name),
    cause_name = "Lung cancer mortality",
    source     = "IHME",
    mx_name    = "person-year at risk",
    q_flag     = 0L,
    year       = as.character(year),
  ) |>
  filter(
    location_name %in% c(sfba_names, "California"),
    age_name != "Age-standardized"     # exclude age-standardized estimates
  ) |>
  select(
    geoid    = fips,
    geolevl = geolevl,
    lctn_nm  = location_name,
    age_grp  = age_name,
    sex_grp  = sex_name,
    race_grp = race_name,
    otcm_nm  = cause_name,
    year,
    source,
    mx_name,
    mx       = val,
    mx_lower = lower,
    mx_upper = upper,
    q_flag
  )

# =============================================================================
# C. QA checks
# =============================================================================

message(sprintf("Counties retained: %d (expected 9)", n_distinct(lcan$geoid)))
message(sprintf("Sex groups: %s", paste(unique(lcan$sex_grp), collapse = ", ")))
message(sprintf("Missing rates: %d", sum(is.na(lcan$mx))))
summary(lcan$mx)

# =============================================================================
# D. Write output
# =============================================================================

write_csv(lcan, "data/processed/lcan_mortality_county_2019_IHME.csv", append = FALSE)
rm(lcan)
gc()
