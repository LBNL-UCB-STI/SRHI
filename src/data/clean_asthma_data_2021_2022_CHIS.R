# =============================================================================
# Clean California Health Interview Survey (CHIS) 2021-2022 asthma prevalence
# Adult (18+) and child (0-17) current asthma prevalence by county
#
# Source: CHIS via CKAN API
# Resource ID: a440b99b-ccc6-473c-bea1-2baf36b05dbe
#
# q_flag: 0 = reliable, 1 = missing (unreliable), 2 = statistically unstable
# Child county estimates are often unstable or missing — state-level more reliable
# Adult estimates: keep q_flag == 0 only
# Child estimates: keep q_flag 0 and 2 (unstable retained for sensitivity analyses)
# =============================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(sf)

source("config.R")

# =============================================================================
# A. Load county FIPS lookup
# =============================================================================

fips <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbacounty_cleaned.shp") |>
  st_drop_geometry() |>
  select(geoid = 1, lctn_nm = 2)

# =============================================================================
# B. CKAN download function
# =============================================================================

fetch_all_ckan <- function(resource_id, query, limit = 500) {
  base_url    <- "https://data.chhs.ca.gov/api/3/action/datastore_search"
  offset      <- 0
  all_records <- list()
  
  repeat {
    url <- paste0(
      base_url,
      "?resource_id=", resource_id,
      "&q=",           URLencode(query),
      "&limit=",       limit,
      "&offset=",      offset
    )
    
    parsed  <- GET(url) |> content(as = "parsed", type = "application/json")
    records <- parsed$result$records

    if (length(records) == 0) break
    all_records[[length(all_records) + 1]] <- records
    offset <- offset + limit
  }

  bind_rows(all_records)
}

# =============================================================================
# C. Download CHIS data
# =============================================================================

message("Downloading CHIS 2021-2022 asthma data...")
chis <- fetch_all_ckan("a440b99b-ccc6-473c-bea1-2baf36b05dbe", "2021-2022")
message(sprintf("Downloaded %d rows.", nrow(chis)))

# =============================================================================
# D. Shared cleaning function
# =============================================================================

clean_chis_asthma <- function(data, age_recode, keep_qflags, otcm_suffix = "") {

  sfba_plus_state <- c(sfba_names, "California")

  data |>
    filter(COUNTY %in% sfba_plus_state) |>
    mutate(
      ci_raw  = str_replace_all(`95% CONFIDENCE INTERVAL`, "\u2013", "-"),
      mx_lower = str_match(ci_raw, "([0-9.]+)-([0-9.]+)")[, 2] |> as.numeric(),
      mx_upper = str_match(ci_raw, "([0-9.]+)-([0-9.]+)")[, 3] |> as.numeric(),
      mx       = as.numeric(`CURRENT PREVALENCE`),
      age_raw  = str_replace_all(`AGE GROUP`, "\u2013", "-")
    ) |>
    filter(age_raw %in% names(age_recode)) |>   # filter BEFORE recode
    mutate(
      age_grp  = recode(age_raw, !!!age_recode),
      q_flag = case_when(
        is.na(COMMENT)                                                    ~ 0L,
        COMMENT == "Prevalence not available due to unreliable estimate"  ~ 1L,
        str_detect(COMMENT, "statistically unstable")                     ~ 2L,
        TRUE                                                              ~ 0L
      ),
      otcm_nm  = paste0("Current asthma prevalence", otcm_suffix),
      race_grp = "Total",
      sex_grp  = "Both",
      source   = "CHIS",
      mx_name  = "prevalence",
      geolevl  = if_else(COUNTY == "California", "state", "county"),
      year     = as.character(YEARS)
    ) |>
    filter(q_flag %in% keep_qflags) |>
    left_join(fips, by = c("COUNTY" = "lctn_nm")) |>
    mutate(
      geoid = if_else(COUNTY == "California", "06", paste0("06", geoid))
    ) |>
    select(
      geoid, geolevl, lctn_nm = COUNTY, age_grp, sex_grp, race_grp,
      otcm_nm, year, source, mx_name, mx, mx_lower, mx_upper, q_flag
    )
}

# =============================================================================
# E. Clean adult asthma (18+)
# =============================================================================

adult_age_recode <- c(
  "18+ years"   = "18 plus",
  "18-64 years" = "18 to 64",
  "65+ years"   = "65 plus"
)

# Adult
asth_adult <- clean_chis_asthma(
  data        = chis,
  age_recode  = adult_age_recode,
  keep_qflags = c(0L),
  otcm_suffix = " (adults)"
) |>
  mutate(
    mx       = mx       / 100,
    mx_lower = mx_lower / 100,
    mx_upper = mx_upper / 100
  )


# QA
message(sprintf("Adult asthma rows: %d", nrow(asth_adult)))
message(sprintf("Counties: %d (expected 9 + state)", n_distinct(asth_adult$lctn_nm)))
summary(asth_adult[, c("mx", "mx_lower", "mx_upper")])

write_csv(asth_adult, "data/processed/adult_currentasthma_state_county_2021_2022_CHIS.csv")
rm(asth_adult, chis, ct, fips)
gc()
# =============================================================================
# F. Clean child asthma (0-17)
# =============================================================================

child_age_recode <- c(
  "0-4 years"  = "0 to 4",
  "5-17 years" = "5 to 17",
  "0-17 years" = "0 to 17"
)

# Child
asth_child <- clean_chis_asthma(
  data        = chis,
  age_recode  = child_age_recode,
  keep_qflags = c(0L, 2L),
  otcm_suffix = " (children)"
) |>
  mutate(
    mx       = mx       / 100,
    mx_lower = mx_lower / 100,
    mx_upper = mx_upper / 100
  )


# QA
message(sprintf("Child asthma rows: %d", nrow(asth_child)))
message(sprintf("Counties: %d", n_distinct(asth_child$lctn_nm)))
summary(asth_child[, c("mx", "mx_lower", "mx_upper")])

write_csv(asth_child, "data/processed/child_currentasthma_state_county_2021_2022_CHIS.csv")

rm(asth_child)
gc()

# =============================================================================
# Notes:
# "Current asthma prevalence" = diagnosed by provider AND still has asthma
#   OR had episode/attack in past 12 months
# q_flag: 0 = reliable, 1 = missing (unreliable), 2 = statistically unstable
# Child county estimates are often unstable or missing — state-level more reliable
# mx values converted from percent to proportion (divided by 100)
# =============================================================================



