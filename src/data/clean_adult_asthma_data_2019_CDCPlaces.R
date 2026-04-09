# =============================================================================
# Download CDC PLACES 2021 tract- and county-level asthma prevalence for California
#
# Source: CDC PLACES 2021 release (model year 2019)
# Target: Current asthma prevalence + 95% CI (crude prevalence)
# =============================================================================

library(sf)
library(httr)
library(jsonlite)
library(tidyverse)
library(sf)

source("config.R")

# =============================================================================
# A. Configuration
# =============================================================================

endpoints <- list(
  tract  = "https://data.cdc.gov/resource/373s-ayzu.json",
  county = "https://data.cdc.gov/resource/pqpp-u99h.json"        # replace with 2021 county endpoint
)

# Field names — verify against API before running:
# httr::GET(paste0(endpoint, "?$limit=1")) |> httr::content("text") |> jsonlite::fromJSON() |> names()
fields <- list(
  geoid       = "locationid",
  lctn_nm     = "locationname",
  state       = "stateabbr",
  measure     = "measureid",
  prevalence  = "data_value",
  mx_name     = "data_value_type",
  ci_low      = "low_confidence_limit",
  ci_high     = "high_confidence_limit"
)

ca_state      <- "CA"
measure_id    <- "CASTHMA"
mx_type       <- "Crude prevalence"
batch_size    <- 50000

# =============================================================================
# B. Download function (defined once)
# =============================================================================

download_places_paginated <- function(endpoint, state, measure_id, mx_type,
                                      fields, batch_size = 50000) {
  select_fields <- paste(unique(unlist(fields)), collapse = ",")
  where_clause  <- sprintf(
    "%s='%s' AND %s='%s' AND %s='%s'",
    fields$state,   state,
    fields$measure, measure_id,
    fields$mx_name, mx_type
  )
  
  all_data <- list()
  offset   <- 0
  page     <- 1
  
  repeat {
    message(sprintf("Downloading batch %d (offset %d)...", page, offset))
    
    response <- GET(
      url   = endpoint,
      query = list(
        `$select` = select_fields,
        `$where`  = where_clause,
        `$limit`  = as.integer(batch_size),
        `$offset` = as.integer(offset)
      )
    )

    if (http_error(response)) {
      stop(sprintf("API request failed: HTTP %d\n%s",
                   status_code(response),
                   content(response, as = "text", encoding = "UTF-8")))
    }
    
    batch <- content(response, as = "text", encoding = "UTF-8") |>
      fromJSON(flatten = TRUE) |>
      as_tibble()
    
    if (nrow(batch) == 0) { message("No more rows — download complete."); break }
    
    all_data[[page]] <- batch
    message(sprintf("  Retrieved %d rows.", nrow(batch)))
    
    if (nrow(batch) < batch_size) break
    offset <- offset + batch_size
    page   <- page + 1
  }
  
  bind_rows(all_data)
}

# =============================================================================
# C. Shared cleaning function
# =============================================================================

clean_places <- function(raw, geolevl, fields) {
  raw |>
    rename(
      geoid    = !!fields$geoid,
      lctn_nm  = !!fields$lctn_nm,
      mx_name  = !!fields$mx_name,
      mx       = !!fields$prevalence,
      mx_lower = !!fields$ci_low,
      mx_upper = !!fields$ci_high
    ) |>
    mutate(
      across(c(mx, mx_lower, mx_upper), as.numeric),
      mx       = mx       / 100,    # percent → proportion
      mx_lower = mx_lower / 100,
      mx_upper = mx_upper / 100,
      otcm_nm  = "Current asthma prevalence (adults)",
      source   = "CDC Places",
      geolevl  = geolevl,
      age_grp  = "18 plus",
      sex_grp  = "Both",
      race_grp = "Total",
      year     = "2019",
      mx_name  = "prevalence",
      q_flag   = 0L
    ) |>
    select(geoid, geolevl, lctn_nm, age_grp, sex_grp, race_grp,
           otcm_nm, year, source, mx_name, mx, mx_lower, mx_upper, q_flag)
}

# =============================================================================
# D. Download and clean — tract level
# =============================================================================

message("Downloading CDC PLACES 2021 asthma data — census tracts...")
raw_tract <- download_places_paginated(endpoints$tract, ca_state, measure_id,
                                       mx_type, fields, batch_size)
message(sprintf("Downloaded %d rows.", nrow(raw_tract)))

places_tract <- clean_places(raw_tract, geolevl = "tract", fields) |>
  filter(geoid %in% sfba_geoids)   # sfba_geoids = tract GEOIDs from config.R (see note below)

# QA
message(sprintf("SFBA tracts retrieved: %d (expected ~1,580)", n_distinct(places_tract$geoid)))
if (any(is.na(places_tract$mx)))
  warning(sprintf("%d tracts have missing asthma prevalence.", sum(is.na(places_tract$mx))))
summary(places_tract[, c("mx", "mx_lower", "mx_upper")])

# =============================================================================
# E. Download and clean — county level
# =============================================================================

message("Downloading CDC PLACES 2021 asthma data — counties...")
raw_county <- download_places_paginated(endpoints$county, ca_state, measure_id,
                                        mx_type, fields, batch_size)
message(sprintf("Downloaded %d rows.", nrow(raw_county)))

places_county <- clean_places(raw_county, geolevl = "county", fields) |>
  filter(lctn_nm %in% sfba_names)   # from config.R

# QA
message(sprintf("SFBA counties retained: %d (expected 9)", n_distinct(places_county$geoid)))
if (any(is.na(places_county$mx)))
  warning(sprintf("%d counties have missing asthma prevalence.", sum(is.na(places_county$mx))))
summary(places_county[, c("mx", "mx_lower", "mx_upper")])

# =============================================================================
# F. Combine and write
# =============================================================================

bind_rows(places_tract, places_county) |>
  write_csv("data/processed/adult_currentasthma_ctract_county_2019_CDCPlaces.csv", append = FALSE)

rm(ct, endpoints, fields, places_county, places_tract, raw_county, raw_tract)
gc()
