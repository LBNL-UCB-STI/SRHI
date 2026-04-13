####################################################################################
# Download and clean US Census county-level population data for single year ages, race/ethnicity
# We require single age group population data to estimate prevalence of ALRI infections, as the ALRI data 
# has different age groups than that of the census tract-level population data above.
# We downloaded the 2019 unbridged population estimates for single year age groups from 
# https://seer.cancer.gov/popdata/download.html 



library(vroom)
library(dplyr)
library(sf)

# make file paths for input and output files
file_path  <- "data/raw/population_data/ca.1990_2024.singleages.through89.90plus.txt"
output_csv <- "data/raw/pops_county_2019.csv"

# define chunk size and number to read in data
chunk_size <- 5000
n_chunks   <- 479  # total iterations 

# If the output file already exists, delete it first
if (file.exists(output_csv)) file.remove(output_csv)

for (i in seq_len(n_chunks)) {
  skip <- (i - 1) * chunk_size
  message("Processing chunk ", i, "/", n_chunks, " (skipping ", skip, " rows)")
  
  # Read chunk
  data <- vroom(
    file_path,
    delim = "\t",
    col_names = FALSE,
    skip = skip,
    n_max = chunk_size,
    col_types = cols(X1 = col_character())
  )
  
  # Parse + filter for 2019 estimates for children under 18
  data <- data %>%
    mutate(
      year       = substr(X1, 1, 4),
      state      = substr(X1, 5, 6),
      st_fips    = substr(X1, 7, 8),
      co_fips    = substr(X1, 9, 11),
      race       = substr(X1, 14, 14),
      hispanic   = substr(X1, 15, 15),
      sex_name   = substr(X1, 16, 16),
      age_name   = substr(X1, 17, 18),
      population = substr(X1, 19, 26)
    ) %>%
    filter(age_name < 18, year == "2019")
  
  # Skip empty chunks (if filter removes all rows)
  if (nrow(data) == 0) next
  
  
  # Write results — append after first chunk
  write_csv(
    data,
    output_csv,
    append = file.exists(output_csv),
    col_names = !file.exists(output_csv)  # only write header once
    )
  
  rm(data, skip)
  gc()
  
}

# read in population results
pop <- read_csv("data/raw/pops_county_2019.csv", col_types = cols(
  "X1" = col_character(),         
  "year" = col_character(),      
  "state" = col_character(),      
  "st_fips"   = col_character(),  
  "co_fips"  = col_character(),   
  "race"  = col_character(),      
  "hispanic"  = col_character(),  
  "sex_name"   = col_character(),
  "age_name"  = col_character(),  
  "population" = col_double()))

# include state code in co_fips code
pop$co_fips <- paste0(pop$st_fips, pop$co_fips)

# limit to needed columns
pop <- pop[, c(5, 9, 8, 7, 6, 2, 10)]

colnames(pop) <- c("fips",    
                   "age_name",   
                   "sex_name",   
                   "hispanic",   
                   "race",  
                   "year",       
                   "population")

# load county and census tract shapefiles
counties <- st_read("data/raw/counties_and_centroids.shp")
counties <- st_as_sf(counties, crs = 4326)

# make a dataframe for merge with pop data (to add county name column)
fips <- counties[, c(2,3)] %>%
  st_drop_geometry(.)
colnames(fips) <- c("fips", "location_name")

# add location_name column and limit to the SFBA counties
pop <- merge(fips, pop, all.x = TRUE)

# recode race and ethnicity variables
# Following the method described in the US Census census tract level population estimates,
# we recoded race and ethnicity data to non-hispanic White, Black, AIAN, API and Hispanic 
pop <- pop %>%
  mutate(race_name = recode(race, "1" = "White",
                    "2" = "Black",
                    "3" = "American Indian / Alaskan Native",
                    "4" = "Asian / Pacific Islander"))
pop <- pop %>%
  mutate(race_name = ifelse(hispanic == "1", "Hispanic", race_name))

pop$race <- NULL
pop$hispanic <- NULL

# add a population total for all races (add a level "total" in race column)
race <- pop %>%
  group_by(fips, location_name, age_name, sex_name) %>%
  summarize(population = sum(population))
race$race_name <- "Total"
race$year <- "2019"

pop <- rbind(pop, race)

# recode sex variables and add a level for both sexes
pop <- pop %>%
  mutate(sex_name = recode(sex_name,
                           "1" = "Male",
                           "2" = "Female"))

both <- pop %>%
  group_by(fips, location_name, age_name, race_name) %>%
  summarize(population = sum(population))
both$year <- "2019"
both$sex_name <- "Both"

pop <- rbind(pop, both)

# sum all age groups to produce population estimates for ages 0-17, by county, race and sex
pop <- pop %>%
  group_by(fips, location_name, race_name, sex_name) %>%
  summarize(population = sum(population))
  
pop$age_name <- "0-17"
pop$year <- "2019"



# save file for merging with alri data  
write_csv(pop, "data/raw/pop_0_17_sex_race_county_2019.csv", append = FALSE)

rm(both, counties, data, fips, pop, race)
gc()
