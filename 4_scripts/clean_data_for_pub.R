# Clean Data for Pub


# Description -------------------------------------------------------------


# Clean datasets for publication on Dataverse by removing PII including name,
# farm name, and latitude/longitude.

# The wrangled data will include:
#   a2_sampled_farms
#   a4_spp_identification
#   a5_spp_traits
#   farm_dat_spp (farm level)
#   full (observation level)



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  skimr,
  dplyr,
  purrr,
  stringr,
  sf,
  readr
)

# farms <- readRDS('2_clean/a2_sampled_farms.rds')
# species <- readRDS('2_clean/a4_spp_identification.rds')
# farm_dat <- readRDS('2_clean/spatial/farms_sf.rds') %>% 
#   select(-geometry) %>% 
#   as.data.frame()
# traits <- readRDS('2_clean/a5_spp_traits.rds')
# full <- readRDS('2_clean/full.rds')



# Prep Data For Pub -------------------------------------------------------


# These are the Appendices (2, 4, 5) that have been cleaned, but have not 
# removed API from them or done any other wrangling, spatial or otherwise.

# We want to remove farm, codfrm, latitude, longitude, collector
datasets <- list(
  a2_sampled_farms = readRDS('2_clean/a2_sampled_farms.rds'),
  a4_spp_id = readRDS('2_clean/a4_spp_identification.rds'), 
  a5_spp_traits = readRDS('2_clean/a5_spp_traits.rds'),
  farm_data = readRDS('2_clean/farm_spp_dat.rds'),
  observation_data = readRDS('2_clean/full.rds')
)
names(datasets)
map(datasets, get_str)

# Remove PII from each, along with the species list (leaving species strings)
# For codfrm, we need to replace each one with a code.

# Get a key for each codfrm
key <- datasets$a2_sampled_farms %>% 
  select(codfrm) %>% 
  mutate(farm_code = row_number())
get_str(key)

# Vars to remove
remove <- c(
  'farm',
  'codfrm',
  'latitude',
  'longitude',
  'collector',
  'species_list',
  'notes'
)

# Swap out keys where applicable, then remove vars
datasets_clean <- map(datasets, ~ {
  if ('codfrm' %in% names(.x)) {
    df <- .x %>% 
      left_join(key) %>% 
      select(farm_code, everything())
  } else {
    df <- .x
  }
  df %>% 
    select(-any_of(remove))
})
map(datasets_clean, get_str)

# Save them to data pub folder
iwalk(datasets_clean, ~ {
  write_csv(
    .x,
    file = paste0('6_outputs/data_pub/', .y, '.csv')
  )
})


clear_data()