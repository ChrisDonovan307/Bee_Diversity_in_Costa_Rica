# Temporary stuff, fiddling around


# Load Data ---------------------------------------------------------------


pacman::p_load(
  skimr,
  dplyr,
  purrr,
  stringr,
  sf
)

farms <- readRDS('2_clean/a2_sampled_farms.rds')
species <- readRDS('2_clean/a4_spp_identification.rds')
farm_dat <- readRDS('2_clean/spatial/farms_sf.rds') %>% 
  select(-geometry) %>% 
  as.data.frame()
traits <- readRDS('2_clean/a5_spp_traits.rds')
full <- readRDS('2_clean/full.rds')



# -------------------------------------------------------------------------

get_str(farm_dat)
get_str(full)

full$codsp

farm_dat$farm %>% 
  str_split_i('[0-9]|#|\\(', 1) %>% 
  str_trim() %>% 
  unique()

farm_dat$start_time

get_str(farm_dat)
skimr::skim(farm_dat)

farm_dat$abundance_on_farm %>% sum(na.rm = TRUE)
full$abundance %>% sum()


# Summary Stats -----------------------------------------------------------


# No APIMEL
full <- readRDS('2_clean/full.rds')
farm_dat <- readRDS('2_clean/farm_spp_dat.rds')
dists <- readRDS('2_clean/dists_traits.rds')
rare <- readRDS('5_objects/rare_richness_by_region.rds')

get_str(farm_dat)
df <- data.frame(
  mean_abundance = mean(farm_dat$abundance_on_farm, na.rm = TRUE),
  mean_richness = mean(farm_dat$richness_on_farm, na.rm = TRUE)
)
df  

farm_dat %>% 
  group_by(region) %>% 
  summarize(
    mean_abundance = mean(abundance_on_farm, na.rm = TRUE),
    median_abundance = median(abundance_on_farm, na.rm = TRUE),
    mean_richness = mean(richness_on_farm, na.rm = TRUE)
  )

# Functional numbers
farm_dat %>% 
  filter(hill_shannon != 0) %>% 
  group_by(region) %>% 
  summarize(
    median_hill = median(hill_shannon, na.rm = TRUE)
  )

# Rarefied richness
rare


# Median and Mean Richness ------------------------------------------------


get_str(farm_dat)
farm_dat %>% 
  summarize(
    'mean_abundance' = mean(abundance_on_farm, na.rm = TRUE),
    'median_abundance' = median(abundance_on_farm, na.rm = TRUE)
  )

# Add up total abundance on farm
sum(farm_dat$abundance_on_farm)
# 1640

get_str(full)
sum(full$abundance)
# 1640

# Check with APIs
full_api <- readRDS('2_clean/with_apimel/full_api.rds')
sum(full_api$abundance)


# Numbers of families and such
full_api %>% 
  group_by(family) %>% 
  summarize(count = sum(abundance))
full_api %>% 
  group_by(tribe) %>% 
  summarize(count = sum(abundance)) %>% 
  arrange(count)



# Check dims --------------------------------------------------------------


farm_dat <- readRDS('2_clean/farm_spp_dat.rds')
farm_dat_func <- readRDS('2_clean/farm_spp_dat_func.rds')

dim(farm_dat)
dim(farm_dat_func)



# Species Names -----------------------------------------------------------


full <- readRDS('2_clean/full.rds')
get_str(full)

full %>% 
  select(codsp, genus, species) %>% 
  unique() %>% 
  arrange(codsp) %>% 
  print(n = 500)



# Explore Functional Traits -----------------------------------------------


# full dataset including APIMEL
full <- readRDS('2_clean/with_apimel/full_api.rds')
get_str(full)

# Floral constancy
full %>% 
  group_by(floral_constancy) %>% 
  summarize(
    count = sum(abundance),
    percent = count / sum(full$abundance) * 100
  )

# Nesting
full %>% 
  group_by(nesting_substrate) %>% 
  summarize(
    count = sum(abundance),
    percent = count / sum(full$abundance) * 100
  )

# Social behavior
full %>% 
  group_by(social_behavior) %>% 
  summarize(
    count = sum(abundance),
    percent = count / sum(full$abundance) * 100
  )



# Numbers of species by group ---------------------------------------------


# full dataset including APIMEL
full <- readRDS('2_clean/with_apimel/full_api.rds')
get_str(full)

# Tribe WITHOUT TURRIALBA
full %>% 
  filter(region != 'Turrialba') %>%
  group_by(tribe) %>% 
  summarize(abundance = sum(abundance))

# Species WITHOUT TURRIALBA
# First pick a few we are reporting in text
spp <- c('GEOLUT', 'TRICOR', 'TETANG')
full %>% 
  filter(region != 'Turrialba', codsp %in% spp) %>%
  group_by(codsp) %>% 
  summarize(
    abundance = sum(abundance),
    percent_meli = (abundance / 1137) * 100) %>% 
  print(n = 100)



# Summary Stats by Region -------------------------------------------------


get_str(farm_dat)
# get_str(farms)

farm_dat %>% 
  group_by(region) %>% 
  summarize(
    elevation = mean(elevation_m),
    mean_temp = mean(mean_spring_temp_c),
    mean_precip = mean(mean_spring_precip_mm)
  )


# Species per Region ------------------------------------------------------


get_str(species)

# Diversity
species %>% 
  group_by(region) %>% 
  summarize(
    families = length(unique(family)),
    tribes = length(unique(tribe)),
    genera = length(unique(genus)),
    species = length(unique(codsp))
  ) %>% 
  print(n = 100)

# Abundance by tribe
species %>% 
  group_by(region, tribe) %>% 
  summarize(
    abundance = sum(abundance)
  ) %>% 
  print(n = 100)
  
# Raw abundance
species %>% 
  group_by(region) %>% 
  summarize(
    abundance = sum(abundance)
  ) %>% 
  print(n = 100)

# Mean farm abundance
farm_dat %>% 
  group_by(region) %>% 
  summarize(
    mean_farm_abun = mean(abundance_on_farm)
  )

# Forest fragment thing
farm_dat %>% 
  group_by(region) %>% 
  summarize(
    forest = mean(prop_forest_1km_sinac)
  )


# -------------------------------------------------------------------------

# When did sampling take place in Turrialba
species %>% 
  filter(region == 'Turrialba') %>% 
  pull(date)

