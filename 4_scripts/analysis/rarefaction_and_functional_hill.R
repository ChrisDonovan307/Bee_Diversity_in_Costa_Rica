#' Rarefaction and functional hill numbers


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  vegan,
  tidyr,
  skimr,
  tibble,
  hillR,
  purrr,
  stringr,
  tibble
)

# Pull in the spatial farm data - it has all the ecoregion info
farm_dat <- readRDS('2_clean/spatial/farms_sf.rds') %>% 
  select(-geometry) %>% 
  as.data.frame()
full <- readRDS('2_clean/full.rds')
traits <- readRDS('2_clean/dists_traits.rds')



# Wrangle -----------------------------------------------------------------


#' Do we want data at the farm level, or region level for Shannon?
#' For regressions, we would want it at the farm level. Let's do that for now.
#' So we want a DF with 136 rows (one for each farm) and 60 columns (species)

# Starting with the full dataset
get_str(full)

# Select relevant columns, group by farm, and make a col for each species
# By abundance
vegan_dat_farm <- full %>% 
  select(codfrm, codsp, abundance) %>% 
  arrange(codsp) %>% 
  filter(!grepl('SPP$', codsp)) %>%
  pivot_wider(names_from = codsp, 
              values_from = abundance, 
              values_fill = 0)
get_str(vegan_dat_farm)
# Down to 40 when removing the 20 that were only IDed to genus
# Also lost 2 farms because they ONLY had xxxSPP observations to genus

# Same thing but for region (rarefaction)
vegan_dat_region <- full %>% 
  select(region, codsp, abundance) %>% 
  filter(!grepl('SPP$', codsp)) %>% 
  pivot_wider(names_from = codsp, 
              values_from = abundance,
              values_fn = ~ sum(.x),
              values_fill = 0) %>% 
  column_to_rownames('region') %>% 
  select(order(names(.)))
get_str(vegan_dat_region)

#' For hill numbers, we also need a DF with species as rows and traits as cols
hill_traits <- traits %>% 
  select(codsp, matches('^(fc_|sb_|ns_)')) %>% 
  arrange(codsp) %>% 
  filter(!grepl('SPP$', codsp)) %>% 
  column_to_rownames('codsp') %>% 
  mutate(across(everything(), as.numeric))

get_str(hill_traits)



# Trad Shannon ------------------------------------------------------------


div <- diversity(vegan_dat_farm[, -1], index = 'shannon')
hist(div)

# Save this back to the vegan_dat DF, from there back to farm_dat
farm_shannon <- bind_cols(vegan_dat_farm, trad_shannon = div) %>%
  select(codfrm, trad_shannon) %>%
  left_join(farm_dat, by = 'codfrm')
get_str(farm_shannon)



# Rarefaction -------------------------------------------------------------


# Get minimum number of observations per sample, but into rarefy
(obs_per_farm <- rowSums(vegan_dat_farm[, -1]))

# Many farms had only 1 obs. Can't imagine this will work
(rare <- rarefy(vegan_dat_farm[, -1], min(obs_per_farm)))
# Yeah. No

# Try it at the region level. Wrangle again by region
get_str(vegan_dat_region)
(obs_per_region <- rowSums(vegan_dat_region[, -1]))
# 244 275 465 169 403
# Lowest is 169 - Valle Occidental

(rare_region <- rarefy(vegan_dat_region[, -1], min(obs_per_region)))
# Better

# Add names back into it as a DF
rare_rich_region <- data.frame(
  region = rownames(vegan_dat_region),
  rare_rich = round(rare_region, 3)
)
# Will save this below



# Functional Hill Numbers --------------------------------------------------


# Try with q = 1 (functional hill number). First by region
(hill_output <- hill_func(vegan_dat_region, hill_traits, q = 0.999))
get_str(hill_output)
(hill_shannon <- hill_output['D_q', ])

# Try with q = 1 (Hill Shannon)
(hill_output <- hill_func(vegan_dat_farm[, -1], hill_traits, q = 0.999))
get_str(hill_output)

# Pull out just the D_q (hill shannon)
(hill_shannon <- hill_output['D_q', ])

# Join back to farm_shannon
farm_hill <- bind_cols(farm_shannon, hill_shannon = hill_shannon)
get_str(farm_hill)
# Down from 136 farms (no apimel) to 134 farms (after removing unidentified spp)

farm_hill$hill_shannon %>%
  get_table()
# Now has 37 zeroes (because we took out APIMEL)



# Save --------------------------------------------------------------------


# Save rarefied richness by region
saveRDS(rare_rich_region, '5_objects/rare_richness_by_region.rds')

# Saving over spatial version of farm dat
saveRDS(farm_hill, '2_clean/spatial/farms_sf.rds')

# Save a non-spatial version so we can avoid loading sf package later
farm_hill %>% 
  select(-geometry) %>% 
  as.data.frame() %>% 
  saveRDS('2_clean/farm_spp_dat.rds')

# Clear environment
clear_data()
gc()
