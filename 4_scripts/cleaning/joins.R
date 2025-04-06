# Joining datasets

#' Consolidating the useful joins into this script. Making one set of data with
#' APIMEL and one without. Note that this script could use some refactoring. It
#' is currently running same workflow twice, second time without APIMEL.

#' Outputs:
#'  1. farm_dat - species data by farm
#'  2. full - join of sampled farms and species ID, one row for each observation
#'  3. traits_dists - traits and observed and recorded dists for each species



# Load Packages and Data --------------------------------------------------


pacman::p_load(
    dplyr,
    purrr,
    tidyr,
    stringr,
    readxl
)

farms <- readRDS('2_clean/a2_sampled_farms.rds')
species <- readRDS('2_clean/a4_spp_identification.rds')
traits <- readRDS('2_clean/a5_spp_traits.rds')



# With APIMEL -------------------------------------------------------------
## Richness and Abundance from Species -------------------------------------


# Group species data by farm - will join later
spp_dat_by_farm <- species %>% 
    group_by(codfrm) %>% 
    summarize(abundance_on_farm = sum(abundance),
              richness_on_farm = length(unique(codsp)),
              spp_list = list(codsp),
              spp_strings = paste(codsp, collapse = ', '))

get_str(spp_dat_by_farm)
head(spp_dat_by_farm)

# Check if richness matches spp_list length
which(sapply(seq_len(nrow(spp_dat_by_farm)), function(i) {
    spp_dat_by_farm$richness_on_farm[i] != length(spp_dat_by_farm$spp_list[[i]])
}))
# Clear



## Data by Farm ------------------------------------------------------------


# Putting spp_dat_by_farm data frame (richness and abundance) together with farms
get_str(farms)
get_str(spp_dat_by_farm)

# But there are some farms that have no spp data associated with them
# Which are they?
setdiff(farms$codfrm, spp_dat_by_farm$codfrm)
# MARBAR and ELPIL1, others have no spp data associated. Some lost because Apis
# was taken out

# Pull some more info about them
farms %>% 
    filter(codfrm %in% c('MARBAR', 'ELPIL1')) %>% 
    select(codfrm, farm, date, region, province)
# 1 MARBAR Mario Barquero   2021-04-05 00:00:00 Valle Central    Heredia 
# 2 ELPIL1 Finca El Pilon 1 2021-04-16 00:00:00 Valle Occidental Alajuela

# Use inner join to only keep farms that have spp data
farm_dat <- inner_join(farms, spp_dat_by_farm, by = 'codfrm')
get_str(farm_dat)
# This brings farm count down from 142 to 140

# Check if richness on farm looks reasonable
table(farm_dat$richness_on_farm)
# Looks good



## Full Join ---------------------------------------------------------------


get_str(farms)
get_str(species)

# Omitting the two farms without spp data
full <- farms %>% 
    filter(codfrm %in% species$codfrm) %>% 
    full_join(species, by = 'codfrm', suffix = c('_farm', '_spp'))

get_str(full)

# Check if variables are identical between datasets
vars <- c('region',
          'farm',
          'date',
          'latitude',
          'longitude',
          'elevation_m')
vars_farm <- paste0(vars, '_farm')
vars_spp <- paste0(vars, '_spp')
map2(vars_farm, vars_spp, ~ identical(full[[.x]], full[[.y]]))
# Latitude is different, everything else is okay

# Where is it different
full[which(full$latitude_farm != full$latitude_spp), ]
# This is Miguel Nunez, we fixed this in the farms data. 
# Let's just have it override spp data here then.

# Let's just get rid of extraneous columns and redo the join.
species_to_join <- species %>% 
    select(-all_of(vars))

full <- farms %>% 
    filter(codfrm %in% species$codfrm) %>% 
    full_join(species_to_join, by = 'codfrm')

get_str(full)



## Add codes to traits -----------------------------------------------------


get_str(farm_dat)
get_str(traits)
get_str(full)
# 665 records

# Pull just codes, genus and sp from full
codes <- full %>%
    select(genus, species, codsp) %>% 
    unique()
dim(codes)

# Now do full join for reals
traits_codsp <- full_join(codes, traits, by = c('genus', 'species'))
get_str(traits_codsp)

traits_codsp %>% 
    select(1:5) %>% 
    arrange(codsp) %>% 
    print(n = 100)
# there are 8 unknowns that are not matched in traits doc, but that's okay!



## Observed Distributions --------------------------------------------------


# Figure out in which provinces we found which species. Make DF for posterity
get_str(full)
dists <- full %>% 
    group_by(codsp) %>% 
    summarize(
        province_list = list(unique(str_remove(province, ' '))),
        .groups = 'drop') %>%
    unnest(province_list) %>% 
    mutate(present = 1) %>%
    pivot_wider(
        names_from = province_list, 
        values_from = present, 
        values_fill = list(present = 0)) %>% 
    rename_with(~ paste0('dist_', ., '_obs'), .cols = -codsp)
get_str(dists)

# Make new distributions DF that has all the info from traits and also our data
dists_traits <- inner_join(dists, traits_codsp, by = 'codsp') %>% 
    select(codsp, 
           family, 
           tribe, 
           genus, 
           species, 
           floral_constancy:nesting_substrate,
           everything())
get_str(dists_traits)



## Fuller Join -------------------------------------------------------------


# Want everything together in one giant DF for graphs and regressions
get_str(full)

# Join full with dists_traits so we have everything
fuller <- left_join(full, dists_traits)
get_str(fuller)



## Save and Clear ----------------------------------------------------------


# Saving these with APIMEL in a separate folder so as not to get them mixed up
# Full join of farm data and species observations
saveRDS(fuller, '2_clean/with_apimel/full_api.rds')

# Farms with species found on those farms
saveRDS(farm_dat, '2_clean/with_apimel/farm_spp_dat_api.rds')

# Distributions and traits for all species
saveRDS(dists_traits, '2_clean/with_apimel/dists_traits_api.rds')

clear_data()



# Without APIMEL ----------------------------------------------------------


#' This is some pretty unfortunate code, for now just copy pasting and running
#' through same workflow without APIMEL. Note that this could use refactoring

farms <- readRDS('2_clean/a2_sampled_farms.rds')
species <- readRDS('2_clean/a4_spp_identification.rds')
traits <- readRDS('2_clean/a5_spp_traits.rds')

# Removing APIMEL from everything because of inconsistent sampling in Turrialba
get_str(species)
species <- filter(species, codsp != 'APIMEL')



## Richness and Abundance from Species -------------------------------------


# Group species data by farm - will join later
spp_dat_by_farm <- species %>% 
    group_by(codfrm) %>% 
    summarize(abundance_on_farm = sum(abundance),
              richness_on_farm = length(unique(codsp)),
              spp_list = list(codsp),
              spp_strings = paste(codsp, collapse = ', '))

get_str(spp_dat_by_farm)
head(spp_dat_by_farm)

# Check if richness matches spp_list length
which(sapply(seq_len(nrow(spp_dat_by_farm)), function(i) {
    spp_dat_by_farm$richness_on_farm[i] != length(spp_dat_by_farm$spp_list[[i]])
}))
# Clear



## Data by Farm ------------------------------------------------------------


# Putting spp_dat_by_farm data frame (richness and abundance) together with farms
get_str(farms)
get_str(spp_dat_by_farm)

# But there are some farms that have no spp data associated with them
# Which are they?
setdiff(farms$codfrm, spp_dat_by_farm$codfrm)
# MARBAR and ELPIL1, others have no spp data associated. Some lost because Apis
# was taken out

# Pull some more info about them
farms %>% 
    filter(codfrm %in% c('MARBAR', 'ELPIL1')) %>% 
    select(codfrm, farm, date, region, province)
# 1 MARBAR Mario Barquero   2021-04-05 00:00:00 Valle Central    Heredia 
# 2 ELPIL1 Finca El Pilon 1 2021-04-16 00:00:00 Valle Occidental Alajuela

# Use inner join to only keep farms that have spp data
farm_dat <- inner_join(farms, spp_dat_by_farm, by = 'codfrm')
get_str(farm_dat)

# Check if richness on farm looks reasonable
table(farm_dat$richness_on_farm)
# Looks good



## Full Join ---------------------------------------------------------------


get_str(farms)
get_str(species)

# Omitting the two farms without spp data
full <- farms %>% 
    filter(codfrm %in% species$codfrm) %>% 
    full_join(species, by = 'codfrm', suffix = c('_farm', '_spp'))

get_str(full)

# Check if variables are identical between datasets
vars <- c('region',
          'farm',
          'date',
          'latitude',
          'longitude',
          'elevation_m')
vars_farm <- paste0(vars, '_farm')
vars_spp <- paste0(vars, '_spp')
map2(vars_farm, vars_spp, ~ identical(full[[.x]], full[[.y]]))
# Latitude is different, everything else is okay

# Where is it different
full[which(full$latitude_farm != full$latitude_spp), ]
# This is Miguel Nunez, we fixed this in the farms data. 
# Let's just have it override spp data here then.

# Let's just get rid of extraneous columns and redo the join.
species_to_join <- species %>% 
    select(-all_of(vars))

full <- farms %>% 
    filter(codfrm %in% species$codfrm) %>% 
    full_join(species_to_join, by = 'codfrm')

get_str(full)



## Add codes to traits -----------------------------------------------------


get_str(farm_dat)
get_str(traits)
get_str(full)

# Pull just codes, genus and sp from full
codes <- full %>%
    select(genus, species, codsp) %>% 
    unique()
dim(codes)

# Now do full join for reals
traits_codsp <- full_join(codes, traits, by = c('genus', 'species'))
get_str(traits_codsp)

traits_codsp %>% 
    select(1:5) %>% 
    arrange(codsp) %>% 
    print(n = 100)



## Observed Distributions --------------------------------------------------


# Figure out in which provinces we found which species. Make DF for posterity
get_str(full)
dists <- full %>% 
    group_by(codsp) %>% 
    summarize(
        province_list = list(unique(str_remove(province, ' '))),
        .groups = 'drop') %>%
    unnest(province_list) %>% 
    mutate(present = 1) %>%
    pivot_wider(
        names_from = province_list, 
        values_from = present, 
        values_fill = list(present = 0)) %>% 
    rename_with(~ paste0('dist_', ., '_obs'), .cols = -codsp)
get_str(dists)

# Make new distributions DF that has all the info from traits and also our data
dists_traits <- inner_join(dists, traits_codsp, by = 'codsp') %>% 
    select(codsp, 
           family, 
           tribe, 
           genus, 
           species, 
           floral_constancy:nesting_substrate,
           everything())
get_str(dists_traits)



## Fuller Join -------------------------------------------------------------


# Want everything together in one giant DF for graphs and regressions
get_str(full)

# Join full with dists_traits so we have everything
fuller <- left_join(full, dists_traits)
get_str(fuller)


## Save and Clear ----------------------------------------------------------



# Saving these with APIMEL in a separate folder so as not to get them mixed up
# Full join of farm data and species observations
saveRDS(fuller, '2_clean/full.rds')

# Farms with species found on those farms
# Note we are saving this as temp file because we will be adding variables to it
# in spatial and rarefaction scripts
saveRDS(farm_dat, '5_objects/temp/farm_spp_dat.rds')

# Distributions and traits for all species
saveRDS(dists_traits, '2_clean/dists_traits.rds')

clear_data()
