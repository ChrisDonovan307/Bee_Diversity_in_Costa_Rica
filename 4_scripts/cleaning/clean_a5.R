# Cleaning Appendix 5 - Species Traits


# Load Packages and Data --------------------------------------------------


pacman::p_load(
    dplyr,
    janitor,
    skimr,
    stringr,
    purrr,
    readr,
    fastDummies
)

# Load a function to help explore variables
source('3_functions/explore.R')
source('3_functions/make_catalog_dummies.R')

# Load appendix 5 - traits
dat <- read_csv('1_raw/Appendix 5. Species traits.csv') %>% 
    clean_names()



# Explore and Clean Variables ---------------------------------------------


skim(dat)
# Bunch of empty rows, and an errant value in row 62. Let's cut it to 52

dat <- dat[1:52, ]
skim(dat)
get_str(dat)
# Everything looks great now. 52 species represented

explore('family')
explore('tribe')
explore('genus')
explore('species')



# Floral Constancy --------------------------------------------------------


explore('floral_constancy')

dat <- dat %>% 
    dummy_cols('floral_constancy') %>% 
    rename_with(~ paste0('fc_', str_split_i(tolower(.), '_', 3)), matches('ncy_'))



# Social Behavior ---------------------------------------------------------


explore('social_behavior')
# This might be worth making dummy variables for. Different combinations of 
# behaviors. 

dat <- dat |>
    mutate(
        sb_cleptoparasite = ifelse(social_behavior == 'Cleptoparasite', 1, 0),
        sb_primitive_social = ifelse(str_detect(social_behavior, 'Primitive social'), 1, 0),
        sb_semisocial = ifelse(str_detect(social_behavior, 'Semisocial'), 1, 0),
        sb_communal = ifelse(str_detect(social_behavior, 'Communal'), 1, 0),
        sb_social = ifelse(str_detect(social_behavior, 'Social'), 1, 0),
        sb_cleptobiotic = ifelse(str_detect(social_behavior, 'Cleptobiotic'), 1, 0),
        sb_solitary = ifelse(str_detect(social_behavior, 'Solitary'), 1, 0)
    )

# Check new dummy variables
map(select(dat, starts_with('sb')), ~ table(.x, useNA = 'always'))
# cool cool



# Nesting Substrate -------------------------------------------------------


explore('nesting_substrate')
# Let's clean this up

dat$nesting_substrate <- str_replace_all(
    dat$nesting_substrate, 
    'galeries', 
    'galleries'
    ) %>%
    str_to_lower()

# Dummies
dat <- dat %>% 
    dummy_cols('nesting_substrate') %>% 
    rename_with(~ str_replace_all(paste0('ns_', str_split_i(.x, '_', 3)), ' ', '_'), matches('strate_'))



# Distributions -----------------------------------------------------------


#' Exploring distributions of each species. Need to figure out which are where.
#' So we need a dummy variable for each catalogue showing whether or not each
#' species is represented there. Then we can make another variable based on our
#' data to show where we found the bees, then see if any of them were in novel
#' regions.

# Rename catalogs to be shorter
dat <- dat %>% 
    rename(dist_moure = distribution_in_costa_rica_moure_bee_catalogue,
           dist_disc = distribution_in_costa_rica_discoverlife,
           dist_nat = distribution_in_costa_rica_costa_rican_national_museum)

# Check out catalogs
explore('dist_moure')
explore('dist_disc')
explore('dist_nat')

get_str(dat)
#' So first, we need a vector of province names. Then, for each province and 
#' each catalog, we need a dummy column for whether they found it there.

unique(dat$dist_nat)
provinces <- c(
    'Alajuela',
    'San José',
    'Limón',
    'Cartago',
    'Puntarenas',
    'Guanacaste',
    'Heredia'
) %>% sort()

# Issues to note:
# some discrepancy with accent on Limon - must clean this
# All provinces, no record was found
# All provinces except ...

# Check for missing accents
map(select(dat, matches('^dist')), ~ any(str_detect(.x, 'San Jose')))
map(select(dat, matches('^dist')), ~ any(str_detect(.x, 'Limon')))
# San Jose is good, but have to clean Limon

# Replace Limon with Limón
dat <- dat %>% 
    mutate(across(matches('^dist'), ~ str_replace_all(.x, 'Limon', 'Limón')))
map(select(dat, matches('^dist')), ~ any(str_detect(.x, 'Limon')))
# Clean

# Now make new dummy columns for distribution in each province in each catalog
dat <- dat %>% 
    make_catalog_dummies(catalog = 'moure', provinces = provinces) %>% 
    make_catalog_dummies(catalog = 'disc', provinces = provinces) %>% 
    make_catalog_dummies(catalog = 'nat', provinces = provinces)

# Now add one more column for whether a species has ever been found in that area
dat <- get_all_dist(df = dat, provinces = provinces)

get_str(dat)

# See where a species has not been found
index <- dat %>% 
    select(dist_Alajuela_all:last_col()) %>% 
    rowSums() %>% 
    {which(. == 0)}
index
# row 31 has not been found in CR before

dat[index, ] %>% select(1:species)
# Plebeia llorentei

# Now we can use these against our observed data to see if we found species in 
# any areas that were not previously documented.


# Save and clear data -----------------------------------------------------


saveRDS(dat, '2_clean/a5_spp_traits.rds')
clear_data()

