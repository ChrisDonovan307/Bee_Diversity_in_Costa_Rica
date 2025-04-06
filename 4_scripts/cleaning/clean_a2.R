# Cleaning data from Appendix 2 - Sampled Farms



# Load Packages and Data --------------------------------------------------


pacman::p_load(dplyr,
               skimr,
               janitor,
               stringr,
               purrr,
               janitor,
               readr,
               readxl)

# Convenience function to explore variables
source('3_functions/explore.R')

# Appendix 2, sampled farm data
dat <- read_csv('1_raw/Appendix 2. Sampled_farms.csv') |> 
    clean_names()

# Environmental covariates for Turrialba shared by Adina
turri_covars <- read_excel('1_raw/bee observation covariables turrialba.xlsx') %>% 
    janitor::clean_names()


# Check Missing Data ------------------------------------------------------


skim(dat)
# Looks pretty good overall. Some missing farm and coffee areas. Lots vars 
#   have 30 missing, which must be the Turrialba region 2019-2020. Everything
#   else is 2021. 

# Some lat and long must be mixed up. 
# Will have to make area units consistent.

# Check out the 7-9 farms with missing farm area, coffee area, and sky
dat |> 
    filter(region != 'Turrialba') |> 
    filter(is.na(farm_area) | is.na(coffee_area) | is.na(sky) | is.na(coffee_area_unit) | is.na(farm_area_unit)) |> 
    select(region:codfrm,
           province,
           farm_area:coffee_area_unit,
           sky,
           notes)

# 6 are missing coffee area and farm area
# 8 are missing sky
# This doesn't quite add up - maybe Turrialba farms are also missing some?

(missing_data <- dat |> 
    filter(is.na(farm_area) | is.na(coffee_area) | is.na(sky) | is.na(coffee_area_unit) | is.na(farm_area_unit)) |> 
    select(region:codfrm,
           province,
           farm_area:coffee_area_unit,
           sky,
           notes))
# Yes, Turri has 3 missing farm area, one of which is also missing coffee area



# Explore and Clean Variables ---------------------------------------------


get_str(dat)
# 142 farms

explore('region')
# 5 regions

explore('province')
# 5 provinces

# Says there is one in Puntarenas, but doesn't look that way on the map
# Which one is it?
dat |> 
    filter(province == 'Puntarenas') |> 
    select(region, farm, codfrm, province:district)
# region        farm         codfrm province   county       district
# Pérez Zeledón Mario Piedra MARPIE Puntarenas Buenos Aires Volcan

explore('county')
# Same as canton?
# 19 counties represented

explore('district')
# 44 represented
# Cirri Sur and Cirri Sur with accent
# Some capitalization inconsistencies

# Fix these
dat$district <- str_to_title(dat$district) |> 
    str_replace_all('Cirri Sur', 'Cirrí Sur') |> 
    str_replace_all('Volcan', 'Volcán')
explore('district')
# good good

explore('codfrm')
# all unique

hist(dat$latitude)
hist(dat$longitude)
# Someone is backward

# Find out where
dat |> 
    filter(latitude < 0) |> 
    pull(farm)

# Find indices
which(str_detect(dat$farm, 'Miguel N'))

# Swap them
dat[132:133, c('latitude', 'longitude')] <- dat[132:133, c('longitude', 'latitude')]

# Check lat and long ranges to double check
hist(dat$latitude)
hist(dat$longitude)
# looks good

hist(dat$elevation)
range(dat$elevation)
# Looks good. Let's include units though
dat <- rename(dat, elevation_m = elevation)

hist(dat$start_time, breaks = 'hours')
head(dat$start_time)
# Apparently a late start on one? Can't be bees out at 8pm right, maybe typo

# Convert start time to time format, get rid of bad date
dat <- dat %>% 
    mutate(start_time = format(start_time, "%H:%M:%S %Z"))

hist(dat$date, breaks = 'months')
# all between March and May of 2021

dat <- rename(dat, temp_c = temperature)
range(dat$temp_c, na.rm = TRUE)
hist(dat$temp_c)
# Good

range(dat$wind, na.rm = TRUE)
hist(dat$wind)


range(dat$farm_area, na.rm = TRUE)
unique(dat$farm_area_unit)
range(dat$coffee_area, na.rm = TRUE)
unique(dat$coffee_area_unit)
# Units are only mzn and ha.
# Error - no farm is 44,400 mzn. This was read in as a date. 8-9 rather than 8.5

# Find this and manually set to 8.5 mzn
dat %>% 
    filter(farm_area > 40000) %>% 
    select(codfrm, farm_area, farm_area_unit)
# EDUCOR farm says 44,447 mzn, but it should be 8.5

# Change it
dat$farm_area[dat$codfrm == 'EDUCOR'] <- 8.5

# Check it
dat %>% 
    filter(codfrm == 'EDUCOR') %>% 
    pull(farm_area)

# Make new vars for farm and coffee area in ha and get rid of old columns
dat <- dat |>
    mutate(farm_area_ha = ifelse(farm_area_unit == 'mzn', 
                                 farm_area * 0.69, 
                                 farm_area),
           coffee_area_ha = ifelse(coffee_area_unit == 'mzn', 
                                   coffee_area * 0.69, 
                                   coffee_area)) |> 
    select(-c(farm_area_unit,
              farm_area,
              coffee_area_unit,
              coffee_area))

# Check new ranges
map(c('farm_area_ha', 'coffee_area_ha'), ~ range(dat[[.]], na.rm = TRUE))



# Add Turrialba Data ------------------------------------------------------


#' Incorporating data from the list of covariates that Adina shared. They are 
#' split up by visit, not by farm, and there are generally three visits per 
#' farm. We will take the means or modes of the three flower variables, as well
#' as temp, wind, and sky, to fill in some of that missing data.
get_str(dat)
get_str(turri_covars)

find_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

turri_covars <- turri_covars %>% 
    rename(codfrm = farm) %>% 
    group_by(codfrm) %>% 
    summarize(
        mode_coffee = find_mode(coffee_bloom),
        mode_weed = find_mode(weed_bloom),
        mode_shade = find_mode(shade_bloom),
        mean_temp = mean(temp, na.rm = TRUE),
        mean_wind = mean(wind, na.rm = TRUE),
        mean_sky = mean(sky, na.rm = TRUE)
    )

# Check that all Turri farms are in main farm_dat df
turri_farms <- unique(turri_covars$codfrm)
all(turri_farms %in% dat$codfrm)
# Good good

# Port Turri data into the farm_dat data
dat <- dat %>% 
    left_join(turri_covars, by = 'codfrm') %>% 
    mutate(
        coffee_flower = coalesce(coffee_flowering, mode_coffee),
        ground_flower = coalesce(ground_flowers, mode_weed),
        canopy_flower = coalesce(canopy_flowers, mode_shade),
        temp_c = coalesce(temp_c, mean_temp),
        wind = coalesce(wind, mean_wind),
        sky = coalesce(sky, mean_sky),
        .keep = 'unused'
    )



# Fix Classes -------------------------------------------------------------


# Make factors factors, and ordinal as necessary
# rain, shade_type are not ordered
# rain intensity, and coffee flowering are ordinal 1 to 3
# ground flowers and canopy flowers ordinal 1 to 4
get_str(dat)
dat <- dat |>
    mutate(across(c(rain, shade_type), as.factor),
           across(c(rain_intensity, coffee_flower:canopy_flower), as.ordered))



# Check sites and farms ---------------------------------------------------


# Number of unique farms
dat$farm %>% 
  str_split_i('[0-9]|#|\\(', 1) %>% 
  str_trim() %>% 
  unique()
# 120

# Number of sites
dat$codfrm %>% 
  unique %>% 
  length
# 142



# Save Clean Data ---------------------------------------------------------


saveRDS(dat, '2_clean/a2_sampled_farms.rds')
clear_data()

