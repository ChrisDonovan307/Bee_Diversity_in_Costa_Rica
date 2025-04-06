#' Coffee Production
#' 2024-09-29

# Just cleaning here - plot is in descriptive.R


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr,
  purrr,
  sf
)

# Pull farm data
farm_dat <- readRDS('2_clean/farm_spp_dat.rds')

# Pull data from icafe production, hand entered from PDF
# https://www.icafe.cr/wp-content/uploads/informacion_mercado/informes_actividad/actual/Informe%20Actividad%20Cafetalera.pdf
raw <- read_csv('1_raw/icafe_production.csv')

# Acreage from https://apps.fas.usda.gov/newgainapi/api/Report/DownloadReportByFileName?fileName=Coffee+Annual_San+Jose_Costa+Rica_CS2022-0008.pdf

# Newer reports
# https://apps.fas.usda.gov/newgainapi/api/Report/DownloadReportByFileName?fileName=Coffee%20Annual_San%20Jose_Costa%20Rica_CS2024-0009.pdf
# https://www.icafe.cr/wp-content/uploads/informacion_mercado/informes_actividad/actual/Informe%20Actividad%20Cafetalera.pdf

# 2020 to 2021, what we actually want
# https://apps.fas.usda.gov/newgainapi/api/Report/DownloadReportByFileName?fileName=Coffee%20Annual_San%20Jose_Costa%20Rica_05-15-2021.pdf



# Wrangle -----------------------------------------------------------------


str(raw)

# Clean and recode, remove missing
dat <- raw %>% 
  mutate(
    region = case_when(
      region == 'LS' ~ 'Los Santos',
      region == 'PZ'~ 'Perez Zeledon',
      region == 'T'~ 'Turrialba',
      region == 'VC'~ 'Valle Central',
      region == 'VO'~ 'Valle Occidental'
    )
  ) %>% 
  na.omit() %>% 
  setNames(c('region', 'province', 'y20_21', 'y22_23', 'y23_24'))
str(dat)

# Aggregate
dat <- dat %>% 
  group_by(region) %>% 
  summarize(
    y20_21 = sum(y20_21, na.rm = TRUE),
    y22_23 = sum(y22_23, na.rm = TRUE),
    y23_24 = sum(y23_24, na.rm = TRUE)
  )
str(dat)


# Add acreage
dat <- dat %>% 
  mutate(
    ha20_21 = c(27944, 13314, 4917, 13326, 21992), # Doesn't change in recent updates
    prod_per_ha_20_21 = y20_21 / ha20_21
  )
str(dat)

# Save it
saveRDS(dat, '2_clean/coffee_production.rds')
clear_data()
