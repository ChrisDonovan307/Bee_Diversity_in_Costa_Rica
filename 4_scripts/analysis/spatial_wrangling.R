#' Spatial Wrangling


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  sf,
  stars,
  mapview,
  rnaturalearth,
  viridisLite,
  readxl,
  leaflet,
  ggplot2,
  reactable,
  lwgeom,
  tictoc,
  readxl,
  purrr
)

pacman::p_load_gh('elipousson/sfext')

# Clean data at farm level, from temp folder
farm_dat <- readRDS('5_objects/temp/farm_spp_dat.rds')

# Initialize results list
results <- list()



# Colors ------------------------------------------------------------------


# display.brewer.all(
#   n = NULL,
#   type = "all",
#   select = NULL,
#   exact.n = TRUE,
#   colorblindFriendly = FALSE
# )
# display.brewer.pal(8, 'Set1')
# brewer.pal(8, 'Set1')



# Farms -------------------------------------------------------------------


farms_sf <- farm_dat %>%
   select(farm,
          codfrm,
          region,
          province,
          county,
          district,
          elevation_m,
          longitude,
          latitude,
          farm_area_ha:richness_on_farm,
          species = spp_strings,
          species_list = spp_list,
          start_time:sky) %>% 
   st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)



# Provinces ---------------------------------------------------------------


# Download a layer of CR Provinces from rnaturalearth
ne_prov <- ne_states(country = "costa rica") %>%
  select(name, name_alt, name_local, postal) %>%
  st_transform(4326)
ne_prov
st_crs(ne_prov)

# Use another layer that Adina shared
ad_prov <- st_read('1_raw/spatial/province_shapefiles/') %>%
  st_transform(4326) %>%
  st_make_valid()
ad_prov
get_str(ad_prov)

# Compare these to the natural earth provinces above
mapview(ad_prov, col.regions = 'red') +
  mapview(ne_prov, col.regions = 'blue')
# Yep, Adina's files are better

# Keep only relevant variables, make title case using ne names because they 
# already have accents. Also make them all multipolygons so they only show up
# with one label instead of like 100
ne_prov$name
ad_prov <- ad_prov %>% 
  select(
    province = PROVINCIA, 
    geometry
  ) %>% 
  mutate(
    province = str_to_title(province),
    province = case_when(
      str_detect(province, '^Lim') ~ ne_prov$name[3],
      str_detect(province, '^San') ~ ne_prov$name[6],
      .default = province
  )) %>% 
  st_cast('MULTIPOLYGON')
  
get_str(ad_prov)

# Save these
saveRDS(ad_prov, '2_clean/spatial/provinces.rds')



# Worldclim Temp Precip ---------------------------------------------------


# Worldclim bio1 (mean annual temp) and bio12 (mean annual precip)
# Using 30s data
# https://worldclim.org/data/bioclim.html
file_paths <- list.files('1_raw/spatial/wc2.1_30s_bio/', 
                         full.names = TRUE)
files <- map(file_paths, ~ {
  read_stars(.x)
}) %>%
  setNames(c('temp', 'precip'))

get_str(files)
files[[1]]
map(files, st_crs)

# Crop
sf_use_s2(FALSE)
cropped <- map(files, ~ st_crop(.x, ad_prov))

# Extract values
extracted <- map(cropped, ~ st_extract(.x, farms_sf))
sf_use_s2(TRUE)

# Join with farms_sf
farms_sf_climate <- farms_sf %>% 
  st_join(extracted[[1]]) %>% 
  st_join(extracted[[2]]) %>% 
  rename('mean_temp_c' = matches('bio_1\\.'),
         'mean_precip_mm' = matches('bio_12\\.'))
get_str(farms_sf_climate)

# Put farms_sf back together
farms_sf <- farms_sf_climate



# WorldClim Monthly 2.5 ---------------------------------------------------
## Precip ------------------------------------------------------------------


# Using monthly 2.5 minute data for march, april, and may from 2019-2021
file_paths <- list.files(
  '1_raw/spatial/wc_2.5_monthly/prec/',
  full.names = TRUE
)

test <- read_stars(file_paths[1])

# bbox to crop 
bbox_buffer <- farms_sf %>% 
  st_transform(st_crs(test)) %>% 
  st_bbox() %>%
  st_buffer_ext(
    dist = 10,
    unit = 'km'
  )

# read as stack, then crop
precip_stack <- read_stars(file_paths, along = 'month') %>% 
  st_crop(bbox_buffer)
gc()
precip_stack
mapview(precip_stack[, , , 1]) # dimension, x, y, month
mapview(precip_stack[, , , 9])

# Calculate the mean across all rasters in the list
avg_precip <- st_apply(
  precip_stack, 
  MARGIN = c("x", "y"), 
  FUN = mean, 
  na.rm = TRUE
)
avg_precip

# Check that it worked
mapview(avg_precip)
avg_precip[, 1, 1] # average for cell 1, 1 is 206.6

# Check cell 1, 1 manually
map_dbl(1:dim(precip_stack)[3], \(x) {
  precip_stack[1, 1, 1, x] %>% 
    as.numeric
}) %>% 
  mean()
# 206.6. Looks good

# Extract values at farm locations
st_crs(farms_sf) == st_crs(avg_precip)
precip_extracted <- st_extract(avg_precip, farms_sf)

farms_sf <- st_join(farms_sf, precip_extracted) %>% 
  rename(mean_spring_precip_mm = mean)
get_str(farms_sf)

# Clear up environment
clear_data(keep = c('farms_sf', 'bbox_buffer'))
gc()



## Temp --------------------------------------------------------------------


#' We are getting the average midpoint of max and min temp across spring months
#' and across all three years. First get midpoints of min and max for each of 9
#' months Then take average of those 9 midpoints

# File paths for both max and min
tmax_files <- list.files(
  '1_raw/spatial/wc_2.5_monthly/tmax/',
  full.names = TRUE
)
tmin_files <- list.files(
  '1_raw/spatial/wc_2.5_monthly/tmin/',
  full.names = TRUE
)

# Work through it by month. End with list of monthly avg rasters
monthly_means <- map(seq_along(tmax_files), \(x) {
  monthly_mean <- read_stars(c(tmax_files[x], tmin_files[x]), along = 'month') %>% 
    st_crop(bbox_buffer) %>% 
    st_apply(
      MARGIN = c("x", "y"), 
      FUN = mean, 
      na.rm = TRUE
    )
  return(monthly_mean)
})
monthly_means

# Put them together
monthly_mean_stack <- do.call(c, c(monthly_means, along = "month"))
monthly_mean_stack

# Get average mean temp across all 9 months
mean_temp <- st_apply(
  monthly_mean_stack,
  MARGIN = c('x', 'y'),
  FUN = mean,
  na.rm = TRUE
)
# mapview(mean_temp)

# Extract
farms_sf <- st_extract(mean_temp, farms_sf) %>% 
  st_join(farms_sf) %>% 
  relocate(mean, .after = last_col()) %>% 
  rename(mean_spring_temp_c = mean)
get_str(farms_sf)

# Clear up environment
clear_data(keep = 'farms_sf')
gc()



# Fitogeograficas ---------------------------------------------------------


fito <- st_read('1_raw/spatial/fitogeograficas') %>% 
  janitor::clean_names()

fito
st_crs(fito)
class(fito)
get_str(fito)

# Reduce fito to relevant vars, project into 4326 and join with farms_sf
fito_join <- fito %>% 
  select(idufito:descrip, 
         nombre:sub,
         uf,
         sub_uf,
         uf_nombre,
         nomenclatu,
         recno,
         geometry) %>% 
  st_transform(4326) %>% 
  st_make_valid()

# Join them
farms_sf <- st_join(farms_sf, fito_join)
get_str(farms_sf)
# 136 (no APIMEL)

# Clear environment
clear_data(keep = 'farms_sf')
gc()



# SINAC forest buffer  ----------------------------------------------------


# Want proportion of forest around each farm, 1km radius
sinac <- read_stars('1_raw/spatial/lulc_sinac_2021/raster/Tipos de Bosque y Otras Tierras de Costa Rica 2021.tif')
sinac

# Reproject farms_sf into crs of sinac so we can work with them
sinac_crs <- st_crs(sinac)
farms_sf_prj <- st_transform(farms_sf, sinac_crs) %>% 
  select(-species_list)
st_crs(farms_sf_prj)

# Make buffer around farms
bbox_buffer <- st_bbox(farms_sf_prj) %>%
  st_buffer_ext(
    dist = 10,
    unit = 'km'
  )
st_crs(bbox_buffer)

# Crop sinac layer to buffered bbox from last section
sinac_crop <- sinac %>%
  st_crop(bbox_buffer)
# mapview(sinac_crop) + mapview(farms_sf_prj)
sinac_crop


## Get classes from accompanying excel file
classes <- read_excel('1_raw/spatial/lulc_sinac_2021/raster/Codigos Clases_.xlsx')
classes
# Looks 1 through 7 are all different kinds of forest, 0 is other
# so it is just proportion of 1 through 7 out of total


## Get buffers around each farm
buffers <- farms_sf_prj %>%
  select(codfrm, geometry) %>%
  st_buffer(dist = 1000, nQuadSegs = 100) %>%
  st_simplify(dTolerance = 25)
# mapview(buffers) + mapview(farms_sf_prj)


## Function to get mean of forest (10)
get_sinac_forest_prop = function(x) {
  mean(x > 0, na.rm = TRUE)
}

tic()
out <- buffers %>% 
  split(seq(nrow(buffers))) %>% 
  map(\(row) {
    aggregate(sinac_crop, row, FUN = get_sinac_forest_prop)[[1]]
  }) %>% 
  setNames(c(buffers$codfrm))
toc()
# 90 seconds or so

out
hist(unlist(out))

# Manually check a few
# mapview(buffers) + mapview(farms_sf_prj) + mapview(sinac_crop)
out[which(names(out) == 'FREGAR')]
out[which(names(out) == 'ELVTAL')]
# Looks good

# Join it back to farms_sf. First get clean prop forest df
prop_forest_sinac <- out %>% 
  unlist() %>% 
  as.data.frame() %>% 
  setNames('prop_forest_1km_sinac') %>% 
  tibble::rownames_to_column('codfrm')
prop_forest_sinac

# Join back to farms_sf and rename esa layer to distinguish
farms_sf <- farms_sf %>% 
  left_join(prop_forest_sinac)
get_str(farms_sf)



# Save and Clear ----------------------------------------------------------


# Farm spatial data
saveRDS(farms_sf, '2_clean/spatial/farms_sf.rds')
clear_data()
gc()
