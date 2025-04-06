#' Maps Static


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  mapview,
  sf,
  leaflet,
  stringr,
  tidyr,
  tmap,
  viridisLite,
  tmaptools,
  stars,
  RColorBrewer,
  maptiles,
  tmaptools,
  scales,
  cowplot
)

# Provinces for tiles and references
provinces <- readRDS('2_clean/spatial/provinces.rds')

# Spatial farm object
farms <- readRDS("2_clean/spatial/farms_sf.rds") %>% 
  select(-species_list) %>% 
  st_as_sf()

# Reorganize regions into levels to match other color schemes
regions <- unique(farms$region)

farms <- farms %>% 
  mutate(region = factor(
    region,
    levels = c(regions[1], regions[3], regions[5], regions[2], regions[4])
  ))
get_str(farms)

# Color scheme for regions
color_scheme <- hue_pal()(5) %>% 
  setNames(c('LS', 'PZ', 'TU', 'VC', 'VO'))
# "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"

# Initialize results list
results <- list()



# Reference ---------------------------------------------------------------


# Projections: 4326, 4269, 9822

# Pretty nice tool here
# tmaptools::palette_explorer()

# pacman::p_load(systemfonts)
# system_fonts()
# system_fonts() %>%
#   filter(str_detect(name, 'Times'))



# Tiles -------------------------------------------------------------------


# Get bounding box from farm points to make tiles
(bbox <- st_bbox(farms))

# Make it a bit bigger than 
extend_by <- 0.05

bbox[1] <- bbox[1] - extend_by  # xmin
bbox[2] <- bbox[2] - extend_by  # ymin
bbox[3] <- bbox[3] + extend_by  # xmax
bbox[4] <- bbox[4] + extend_by  # ymax

bbox

tiles <- get_tiles(bbox,
                   # provider = "CartoDB.Positron",
                   # provider = "Esri.WorldShadedRelief",
                   provider = "CartoDB.PositronNoLabels",
                   zoom = 11,
                   crop = TRUE)

# Save tiles just in case
saveRDS(tiles, '2_clean/spatial/tiles.rds')

# While we're here, crop a version of Provinces by the bbox so the labels
# come out properly. Have to crop by even wider so we don't break provinces
extend_by <- 0.1
bbox_prov <- bbox
bbox_prov[1] <- bbox[1] - extend_by  # xmin
bbox_prov[2] <- bbox[2] - extend_by  # ymin
bbox_prov[3] <- bbox[3] + extend_by  # xmax
bbox_prov[4] <- bbox[4] + extend_by  # ymax

bbox_prov

# Crop the provinces to use in the map
provinces_crop <- provinces %>% 
  st_make_valid() %>% 
  st_crop(bbox_prov) %>% 
  st_cast('MULTIPOLYGON')



# Farm Map ------------------------------------------------------------------


# Map of sampled farms. Color by region, size by richness
tmap_options(check.and.fix = TRUE)

farm_map <- 
  tm_shape(tiles) +
  tm_rgb() +
  
  tm_shape(provinces) +
  tm_borders(
    lwd = 1.5,
    lty = 'solid',
    col = 'black'
  ) +
  
  tm_shape(farms) +
  tm_symbols(
    size = 'richness_on_farm',
    scale = 2,
    col = 'region',
    palette = color_scheme,
    border.col = 'black',
    alpha = 0.7,
    title.col = 'Coffee Region',
    title.size = 'Species Richness',
    shapes.legend.fill = 'grey'
  ) +
  
  tm_shape(provinces_crop) +
  tm_text(
    text = 'province',
    ymod = c(-3, 0, -1, 3, 0, 4), # in order as they appear, not alphabetical
    xmod = c(3, -4, -1, -2, -2, -6)
  ) + 
  
  tm_add_legend(
    type = 'symbol'
  ) + 
  
  # tm_credits('Data: USDM\nTiles: CartoDB') +
  
  tm_layout(
    inner.margins = rep(0, 4),
    outer.margins = rep(0.01, 4),
    fontfamily = 'serif',
    legend.position = c('left', 'bottom'),
    legend.height = 0.5,
    legend.width = 0.25,
    legend.frame = TRUE,
    legend.frame.lwd = 1,
    legend.text.size = 0.9,
    legend.title.size = 1
  )

farm_map

# tmap_mode('view')
# tmap_mode('plot')


# CR Inset ----------------------------------------------------------------


cr_provinces <- st_cast(provinces, 'POLYGON') %>%
  mutate(sq_km = st_area(.)/1000) %>%
  {attributes(.$sq_km) <-  NULL; .} %>%
  filter(sq_km > 50000)

cr <- st_read('1_raw/spatial/gadm/gadm41_CRI_1.shp')
get_str(cr)

cr <- cr[1] %>% 
  st_crop(cr_provinces)
plot(cr)

# Box
bbox_polygon <- st_as_sf(st_sfc(st_polygon(list(rbind(
  c(bbox[1], bbox[2]),
  c(bbox[3], bbox[2]),
  c(bbox[3], bbox[4]),
  c(bbox[1], bbox[4]),
  c(bbox[1], bbox[2])
)))), crs = 4326)

# Map
cr_map <- 
  
  tm_shape(cr) +
  tm_polygons(
    col = '#F5F5F5',
    border.col = 'black',
    lwd = 1
  ) +
  
  tm_shape(bbox_polygon) +
  tm_borders(
    lwd = 2,
    col = 'black'
  ) +
  
  tm_layout(inner.margins = rep(0, 4),
            fontfamily = 'serif',
            legend.show = FALSE)

cr_map



# Combine -----------------------------------------------------------------


farm_grob <- tmap_grob(farm_map)
cr_grob <- tmap_grob(cr_map)

farms_full <- ggdraw(clip = 'on') +
  draw_plot(farm_grob) +
  draw_plot(cr_grob,
            height = 0.25,
            x = -0.135,
            y = 0.023
  )
farms_full



# Save --------------------------------------------------------------------


asp <- get_asp_ratio(farm_map)
width <- 5.5

ggsave2(
  filename = '7_plots/static_maps/map_farm_richness.png',
  plot = farms_full,
  width = width,
  height = width / asp,
  units = 'in',
  dpi = 300,
  scale = 1.25
)


# Clear environment
clear_data()
gc()
