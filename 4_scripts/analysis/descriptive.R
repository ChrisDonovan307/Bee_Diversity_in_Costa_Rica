# Descriptive Analysis

# At one time, this script held many graphs, tables, and visualizations. It has
# been reduced to only the figures that were used or reported in the paper. This
# includes a table we used to observe new species distributions, the key to 
# phytogeographic regions shown in the appendix, and the coffee and rarefied
# richness plot shown in the discussion.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  ggplot2,
  RColorBrewer,
  colorspace,
  tidyr,
  stringr,
  tibble,
  rlang,
  forcats,
  cowplot,
  knitr,
  kableExtra,
  stringr,
  ggpubr,
  reactable,
  sf,
  ggrepel
)


## Data without APIMEL
# Taxonomic ranks to use various places
taxa <- c('Family', 'Tribe', 'Genus', 'Species')
taxa_low <- str_to_lower(taxa)

# Load datsets, rename with taxa
full <- readRDS('2_clean/full.rds') %>% 
  rename_with(~ str_to_title(.x), all_of(taxa_low))
farm_dat <- readRDS('2_clean/farm_spp_dat.rds')
dists <- readRDS('2_clean/dists_traits.rds')
farms_sf <- readRDS('2_clean/spatial/farms_sf.rds') %>% 
  select(-geometry) %>% 
  as.data.frame()

# Replace old temp and precip values with
# This is jenky but we're doing it here
test <- farms_sf %>% 
  select(-mean_temp_c, -mean_precip_mm) %>% 
  rename(
    mean_temp_c = mean_spring_temp_c,
    mean_precip_c = mean_spring_precip_mm
  )

    
## Data with APIMEL
# Use this for tables, phylogenic tree only
full_api <- readRDS('2_clean/with_apimel/full_api.rds') %>% 
  rename_with(~ str_to_title(.x), all_of(taxa_low))

## Other datasets
# Rarefied richness by region
rare_rich_region <- readRDS('5_objects/rare_richness_by_region.rds')
# Coffee numbers
coffee <- readRDS('2_clean/coffee_production.rds')
# Aggregate pollinator efficiency by region
efficiency <- readRDS('5_objects/pollinator_efficiency_no_trigona.RDS')


## Prep results list, format settings, etc
# Initialize list of exploratory results
results <- list()

# Set ggplot theme
theme_set(
  theme_bw() + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
)

# Kable styling options
options(
  kable_styling_position = NULL,
  kable_styling_latex_options = c(
    'hold_position',
    'repeat_header'
  ),
  knitr.table.toprule = '\\hline\\hline',
  knitr.table.bottomrule = '\\hline\\hline'
)
font_size <- 9



# New Distributions -------------------------------------------------------


# Table not shown in paper, but this is where data came from.

# Show where a species was found in a province for the first time
str(dists)
dists_df <- dists %>% 
  select(codsp, matches('obs$|all$')) %>% 
  mutate(
    Alajuela = case_when(dist_Alajuela_obs == 1 & dist_Alajuela_all == 0 ~ 1),
    Cartago = case_when(dist_Cartago_obs == 1 & dist_Cartago_all == 0 ~ 1),
    Heredia = case_when(dist_Heredia_obs == 1 & dist_Heredia_all == 0 ~ 1),
    Puntarenas = case_when(dist_Puntarenas_obs == 1 & dist_Puntarenas_all == 0 ~ 1),
    SanJosé = case_when(dist_SanJosé_obs == 1 & dist_SanJosé_all == 0 ~ 1)
  ) %>% 
  select(codsp, order(everything())) %>% 
  select(-matches('Guanacaste|Lim|_obs$|_all$')) %>% 
  filter(rowSums(select(., -codsp), na.rm = TRUE) >= 1)
dists_df

dists_df %>% 
  reactable(
    resizable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    showPageSizeOptions = TRUE,
    bordered = TRUE,
    striped = TRUE,
    compact = TRUE,
    fullWidth = TRUE
  )



# Key to Phtyogeographic Regions ------------------------------------------


str(farms_sf)
farms_sf$uf %>% unique %>% length
farms_sf$sub_uf %>% unique %>% length
farms_sf$nombre %>% unique %>% length
# sub_uf is same as nombre. Use it for graphs, and include a table to compare

# Table to decode sub_uf
farms_sf %>% 
  select(sub_uf, nombre) %>% 
  unique() %>% 
  mutate(sub_uf = str_remove(sub_uf, 'SubUF') %>% as.numeric()) %>% 
  arrange(sub_uf) %>% 
  setNames(c('Unit', 'Name')) %>% 
  kbl(
    format = 'latex',
    longtable = FALSE,
    booktabs = TRUE,
    # align = c('cl'),
    align = c('c', 'p{5in}'),  # Use p{4cm} for the second column
    caption = 'Phytogeographic units represented in sampled farms',
    label = 'phyto_table',
    linesep = '',
    font_size = font_size,
  ) %>%
  kable_styling() %>%
  save_kable(
    file = paste0('7_plots/latex/phyto_table.tex')
  )



# Coffee by Rarefied Richness ---------------------------------------------


str(coffee)
str(rare_rich_region)

# Harmonize names
coffee_df <- coffee %>% 
  arrange(region) %>% 
  mutate(region = sort(rare_rich_region$region)) %>% 
  inner_join(rare_rich_region)
coffee_df

# Check correlation
cor.test(coffee_df$rare_rich, coffee_df$prod_per_ha_20_21)

set.seed(42)
coffee_plot <- coffee_df %>% 
  ggplot(aes(x = rare_rich, y = prod_per_ha_20_21, color = region)) +
  geom_point(
    size = 3
  ) +
  labs(
    x = 'Rarefied Richness',
    y = 'Coffee Yield 20-21 (fanegas/ha)',
    color = 'Region'
  ) +
  geom_label_repel(
    aes(label = region), 
    nudge_y = 0, 
    nudge_x = 0.5,
    point.padding = 5,
    force = 5
  ) +
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 30)) +
  theme(legend.position = 'none')
coffee_plot

# Save this
ggsave(
  filename = '7_plots/graphs/coffee_production.png',
  width = 600,
  height = 400,
  units = 'px',
  dpi = 125
)


clear_data()
