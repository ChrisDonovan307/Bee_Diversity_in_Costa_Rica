#' NMDS Plots 


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  vegan,
  ggplot2,
  tidyr,
  stringr,
  tibble,
  knitr,
  kableExtra,
  scales,
  indicspecies
)

# Pull in data. Note we are removing outliers: ELVTAL and VICAR2 
# Note we are replacing old temp and precip for new here
farm_dat <- readRDS('2_clean/farm_spp_dat.rds') %>%
  filter(! codfrm %in% c('ELVTAL', 'VICAR2')) %>% 
  select(-c(mean_temp_c, mean_precip_mm)) %>% 
  rename(
    mean_temp_c = mean_spring_temp_c,
    mean_precip_mm = mean_spring_precip_mm
  )
  
full <- readRDS('2_clean/full.rds') %>% 
  filter(! codfrm %in% c('ELVTAL', 'VICAR2'))
  

# List for results
results <- list()

# Custom kable latex tables
source('3_functions/my_kbl.R')



# Wrangle DFs -------------------------------------------------------------


# Wrangle a DF with abundance by species for NMDS 
nmds_spp_abundance <- full %>% 
  select(codfrm, codsp, abundance) %>%
  filter(
    str_detect(codsp, 'SPP$', negate = TRUE)
  ) %>%
  arrange(codsp) %>%
  pivot_wider(names_from = codsp, 
              values_from = abundance, 
              values_fn = ~ sum(.x),
              values_fill = 0) %>% 
  column_to_rownames('codfrm')
get_str(nmds_spp_abundance)

# And another farm level set with environmental variables for envfit
nmds_dat_farm_env <- farm_dat %>%
  filter(codfrm %in% rownames(nmds_spp_abundance)) %>% 
  select(
    codfrm,
    elevation_m,
    temp_c,
    wind,
    rain,
    shade_type,
    sky,
    region,
    mean_temp_c,
    mean_precip_mm,
    prop_forest_1km_sinac,
    uf,
    sub_uf,
    matches('flower')
  ) %>%
  arrange(codfrm)
get_str(nmds_dat_farm_env)



# Indicator Species -------------------------------------------------------


get_str(nmds_spp_abundance)

# Get vector of regions based on order of codfrm
region_vector <- nmds_spp_abundance %>% 
  rownames_to_column('codfrm') %>% 
  left_join(farm_dat, by = 'codfrm') %>% 
  pull(region)

indval <- multipatt(
  x = nmds_spp_abundance, 
  cluster = region_vector, 
  control = how(nperm = 999)
)
summary(indval, indvalcomp = TRUE)

# Extract only significant species to put in table below
sig_spp <- indval$sign %>% 
  rownames_to_column('codsp') %>% 
  select(codsp, p.value) %>% 
  filter(p.value < 0.05 | is.na(p.value))



## Latex Table -------------------------------------------------------------


## Save all results to Latex table
indval$sign %>% 
  rownames_to_column('codsp') %>% 
  setNames(c('codsp', 'LS', 'PZ', 'TU', 'VC', 'VO', 'index', 'stat', 'p')) %>% 
  mutate(stat = format(round(stat, 3), nsmall = 3)) %>% 
  filter(p < 0.05 | is.na(p)) %>%
  select(-index) %>% 
  my_kbl(
    caption = 'Indicator species',
    label = 'ind_spp',
    font_size = 9
  ) %>%
  footnote(
    general_title = '',
    general = paste0(
      "Only species that were significantly associated with one or more regions are shown. ",
      'Significant associations are shown with a 1. ',
      "Species with missing p values have the highest index of association for the set of all regions."
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(file = paste0('7_plots/latex/indicator_species.tex'))



# NMDS --------------------------------------------------------------------
## MetaMDS -----------------------------------------------------------------


get_str(nmds_spp_abundance)
set.seed(42)
(nmds <- metaMDS(nmds_spp_abundance, 
                 k = 3,
                 distance = 'jaccard',
                 try = 20, 
                 trymax = 100, 
                 wascores = TRUE,
                 trace = FALSE))

# Get site scores to color points
site_scores <- scores(nmds, display = 'sites')
group <- nmds_dat_farm_env$region



## Fit Region Table --------------------------------------------------------------


set.seed(42)
fit <- envfit(
  nmds ~ region,
  data = nmds_dat_farm_env,
  perm = 999,
  na.rm = TRUE,
  choices = c(1, 2)
)
fit

# Make DF of results
region_table <- fit$factors$centroids %>%
  as.data.frame() %>%
  rownames_to_column('Region') %>%
  mutate(
    Region = str_remove(Region, 'region'),
    across(where(is.numeric), ~ round(., 3))
  )
region_table
get_str(region_table)

# Save latex table
region_table %>%
  my_kbl(
    caption = 'Fit of region on ordination',
    label = 'nmds_region_fit'
  ) %>%
  footnote(
    general_title = 'Note. ',
    general = paste0(
      "R^2 = ",
      round(fit$factors$r, 3),
      ", p = ",
      round(fit$factors$pvals, 3)
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(file = paste0('7_plots/latex/nmds_region_fit.tex'))



## Fit Climate Table -------------------------------------------------------------


set.seed(42)
fit <- envfit(
  nmds ~ elevation_m + mean_temp_c + mean_precip_mm + shade_type + 
    prop_forest_1km_sinac + sub_uf,
  data = nmds_dat_farm_env, 
  perm = 999,
  na.rm = TRUE,
  choices = c(1, 2)
)

# Wrangle table to report 
factors <- fit$factors$centroids %>% 
  as.data.frame() %>% 
  rownames_to_column('Variable') %>% 
  mutate(
    R2 = c(
      round(fit$factors$r[1], 3), 
      rep(' ', 2),
      round(fit$factors$r[2], 3), 
      rep(' ', 10)
    ),
    p = c(
      round(fit$factors$pvals[1], 3), 
      rep(' ', 2),
      round(fit$factors$pvals[2], 3),
      rep(' ', 10)
    ),
    across(where(is.numeric), ~ format(round(., 3), nsmall = 3))
  )

vectors <- fit$vectors$arrows %>% 
  as.data.frame() %>% 
  rownames_to_column('Variable') %>% 
  mutate(
    R2 = round(fit$vectors$r, 3),
    p = round(fit$vectors$pvals, 3),
    across(where(is.numeric), ~ format(round(., 3), nsmall = 3)),
    Variable = c(
      'Elevation (m)', 
      'Mean Temp (C)', 
      'Mean Precip (mm)',
      'Forest (1km)'
    )
  )

# Combine them
env_fit_table <- bind_rows(vectors, factors) %>% 
  mutate(
    Variable = case_when(
      str_detect(Variable, 'type0$') ~ 'Shade: none',
      str_detect(Variable, 'type1$') ~ 'Shade: simple',
      str_detect(Variable, 'type2$') ~ 'Shade: diverse',
      .default = Variable
    ) %>% 
    str_remove_all('sub_uf')
  )

# Latex table
env_fit_table %>%
  my_kbl(
    caption = 'Fit of climate variables on ordination',
    label = 'nmds_climate_fit'
  ) %>%
  # footnote(
  #   general_title = 'Note. ',
  #   general = '',
  #   escape = FALSE,
  #   threeparttable = TRUE,
  #   footnote_as_chunk = TRUE
  # ) %>%
  save_kable(file = paste0('7_plots/latex/nmds_climate_fit.tex'))



## Plot with Ellipses ------------------------------------------------------


# Get site scores to color points
site_scores <- scores(nmds, display = 'sites')

# Set groups based on regions
group <- as.factor(nmds_dat_farm_env$region)

# Set colors to be the same between plots
colors <- scales::hue_pal()(length(unique(group)))

# PNG settings
png(
  filename = '7_plots/graphs/nmds_final.png',
  res = 300,
  width = 7,
  height = 5,
  units = 'in',
  pointsize = 12
)

layout(matrix(c(1, 1, 2, 2), nrow = 2, byrow = TRUE), heights = c(0.25, 2))

## Legend
# Set margins for legend
par(mar = c(0, 3, 1, 0))

# New plot space for legend on top
plot.new()

# Make legend
legend(
  "top",
  legend = levels(group),
  col = colors,
  pch = 16,
  pt.cex = 1.75,
  inset = c(-0.25, 0),
  horiz = TRUE
)

# Set margins for plots
par(mar = c(4, 4, 0, 1))
plot(
  site_scores[, 1],
  site_scores[, 2],
  col = colors[group],
  pch = 16,
  cex = 1,
  xlab = "NMDS1",
  ylab = "NMDS2",
  xlim = c(-2.5, 4),
  ylim = c(-3, 3)
)

ordiellipse(
  nmds, 
  group,
  kind = "sd", 
  conf = 0.90, 
  col = colors, 
  label = FALSE
)

## Indicator species
spp_scores <- scores(nmds, display = 'species')

# Filter species scores to significant species
sig_spp_scores <- spp_scores %>%
  as.data.frame() %>%
  filter(rownames(.) %in% sig_spp$codsp)

text(
  sig_spp_scores[, 1], 
  sig_spp_scores[, 2], 
  labels = rownames(sig_spp_scores),
  cex = 0.8,
  col = 'black'
)
dev.off()


# Clear environment
clear_data()
