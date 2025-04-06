# Pollinator efficiency

# Using mostly Munyuli et al 2014 to make estimates about relative efficiency
# between regions


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  ggplot2,
  ggpubr
)

source('3_functions/get_importance.R')
source('3_functions/my_kbl.R')

# Load data that includes APIMEL. Remove Turrialba from all sets
dat <- readRDS('2_clean/with_apimel/farm_spp_dat_api.rds')
full <- readRDS('2_clean/with_apimel/full_api.rds')

# Set ggplot theme
theme_set(
  theme_bw() + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
)

# Results list
res <- list()



# Efficiency and Importance -----------------------------------------------


get_str(dat)
get_str(full)

# No Turrialba, but with APIMEL and Trigona 
imp_no_turri <- get_importance(
  full, 
  with_turrialba = FALSE,
  with_apimel = TRUE,
  with_trigona = TRUE
)
imp_no_turri

# Without APIMEL
imp_no_turri_no_api <- get_importance(
  full, 
  with_turrialba = FALSE,
  with_apimel = FALSE,
  with_trigona = TRUE
)
imp_no_turri_no_api

# Without APIMEL nor trigona
imp_no_turri_no_api_no_tri <- get_importance(
  full, 
  with_turrialba = FALSE,
  with_apimel = FALSE,
  with_trigona = FALSE
)
imp_no_turri_no_api_no_tri



## Table -------------------------------------------------------------------

imp_no_turri

imp_no_turri %>% 
  setNames(c(
    'Region', 
    'Family', 
    'Tribe', 
    'Abundance', 
    'Efficiency', 
    'Rel. Abundance', 
    'Importance'
  )) %>% 
  mutate(across(
    c('Rel. Abundance', 'Importance'), 
    ~ format(round(.x, 3), nsmall = 3)
  )) %>% 
  my_kbl(
    caption = 'Pollinator importance by region',
    label = 'pollinator_importance',
    font_size = 8
  ) %>% 
  footnote(
    general_title = '',
    general = paste0(
      'Turrialba is not included in pollinator importance calculations. ',
      'Observations unidentified at the family level were removed. ',
      'Efficiency is the percentage of coffee flowers that yielded fruit after a single visit from a pollinator. ',
      'Pollinator importance is efficiency * relative abundance ',
      '(Munyuli et al., 2014).'
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(file = '7_plots/latex/pollinator_importance.tex')



# Regional Efficiency -----------------------------------------------------


# Get weighted averages in separate output
df_regional_efficacy <- imp_no_turri %>% 
  group_by(region) %>% 
  summarize(weighted_mean = weighted.mean(efficiency, abundance))
df_regional_efficacy

# Compare against regular average
imp_no_turri %>% 
  group_by(region) %>% 
  summarize(test = mean(efficiency))
# Checks out, higher when weighted

# Weighted averages without Trigona
df_no_trigona <- full %>% 
  get_importance(
    with_turrialba = FALSE,
    with_apimel = TRUE,
    with_trigona = FALSE
  ) %>% 
  group_by(region) %>% 
  summarize(weighted_mean = weighted.mean(efficiency, abundance))
df_no_trigona

# Save this to be used to make graph in descriptive.R
saveRDS(df_no_trigona, '5_objects/pollinator_efficiency_no_trigona.RDS')
  

clear_data()
