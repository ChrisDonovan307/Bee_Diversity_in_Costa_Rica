#' Single Level GLMs

#' Single level models - one way glms to show differences in abundance and 
#'  richness between regions. Does not include covariates


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  DHARMa,
  purrr,
  ggplot2,
  stringr,
  tibble,
  sf,
  broom,
  knitr,
  kableExtra,
  glmmTMB,
  multcomp,
  performance,
  ggpubr
)

# custom kbl tables
source('3_functions/my_kbl.R')

# Farm data with spatial stuff
dat <- readRDS('2_clean/farm_spp_dat.rds') %>% 
  mutate(region = as.factor(region))

# Results
out <- list()

# ggplot theme
theme_set(
  theme_bw() + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
)



# Richness ----------------------------------------------------------------


#' Show differences in richness between regions. This is raw richness, as it 
#' cannot be rarefied since we have farms with 1 observation
glm1 <- glm(richness_on_farm ~ region, data = dat, family = 'poisson')
summary(glm1)

# Diagnostics
par(mfrow = c(2, 2))
plot(glm1)
par(mfrow = c(1, 1))
simulateResiduals(glm1, plot = TRUE)
performance::check_model(glm1)

# Pairwise tests between regions
pairwise_regions <- glht(glm1, linfct = mcp(region = "Tukey"))
summary(pairwise_regions)

# Pull out CLD letters for graph
cld_out <- cld(pairwise_regions, level = 0.01)
cld_rich <- cld_out$mcletters$Letters %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  setNames(c('region', 'cld'))

# GLM output table for table
out$cld_rich_table <- tidy(glm1) %>% 
  mutate(
    across(c(estimate, std.error, statistic), ~ exp(.x)),
    term = str_remove(term, 'region'),
    ' ' = ifelse(p.value < 0.01, '*', ' '),
    p.value = format(round(p.value, 3), nsmall = 3)
  )
out$cld_rich_table



## Table -------------------------------------------------------------------


out$cld_rich_table %>% 
  mutate(across(where(is.numeric), ~ format(round(.x, 3), nsmall = 3))) %>% 
  my_kbl(
    caption = 'GLM of richness by region',
    label = 'glm_rich'
  ) %>%
  footnote(
    general_title = '',
    general = paste(
      'Poisson family distribution.', 
      'Residual deviance: 141.33 on 129 degrees of freedom.'
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(file = paste0('7_plots/latex/glm_rich.tex'))



## Plot --------------------------------------------------------------------


# Get standard deviations 
richness_ses <- dat %>% 
  group_by(region) %>% 
  summarize(se = sd(richness_on_farm) / sqrt(n()))

# Put coefficients and cld together, save to list
out$cld_rich_graph <- tidy(glm1) %>% 
  rename(region = term) %>% 
  mutate(
    region = ifelse(str_detect(region, 'Intercept'), 'Los Santos', region),
    region = str_remove(region, 'region')
  ) %>% 
  inner_join(cld_rich, by = 'region')
out$cld_rich_graph

# Make plot
out$cld_rich_plot <- dat %>% 
  group_by(region) %>% 
  summarize(richness = mean(richness_on_farm)) %>% 
  inner_join(out$cld_rich_graph, by = 'region') %>% 
  inner_join(richness_ses, by = 'region') %>% 
  ggplot(aes(x = region, y = richness, fill = region)) +
  geom_col(
    show.legend = FALSE,
    color = 'black'
  ) +
  geom_errorbar(
    aes(ymin = richness - se, ymax = richness + se),
    width = 0.25
  ) +
  geom_text(aes(label = cld, y = richness + se + 0.4)) +
  labs(
    x = 'Region',
    y = 'Mean Farm Richness'
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
out$cld_rich_plot



# Abundance ---------------------------------------------------------------


# Same thing for abundance
glm2 <- glm(abundance_on_farm ~ region, data = dat, family = 'poisson')
summary(glm2)

par(mfrow = c(2, 2))
plot(glm2)
par(mfrow = c(1, 1))
simulateResiduals(glm2, plot = TRUE)
performance::check_model(glm2)
# This is no good at all

# Try negbin
nb1 <- MASS::glm.nb(abundance_on_farm ~ region, data = dat)
summary(nb1)

par(mfrow = c(2, 2))
plot(nb1)
par(mfrow = c(1, 1))
simulateResiduals(nb1, plot = TRUE)
performance::check_model(nb1)
# Looks good

# Pairwise comparisons
pairwise_regions <- glht(nb1, linfct = mcp(region = "Tukey"))
summary(pairwise_regions)

# Pull out CLD letters for graph
cld_out <- cld(pairwise_regions, level = 0.01)
cld_abun <- cld_out$mcletters$Letters %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  setNames(c('region', 'cld'))

# Put coefficients and cld together, save to list
out$cld_abun_dat <- tidy(nb1) %>% 
  rename(region = term) %>% 
  mutate(
    region = ifelse(str_detect(region, 'Intercept'), 'Los Santos', region),
    region = str_remove(region, 'region')
  ) %>% 
  inner_join(cld_abun, by = 'region')
out$cld_abun_dat

# Get a clean table
out$cld_abun_table <- tidy(nb1) %>% 
  mutate(
    term = str_remove(term, 'region'),
    ' ' = ifelse(p.value < 0.05, '*', ' '),
    p.value = format(round(p.value, 3), nsmall = 3)
  )
out$cld_abun_table



## Table -------------------------------------------------------------------


out$cld_abun_table %>% 
  mutate(across(where(is.numeric), ~ format(round(.x, 3), nsmall = 3))) %>% 
  my_kbl(
    caption = 'GLM of abundance by region',
    label = 'glm_abun'
  ) %>%
  footnote(
    general_title = '',
    general = paste(
      'Negative binomial distribution.',
      'Residual deviance: 141.72 on 129 degrees of freedom.'
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(file = paste0('7_plots/latex/glm_abun.tex'))



## Plot --------------------------------------------------------------------


# Get SDs for abundance
abundance_ses <- dat %>% 
  group_by(region) %>% 
  summarize(se = sd(abundance_on_farm) / sqrt(n()))

out$cld_abun_plot <- dat %>% 
  group_by(region) %>% 
  summarize(abundance = mean(abundance_on_farm)) %>% 
  inner_join(out$cld_abun_dat, by = 'region') %>% 
  inner_join(abundance_ses, by = 'region') %>% 
  ggplot(aes(x = region, y = abundance, fill = region)) +
  geom_col(
    show.legend = FALSE,
    color = 'black'
  ) +
  geom_errorbar(
    aes(ymin = abundance - se, ymax = abundance + se),
    width = 0.25
  ) +
  geom_text(aes(label = cld, y = abundance + se + 1)) +
  labs(
    x = 'Region',
    y = 'Mean Farm Abundance'
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
out$cld_abun_plot



# Arrange plots ------------------------------------------------------------


plots <- ggarrange(
  out$cld_abun_plot,
  out$cld_rich_plot,
  labels = c('A', 'B')
)

ggsave(
  filename = '7_plots/graphs/glm_rich_abun.png',
  plot = plots,
  width = 6.5,
  height = 3.5,
  units = 'in',
  dpi = 300
)

