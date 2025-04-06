#' Regressions

#' Multilevel models - climate variables as fixed factors and region as random
#'  intercept


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  DHARMa,
  purrr,
  ggplot2,
  stringr,
  skimr,
  lmtest,
  DescTools,
  lme4,
  lmerTest,
  multilevelTools,
  JWileymisc,
  tibble,
  sf,
  MuMIn,
  EMAtools,
  lattice,
  broom,
  clubSandwich,
  knitr,
  kableExtra,
  glmmTMB,
  multcomp,
  performance,
  merDeriv
)

conflicts_prefer(
  tidyr::expand(),
  .quiet = TRUE
)

# Full dataset
full <- readRDS('2_clean/full.rds')

# Farm data with spatial stuff
farms <- readRDS('2_clean/farm_spp_dat.rds')

# Spp trait and distribution data
dists <- readRDS('2_clean/dists_traits.rds')

# Collect results here
results <- list()



## Styling -----------------------------------------------------------------


source('3_functions/my_kbl.R')

satterthwaite <- function(kbl) {
  kbl %>% footnote(
    general_title = 'Note. ',
    general = paste0(
      "Marginal \\$R^2 = 0.345,\\$ conditional \\$R^2 = 0.348\\$. 
      Satterthwaite corrections reflected in standard errors, confidence 
      intervals, and p values."
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
}
  
  

## Wrangle -----------------------------------------------------------------


# Select only relevant variables from farms
get_str(farms)
dat <- farms %>%
  select(
    codfrm,
    region,
    elevation_m,
    starts_with('mean'),
    abundance_on_farm,
    richness_on_farm,
    shade_type,
    starts_with('prop_forest'),
    sub_uf,
    uf
  )
get_str(dat)

## Grand mean centering
# Save a version without centering
dat_nocenter <- dat
dat <- dat %>%
  mutate(across(c(
    elevation_m, 
    starts_with('mean'),
    prop_forest_1km_sinac
  ), 
   ~ scale(., center = TRUE, scale = TRUE)
  ))
get_str(dat)
get_str(dat_nocenter)



# ICCs --------------------------------------------------------------------


# Intra class correlation coefficients - 1 means all variance is between farms
# 0 means all variance at individual farm level
iccMixed(
  dv = "richness_on_farm",
  id = "region",
  data = dat
)
iccMixed(
  dv = "abundance_on_farm",
  id = "region",
  data = dat
)
# Richness has substantial variance at region level
# but abundance is almost ALL individual level


## Check for sub_uf instead of region
iccMixed(
  dv = "richness_on_farm",
  id = "sub_uf",
  data = dat
)
iccMixed(
  dv = "abundance_on_farm",
  id = "sub_uf",
  data = dat
)



# Richness ----------------------------------------------------------------
## *Try glmer ---------------------------------------------------------------


# Null model
glmer1 <- glmer(richness_on_farm ~ (1 | region), data = dat, family = 'poisson')
summary(glmer1)

# Add fixed effects*
glmer2 <- glmer(
  richness_on_farm ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type + 
    elevation_m + prop_forest_1km_sinac + (1 | region),
  data = dat, 
  family = 'poisson'
)
summary(glmer2)

# Take out region, add subuf?
test1 <- glm(
  richness_on_farm ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type + 
    elevation_m + prop_forest_1km_sinac + sub_uf,
  data = dat, 
  family = 'poisson'
)
summary(test1)
check_model(test1)

# Just subuf, shade, prop forest
# This is pretty decent*
test2 <- glmer(
  richness_on_farm ~ shade_type + prop_forest_1km_sinac + (1 | sub_uf),
  data = dat, 
  family = 'poisson'
)
summary(test2)
check_model(test2)

# What if we look at management variables only
test3 <- glmer(
  richness_on_farm ~ shade_type + prop_forest_1km_sinac + (1 | region),
  data = dat, 
  family = 'poisson'
)
summary(test3)
check_model(test3)
# Looks pretty good

# now natural variables only
test4 <- glmer(
  richness_on_farm ~ elevation_m + mean_spring_temp_c + mean_spring_precip_mm + (1 | sub_uf),
  data = dat, 
  family = 'poisson'
)
summary(test4)
check_model(test4)

# try without second level
test4 <- glm(
  richness_on_farm ~ elevation_m + mean_spring_temp_c + mean_spring_precip_mm 
    + sub_uf,
  data = dat, 
  family = 'poisson'
)
summary(test4)
check_model(test4)
# No good


# Add random effects
glmer3 <- glmer(
  richness_on_farm ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type + 
    elevation_m + prop_forest_1km_sinac + (mean_spring_precip_mm | region),
  data = dat, 
  family = 'poisson'
)
summary(glmer3)
# Singular with any random effect

# Compare
anova(glmer1, glmer2, glmer3)
# glmer2 is the best. This is what is reported in paper



## Diagnostics -------------------------------------------------------------


# Check residuals
par(mfrow = c(2, 2))
plot(glmer2)
par(mfrow = c(1, 1))
DHARMa::testResiduals(glmer2, plot = TRUE)

# Save DHARMA residuals
# png(
#   filename = '7_plots/residuals/richness_mlm_DHARMa.png',
#   width = 900,
#   height = 600,
#   res = 100
# )
# null <- DHARMa::testResiduals(glmer2)
# dev.off()


## Performance
check_model(glmer2)

# Save residual plots
png(
  filename = '7_plots/residuals/richness_mlm_residuals.png',
  width = 1000,
  height = 1000,
  res = 125
)
check_model(glmer2)
dev.off()



## Reporting ---------------------------------------------------------------


# Combine coefficients from lmerTest with confidence intervals to report
# First wrangle confidence intervals
# Note - using robust confidence intervals here
set.seed(42)
ints <- confint(glmer2)
ints_df <- ints %>% 
  as.data.frame() %>% 
  slice(-1) %>% 
  rownames_to_column() %>% 
  setNames(c('Variable', '2.5 %', '97.5 %'))
ints_df

# Now coefficients
coef_df <- summary(glmer2)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column('Variable')
coef_df

# Combine coefficients and confidence intervals
fix_df <- full_join(coef_df, ints_df) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  mutate(Variable = c(
    '(Intercept)',
    'Mean Temp (C)',
    'Mean Precip (mm)',
    'Shade: simple',
    'Shade: diverse',
    'Elevation (m)',
    'Forest (1km)'
  )) %>% 
  rename(
    z = `z value`,
    p = `Pr(>|z|)`
  ) %>% 
  select(
    Variable:z,
    matches('^[0-9]'),
    p
  ) %>% 
  mutate(' ' = ifelse(p < 0.01, '*', ' '))
fix_df

# Put in pseudo-R2 manually
summary(glmer2)
r2d <- r.squaredGLMM(glmer2)
r2_delta_marg <- r2d[1, 1] %>% round(3)
r2_delta_cond <- r2d[1, 2] %>% round(3)
r2_lr <- as.numeric(r.squaredLR(glmer2)) %>% round(3)

# Also level two variance and standard deviation manually
sum <- summary(glmer2)
var <- round(as.numeric(sum$varcor$region), 3)
sd <- round(as.numeric(attributes(sum$varcor$region)$stddev), 3)

# Make latex table
fix_df %>% 
  my_kbl(
    caption = 'Fixed effect parameters for richness regression',
    label = 'lmer_richness_fixed'
  ) %>%
  footnote(
    general_title = '',
    general = paste(
      'Generalized linear model with a poisson distribution and random effects for coffee region.',
      'All predictors are grand-mean centered. ',
      "Marginal \\$R^2 = ", r2_delta_marg, '\\$, ',
      'Conditional \\$R^2 = ', r2_delta_cond, '\\$, ',
      'LR Pseudo \\$R^2 = ', r2_lr, '\\$.',
      'Region variance \\$ = ', var, '\\$,',
      'region standard deviation \\$ = ', sd, '\\$.'
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(file = paste0('7_plots/latex/glmer_richness_fixed.tex'))



# Abundance ---------------------------------------------------------------
## Try glmer ---------------------------------------------------------------


glmer_1 <- glmer(abundance_on_farm ~ (1 | region), data = dat, family = 'poisson')
summary(glmer_1)

glmer_2 <- glmer(
  abundance_on_farm ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type +
    elevation_m + prop_forest_1km_sinac + (1 | region),
  data = dat, family = 'poisson')
summary(glmer2)

# Check diagnostics
simulateResiduals(glmer_2, plot = TRUE)
performance::check_model(glmer_2)
# This is no good - misspecified dispersion and zero inflation



## Try negbin glmmTMB -------------------------------------------------------


# Null model
glmmnb0 <- glmmTMB(
  abundance_on_farm ~ (1 | region), 
  data = dat, 
  family = nbinom1
)
summary(glmmnb0)
testResiduals(glmmnb0)
simulateResiduals(glmmnb0, plot = TRUE)
performance::check_model(glmmnb0)
# Misspecified dispersion and zero inflation

# Try adding zero inflation formula
glmmnb0 <- glmmTMB(
  abundance_on_farm ~ (1 | region), 
  data = dat, 
  family = nbinom1,
  ziformula = ~ .
)
# non positive definite hessian matrix

# Include fixed effects
glmmnb1 <- glmmTMB(
  abundance_on_farm ~ mean_spring_temp_c + mean_spring_precip_mm + elevation_m +
    shade_type + prop_forest_1km_sinac + (1 | region),
  data = dat,
  family = nbinom2
)
summary(glmmnb1)
simulateResiduals(glmmnb1, plot = TRUE)
performance::check_model(glmmnb1)
# Dispersion and zero inflation are the issue still
# Although DHARMa doesn't think it's half bad



## Try lmer ----------------------------------------------------------------


# Null model
m0 <- lmerTest::lmer(
  log(abundance_on_farm) ~ (1 | region),
  data = dat
)
summary(m0)
AIC(m0)

# Random intercept model with fixed effects
m1 <- lmerTest::lmer(
  log(abundance_on_farm) ~ mean_spring_temp_c + mean_spring_precip_mm  + 
    shade_type + elevation_m + prop_forest_1km_sinac + (1 | region),
  data = dat
)
summary(m1)
AIC(m1)

# Random slope model - allow random slopes for temp and elevation
m2 <- lmerTest::lmer(
  log(abundance_on_farm) ~ mean_spring_temp_c + mean_spring_precip_mm  + 
    shade_type + elevation_m + prop_forest_1km_sinac + 
    (mean_spring_temp_c | region),
  data = dat
)
summary(m2)
# m2 is singular

anova(m0, m1, m2)
# m1 is not significantly better than m0



## Try glm.nb --------------------------------------------------------------


# From here on out with single level models, using non centered data
nb1 <- MASS::glm.nb(
  abundance_on_farm ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type +
    elevation_m + prop_forest_1km_sinac + region,
  data = dat_nocenter
)
summary(nb1)
simulateResiduals(nb1, plot = TRUE)
performance::check_model(nb1)
# Not ideal, variance is heteregeneous, dispersion and zero infl are off



## *Try lm/glm -------------------------------------------------------------


# (This is what is reported in paper)
lm1 <- lm(
  log(abundance_on_farm) ~ mean_spring_temp_c + mean_spring_precip_mm + 
    shade_type + elevation_m + prop_forest_1km_sinac + region,
  data = dat_nocenter)
summary(lm1)
simulateResiduals(lm1, plot = TRUE)
performance::check_model(lm1)
# This might be best so far

# Try GLM Poisson
glm_1 <- glm(
  log(abundance_on_farm) ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type +
    elevation_m + prop_forest_1km_sinac + region,
  data = dat_nocenter,
  family = 'poisson'
)
summary(glm_1)
simulateResiduals(glm_1, plot = TRUE)
performance::check_model(glm_1)
# Quite bad

# Try with quasi
glm_2 <- glm(
  log(abundance_on_farm) ~ mean_spring_temp_c + mean_spring_precip_mm  + shade_type +
    elevation_m + prop_forest_1km_sinac + region,
  data = dat_nocenter,
  family = 'quasipoisson'
)
summary(glm_2)
performance::check_model(glm_2)
# This is okay, not great.
# Don't love the log trasnformed response variable in poisson distribution
# I think the plain old linear model is still the best though



## Diagnostics -------------------------------------------------------------


# Going with lm. It is only reasonable found

# Save residual plots from DHARMa
# png(
#   filename = '7_plots/residuals/abundance_lm_DHARMa.png',
#   width = 800,
#   height = 600,
#   units = 'px',
#   res = 100
# )
# null <- DHARMa::testResiduals(lm1, plot = TRUE)
# dev.off()

png(
  filename = '7_plots/residuals/abundance_lm_residuals.png',
  width = 1000,
  height = 1000,
  res = 125
)
check_model(lm1)
dev.off()



## Reporting ---------------------------------------------------------------


summary(lm1)
glance(lm1)
tidy(lm1)
confint(lm1)

# Make DF of results
abundance_df <- tidy(lm1) %>% 
  bind_cols(confint(lm1)) %>% 
  select(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    t = statistic,
    matches('%'),
    p = p.value
  ) %>% 
  mutate(
    ' ' = case_when(
      p < 0.01 ~ '*',
      .default = ' '
    ),
    across(where(is.numeric), ~ format(round(., 3), nsmall = 3)),
    Variable = c(
      '(Intercept)',
      'Mean Temp (C)',
      'Mean Precip (mm)',
      'Shade: simple',
      'Shade: diverse',
      'Elevation (m)',
      'Forest (1km)',
      'Region: Pérez Zeledón',
      'Region: Turrialba',
      'Region: Valle Central',
      'Region: Valle Occidental'
    )
  )

abundance_df

# make latex table
abundance_df %>% 
  my_kbl(
    caption = 'Parameters from abundance regression',
    label = 'lm_abundance'
  ) %>%
  footnote(
    general_title = '',
    general = paste(
      'Linear model with region as a fixed effect and log-transformed abundance as the dependent variable.',
      "Adj. \\$R^2 = 0.042, F = 1.585\\$ on 10 and 123 df, \\$p = 0.119\\$"
    ), 
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>% 
  save_kable(file = paste0('7_plots/latex/lm_abundance.tex'))

clear_data()
