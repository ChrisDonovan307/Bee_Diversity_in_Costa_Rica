# Bee Diversity in Costa Rica: A National Survey of Coffee Agroecosystems
# Table of Contents

# README
knitr::knit('README.Rmd')


# Cleaning ----------------------------------------------------------------


# Clean appendix files - sampled farms, spp identification, and species traits
source('4_scripts/cleaning/clean_a2.R')
source('4_scripts/cleaning/clean_a4.R')
source('4_scripts/cleaning/clean_a5.R')

# Joins and wrangles to get working datasets, and removing APIMEL
source('4_scripts/cleaning/joins.R')



# Prep For Analysis -------------------------------------------------------


# Prepping spatial layers, joining to farm data. Takes a few minutes to run.
source('4_scripts/analysis/spatial_wrangling.R')

# Hill and shannon numbers, rarefaction, joining to farm_dat dataset
# Note: splitting farms DF into a spatial layer and non-spatial layer here
source('4_scripts/analysis/rarefaction_and_functional_hill.R')

# ICAFE Production
source('4_scripts/analysis/coffee_production.R')



# Analysis ----------------------------------------------------------------


# Tables and plots
source('4_scripts/analysis/descriptive.R')

# Cladogram of observed species
source('4_scripts/cladogram.R')

# One-way GLMs on abundance and richness
source('4_scripts/analysis/single_level_glms.R')

# Static Map
source('4_scripts/maps_static.R')

# NMDS Plots
source('4_scripts/analysis/nmds_and_indicator_spp.R')

# Mixed model regressions with fixed effects for climate variables
source('4_scripts/analysis/mixed_model_regressions.R')

# Pollinator efficiency
source('4_scripts/analysis/efficiency.R')



# Miscellany -------------------------------------------------------------


# Clean datasets for publication on Dataverse
source('4_scripts/clean_data_for_pub.R')
