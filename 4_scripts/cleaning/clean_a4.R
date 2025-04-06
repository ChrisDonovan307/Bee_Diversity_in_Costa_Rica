# Cleaning data from Appendix 4 - Species Identification


# Load Packages and Data --------------------------------------------------


pacman::p_load(
    janitor,
    skimr,
    dplyr,
    stringr,
    stringr
)

dat <- read_csv('1_raw/Appendix 4. Spp_identification.csv') |> 
    clean_names()

skim(dat)
# 710 records, each is one species in one farm.
# Turrialba region is missing collector, date, report  (251 rows). A single 
#   collector missing from a farm in Los Santos makes it 252 for collector. 
#   Everything else looks complete!

# Classes all look good too. Not much to do here!

get_str(dat)



# Dealing with duplicates -------------------------------------------------


# Check for duplicated records
any(duplicated(dat))
sum(duplicated(dat))
# 11 duplicated

# Check for duplicates in all but abundance, report, collector
no_abun <- select(dat, -c(abundance, report, collector))
any(duplicated(no_abun))
sum(duplicated(no_abun))
(mismatches <- which(duplicated(no_abun)))
# 45 records

# Which farms are they?
mismatch_farms <- dat[mismatches, ] |> 
    select(codfrm, region)
mismatch_farms
# Mostly Turrialba, but 8 from Los Santos as well

# Combine them
dat <- dat %>%
    group_by(across(-c(abundance, collector, report))) |> 
    summarize(abundance = sum(abundance),
              collector = paste(collector, collapse = ', ')) |> 
    ungroup()

get_str(dat)
# Down to 665 records

any(duplicated(dat))
# Clean



# Explore and Clean Variables ---------------------------------------------


explore('region')
explore('farm')
explore('codfrm')
# 140 unique farms

range(dat$date, na.rm = TRUE)
hist(dat$date, breaks = 'weeks')

hist(dat$latitude)
hist(dat$longitude)

dat <- rename(dat, elevation_m = elevation)
hist(dat$elevation_m)

explore('collector')
explore('order')
explore('family')
explore('species')

# lophocoriphe should be lophocoryphe
dat$species <- str_replace_all(dat$species, 'lophocoriphe', 'lophocoryphe')

# also let's make sure all species names are lower case (unknown is capital)
dat$species <- str_to_lower(dat$species)

explore('common_name')
explore('codsp')
# 60 unique species identified

range(dat$abundance)
table(dat$abundance, useNA = 'always')
# One farm has nearly twice the abundance of any other

dat |> 
    slice_max(abundance) |> 
    select(region, farm, codfrm, abundance)
# JOSMAT has 85 abundance

dat |> 
    group_by(codsp) |> 
    summarize(total_abundance = sum(abundance)) |> 
    arrange(desc(total_abundance))
# Neat!



# Save and Clear ----------------------------------------------------------


saveRDS(dat, '2_clean/a4_spp_identification.rds')
clear_data()
