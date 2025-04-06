#' Make catalog dummies
#' 2024-07-21
#' 
#' Function to parse out the distributions across a given catalog and make
#' a set of dummy variables, one for each province, showing where a species
#' has been found. The function should be mapped over a list of catalogs to get
#' all 21 columns made.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  purrr,
  dplyr,
  stringr,
  rlang
)


# Function ----------------------------------------------------------------


make_catalog_dummies <- function(df, 
                                 catalog = c('moure', 'disc', 'nat'), 
                                 provinces = provinces) {
  
  # Make new names
  new_names <- map(provinces, ~ {
    paste0('dist_', str_replace(.x, ' ', ''), '_', catalog)
  })
  
  # Make a column for each province
  walk2(new_names, provinces, ~ {
    pattern <- paste0('(?<!except\\s)', .y, '|All provinces(?!.*', .y, ')')
    variable_name <- paste0('dist_', catalog)
    
    df[[.x]] <<- case_when(
      str_detect(df[[variable_name]], pattern) ~ 1,
      .default = 0
    )
  })
  
  return(df)
}



# All Distributions -------------------------------------------------------


# To see whether a species has been found in a province by ANY catalog. Run this
# once catalogs are made.

get_all_dist <- function(df, provinces = provinces) {
  
  new_names <- map(provinces, ~ {
    paste0('dist_', str_replace(.x, ' ', ''), '_all')
  })
  
  walk2(new_names, provinces, ~ {
    
    # Pattern to pull dist columns from that province
    pattern <- paste0('dist_', .y) %>% 
      str_remove(' ')
    
    # For each province, get row sums for each of three cols in that province
    # Then, if at least 1 has shown presence in province, give it a 1, if not, 0
    df <<- df %>% 
      mutate(!!sym(.x) := ifelse(rowSums(select(., matches(pattern))) >= 1, 1, 0))
  })
  return(df)
}

