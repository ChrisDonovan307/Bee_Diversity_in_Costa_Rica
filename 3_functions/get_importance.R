# get importance
# 2024-10-20

pacman::p_load(dplyr)

# Input is full dataset
get_importance <- function(df, 
                           with_turrialba = TRUE,
                           with_apimel = TRUE, 
                           with_trigona = TRUE){
  
  # Filter based on arguments
  if (with_turrialba == FALSE) {
    df <- filter(df, region != 'Turrialba')
  }
  
  if (with_apimel == FALSE) {
    df <- filter(df, codsp != 'APIMEL')
  }
  
  if (with_trigona == FALSE) {
    df <- filter(df, genus != 'Trigona')
  }
  
  # Get pollinator importance
  df <- df %>% 
    filter(order == 'Hymenoptera') %>% 
    group_by(region, family, tribe) %>% 
    summarize(abundance = sum(abundance)) %>% 
    mutate(
      efficiency = case_when(
        tribe == 'Meliponini' ~ 92.1,
        tribe == 'Apini' ~ 83.8,
        tribe == 'Halictini' ~ 69.9,
        tribe == 'Xylocopini' ~ 75.3,
        .default = case_when(
          family == 'Apidae' ~ 81.5,
          family == 'Halictidae' ~ 64.8,
          family == 'Megachilidae' ~ 75.6,
          .default = NA
        )
      )
    ) %>% 
    filter(family != 'Unknown') %>% 
    group_by(region) %>%
    mutate(
      rel_abundance = abundance / sum(abundance),
      importance = efficiency * rel_abundance
    ) %>% 
    arrange(region, desc(importance)) %>% 
    ungroup()
  
  return(df)
}
