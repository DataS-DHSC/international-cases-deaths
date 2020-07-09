get_pop <- function(){
  # Data source: UN Population division, 'Total Population at mid-year'
  # https://population.un.org/wup/DataQuery/
  pop <- readxl::read_xlsx('data/pop.xlsx', sheet = 1, skip = 16)
  pop_names <- c('index', 'variant', 'country_region', 'notes', 'country_code', 'type', 'parent_code')
  colnames(pop) <- c(pop_names, paste0('X', colnames(pop)[8:length(colnames(pop))]))
  pop <- pop %>%
    dplyr::select(tidyselect::all_of(c(pop_names, 'X2020'))) %>%
    dplyr::mutate(country_region = clean_country(country_region),
                  X2020 = as.numeric(X2020)*1000)
  return(pop)
}

get_data <- function(url, pop_df, colname){
  data <- readr::read_csv(url)
  colnames(data) <- c('province_state', 'country_region', 'lat', 'long', 
                      as.character(as.Date(colnames(data)[5:length(colnames(data))], 
                                           format = '%m/%d/%y')))
  data <- data %>%
    dplyr::mutate(country_region = clean_country(country_region)) %>%
    # We have chosen to visualise China's SARs as separate epidemiological entities
    dplyr::filter(province_state != 'Macau' | is.na(province_state)) %>% # Remove SAR
    dplyr::mutate(country_region = case_when( 
      province_state == 'Hong Kong' ~ 'Hong Kong', # Move other SAR to own category
      TRUE ~ country_region
    )) %>%
    dplyr::mutate(country_region = clean_country(country_region)) %>%
    pivot_data(., colname) %>% # Pivot to long-form
    dplyr::left_join(pop, by = 'country_region') # Add population data
  return(data)
}

roll_avg <- function(data, colname, rollnum = 3){
  data_roll <- data %>% # Create rolling averages columns
    dplyr::group_by(country_region) %>%
    dplyr::mutate(new = !!sym(colname) - lag(!!sym(colname)), # Create new cases/deaths column
                  new_percap = new / X2020,
                  roll = zoo::rollmean(new, rollnum, 
                                       align = 'right', 
                                       fill = NA),
                  roll_percap = zoo::rollmean(new_percap, rollnum, 
                                              align = 'right', 
                                              fill = NA)) %>%
    dplyr::filter(!is.na(roll)) %>%
    dplyr::ungroup()  %>%
    dplyr::mutate(percap = !!sym(colname) / X2020,
                  # roll = ifelse(roll == 0, NA, roll),
                  # roll_percap = ifelse(roll_percap == 0, NA, roll_percap),
                  type = colname)
  return(data_roll)
}

get_death_markers <- function(deaths, level, label){
  deaths %>%
    dplyr::filter(deaths >= level) %>%
    dplyr::group_by(country_region) %>%
    dplyr::summarise(date = min(as.Date(date))) %>%
    dplyr::mutate(lab = label,
                  date = as.character(date))
}

