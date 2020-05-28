clean_country <- function(country_vec){
  country_vec <- case_when(
    country_vec == 'Republic of Korea' ~ 'South Korea',
    country_vec == 'Korea, South' ~ 'South Korea', 
    country_vec == 'Korea Republic of' ~ 'South Korea',
    country_vec == 'United Kingdom' ~ 'UK',
    country_vec == 'United States of America' ~ 'US',
    country_vec == 'USA' ~ 'US',
    country_vec == 'Iran (Islamic Republic of)' ~ 'Iran',
    country_vec == 'China, Taiwan Province of China' ~ 'Taiwan',
    country_vec == 'China, Hong Kong SAR' ~ 'Hong Kong',
    TRUE ~ country_vec)
  return(country_vec)
}

pivot_data <- function(df, values_to){
  df %>%
    dplyr::group_by(country_region) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::select(-lat, -long) %>%
    tidyr::pivot_longer(cols = contains('-'),
                        names_to = 'date',
                        values_to = values_to)
}

percap_calc <- function(vec, pop, input_name){
  if(input_name == 'per capita') {
    percap = vec / pop
  } else if (input_name == 'per 1000') {
    percap = vec / (pop / 1000)
  } else if (input_name == 'per million') {
    percap = vec / (pop / 1000000)
  } else {
    percap = vec
  }
  return(percap)
}
