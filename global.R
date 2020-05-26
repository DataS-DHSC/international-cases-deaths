### COVID-19 CASES AND DEATHS PLOT GENERATOR
library(shiny)
library(DT)
library(dplyr)
library(magrittr)
library(tidyr) 
library(forcats)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(ggthemes)
library(ggrepel)


#### FUNCTIONS FOR CLEANING & GETTING DATA ####
source('data_processing/data_cleaning.R')
source('data_processing/get_data.R')

#### GETTING COUNTRIES & INTERVENTIONS LIST ####
source('data/vis_settings.R')

#### PREPARING THE REST OF THE DATA ####
pop <- get_pop()
url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/", # JHU data location
              "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
              "time_series_covid19_", c('confirmed', 'deaths'), "_global.csv")

country_col <- as.character(color_palette$colour)
names(country_col) <- as.character(color_palette$country_region)

