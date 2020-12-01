###
# Get Data: bring in data from IMEA (http://www.imea.com.br/imea-site/relatorios-mercado)
# Data was supplied by Cleiton Gauer at IMEA, and contians sowing progress for corn, 
# soy and cotton since the 2008/9 season
###

library(tidyverse)
library(lubridate)
library(Quandl)
library(inspectdf)
library(nlme)
library(broom)

#Import sowing dates
semeadura_mt <- read_csv("./S204_Semeadura_v4.csv") %>%
  filter(!is.na(crop))

#Import harvest dates
colheita_mt <- read_csv("./S204_colheita_v2.csv") %>%
  filter(!is.na(crop))

#Bring in monthly weather data for MT
bra_month_mt <- read_csv("bra_month.csv") %>%
  filter(grepl("MT", geounit)) %>%
  select(geounit, prev_year, year, contains("dec"), contains("jan") , contains("feb"))

#bring in monthly soil moisture data
bra_soilm_mt <- read_csv("bra_soilm.csv") %>%
  filter(grepl("MT", geounit)) %>%
  mutate(year = year(fdate), 
         month = month(fdate, label = TRUE)) %>%
  select(-vdate, -fdate) %>%
  pivot_wider(names_from = month, values_from = soilm) %>%
  filter(year > 2006) %>%
  select(geounit, year, soilm_dec = Dec, soilm_jan = Jan, soilm_feb = Feb)

#Bring in daily weather data for MT
bra_daily_mt <- read_csv("bra_daily_mt.csv") 

#Bring in ayp, weather, sat imagery data
bra_go_latest_all_mt <- read_csv("bra_yield_latest_all_go_2020_11.csv") %>%
  filter(grepl("MT", geounit)) %>% #keep only Mato Grosso
  filter(year > 2006) %>% #keep relevant years
  filter(crop == "2ndcorn") %>% #only keep 2nd corn data
  select(year:state, contains("dec"), contains("jan"), contains("feb"), 
         contains("mar"), contains("apr")) 


