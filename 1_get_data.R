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

#Bring in monthly weather data for MT
bra_month_mt <- read_csv("bra_month.csv") %>%
  filter(grepl("MT", geounit)) %>%
  select(geounit, prev_year, year, contains("dec"), contains("jan") , contains("feb"))

#Bring in daily weather data for MT
bra_daily_mt <- read_csv("bra_daily_mt.csv") %>%
  filter(vmnth == "Jan" | vmnth == "Dec" | vmnth == "Feb") %>%
  filter(vdoy > 354 | vdoy < 55) %>%
  mutate(vyear = ifelse(vmnth == "Dec", vyear + 1, vyear),
         raind = ifelse(prcp > 0.5, 1, 0)) %>% # create a rule for how many days it rains
  group_by(geounit, vyear) %>%
  summarise(.groups = "keep",
    sum_prcp = sum(prcp),
    sum_rday = sum(raind)) %>%
  rename(year = vyear)

#Bring in ayp, weather, sat imagery data

bra_go_latest_all_mt <- read_csv("bra_yield_latest_all_go_2020_11.csv") %>%
  filter(grepl("MT", geounit)) %>% #keep only Mato Grosso
  filter(year > 2006) %>% #keep relevant years
  filter(crop == "2ndcorn") %>% #only keep 2nd corn data
  select(year:state, contains("dec"), contains("jan"), contains("feb"), 
         contains("mar"), contains("apr")) 


