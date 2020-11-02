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
