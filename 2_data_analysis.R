library(nlme)
library(broom)
library(tidyverse)

#wrangle semeadura data
semeadura_mt_w <- semeadura_mt %>%
  filter(!crop == "cotton") %>%
  pivot_longer(c(`CentroSul`:`Mato Grosso`), names_to = "macroregion", values_to = "val") %>%
  mutate(val = as.numeric(val), 
         val = val*100,
         year = year(date),
         month = month(date),
         week = week(date), 
         doy = yday(date)) %>%
  group_by(crop, season, macroregion) %>%
  mutate(min_year = min(year), 
         sow_start = ymd(paste(min_year, ifelse(crop == "cotton", "12",
                                                ifelse(crop == "corn", "01", "09")), "01", sep = "-"))) %>%
  ungroup() %>%
  mutate(doy_c = ifelse(year>min_year, doy + (365-yday(sow_start)), doy - yday(sow_start))) 


semeadura_mt_w %>%
  filter(crop == "soy") %>%
  filter(week > 10) %>%
  ggplot(aes(doy, val)) + 
  geom_line(aes(colour = season)) + 
  facet_wrap(~macroregion)


#Pace by end of OCtober in MT
semeadura_mt_w %>%
  filter(crop == "soy") %>%
  filter(month == 10 ) %>%
  filter(macroregion == "Mato Grosso") %>%
  group_by(year) %>%
  filter(doy == max(doy))

semeadura_mt_w %>%
  filter(crop == "soy") %>%
  arrange(crop, season, macroregion, val) %>%
  group_by(year, macroregion) %>%
  mutate(week.prog = val - lag(val)) %>%
  filter(macroregion == "Mato Grosso") %>%
  filter(week.prog > 20)

#Test to get parameter starting value ideas
model_sem <- semeadura_mt_w %>%
  filter(crop == "soy" & macroregion == "Nordeste" & year == 2016)

gnls(val ~ SSlogis(doy_c, Asym, xmid, scal), model_sem)

#Now run with do-catch
model_sem <- semeadura_mt_w %>%
  group_by(crop, macroregion, season) %>%
  do( model = tryCatch(nlsLM(val ~ Asym/(1 + exp((xmid - doy_c)/scal)), 
                             start = list( Asym = 102, xmid = 53, scal = 8),
                             lower = c(Asym = 99, xmid = 2, scal = -20), 
                             upper = c(Asym = 105, xmid = 70, scal = 20), 
                             control= c(maxiter=100),
                             data=.)))  

augment(model_sem, model) %>%
  filter(macroregion == "CentroSul" & crop == "corn") %>%
  ggplot(aes(doy_c, val)) + geom_point() + geom_line(aes(doy_c, .fitted)) + facet_wrap(~season)


tidy(model_sem, model)

