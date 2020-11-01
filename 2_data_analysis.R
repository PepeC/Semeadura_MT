library(nlme)
library(broom)
library(tidyverse)
library(minpack.lm)

source("1_get_data.R")

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


# Take a look at the data: Pace by end of October in MT
semeadura_mt_w %>%
  filter(crop == "soy") %>%
  filter(month == 10 ) %>%
  filter(macroregion == "Mato Grosso") %>%
  group_by(year) %>%
  filter(doy == max(doy))


#Take a look at the data: Plot of sowing pace by district
semeadura_mt_w %>%
  filter(crop == "soy") %>%
  filter(week > 10) %>%
  ggplot(aes(doy, val)) + 
  geom_line(aes(colour = season)) +
  xlab("Day fo Year") + ylab("Sowing progress (%)") +
  facet_wrap(~macroregion) + ggtitle("Soy sowing pace in Mato Grosso (2008/09 - 2019/20")

# How do we model this? Logistic regression!
# We use the minpack.lm package to fit non-0linear regressions to a 
# form of the logistic function described in Pinheiro and Bates (in package nlme)
# The xmid parameter will be key so we can compare when 50% of sowings are complete.


#Test to get parameter starting value ideas
model_sem <- semeadura_mt_w %>%
  filter(crop == "soy" & macroregion == "Nordeste" & year == 2016)

model_test <- gnls(val ~ SSlogis(doy_c, Asym, xmid, scal), model_sem)

#Now try
model_sem_nest <- semeadura_mt_w %>%
  group_by(crop, macroregion, season) %>%
nest()
#Now run with do-catch
model_sem2 <- semeadura_mt_w %>%
  group_by(crop, macroregion, season) %>%
  do( model = tryCatch(nlsLM(val ~ Asym/(1 + exp((xmid - doy_c)/scal)), 
                             start = list( Asym = 102, xmid = 53, scal = 8),
                             lower = c(Asym = 99, xmid = 2, scal = -20), 
                             upper = c(Asym = 105, xmid = 70, scal = 20), 
                             control= c(maxiter=100),
                             data=.)))  

augment(model_sem2, model) %>%
  filter(macroregion == "CentroSul" & crop == "corn") %>%
  ggplot(aes(doy_c, val)) + geom_point() + geom_line(aes(doy_c, .fitted)) + facet_wrap(~season)


tidy(model_sem2, model) %>%
  filter(term == "xmid") %>%
  select(crop:std.error) %>%
  pivot_wider(names_from = c(crop,term), values_from = c(estimate, std.error)) %>%
  ggplot(aes(estimate_soy_xmid,estimate_corn_xmid)) + geom_point(aes(colour = season)) +
  facet_wrap(~macroregion) + theme_bw()
  
#Define this function to find the days after sowing related to any % completion
findInt <- function(model, value) {
  function(x) {
    predict(model, data.frame(doy_c = x), type="response") - value
  }
}

#Define days to test
prct_sowing <- 75

#now calculate for each crop x macroregion x year combo
model_sem3 <- left_join(model_sem_nest, model_sem2) %>%
  group_by(crop, macroregion, season) %>%
  mutate(
    root_75 = map2(data, model, ~uniroot(findInt(.y, prct_sowing), range(.x$doy_c))$root)
  )

unnest(model_sem3, root_75) 

model_sem_roots<- model_sem3 %>% 
  unnest(root_75) %>%
  select(crop, season, macroregion, root_75)

#Now try plot again, but this time with the 75% completion for each one
model_sem_4 <- model_sem2 %>%
  tidy(model) %>%
  filter(term == "xmid") %>%
  select(crop:std.error) %>% 
  left_join(model_sem_roots) %>%
  pivot_wider(names_from = c(crop,term), values_from = c(estimate, std.error, root_75))
  
#now plot
  model_sem_4 %>%
  ggplot(aes(root_75_soy_xmid, estimate_corn_xmid)) + geom_point(aes(colour = season)) +
  facet_wrap(~macroregion) + theme_bw()

#Now do a quick regression just to get an idea

  model_sem_4b <-  model_sem_4 %>% filter(!macroregion == "Mato Grosso")
  
model_lm_75 <- lm(estimate_corn_xmid ~ macroregion + estimate_soy_xmid, data = model_sem_4b)

glance(model_lm_75)

augment(model_lm_75, newdata = model_sem_4b) %>%
  ggplot(aes(estimate_soy_xmid, estimate_corn_xmid)) + 
  geom_point(aes(colour = season)) +
  geom_line(aes(estimate_soy_xmid, .fitted)) +
  geom_ribbon(aes(ymin=.fitted - 2*.se.fit, ymax=.fitted + 2*.se.fit), linetype=2, alpha=0.5) +
  facet_wrap(~macroregion) + theme_bw()

  
  
  