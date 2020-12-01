
library(caret)
library(elasticnet)

bra_go_latest_all_mt2 <- bra_go_latest_all_mt %>%
  select(-harvested, -state, -crop, -contains("wheat"), -contains("soy")) %>%
  left_join(
    select(ungroup(model_sem_4b), geounit, macroregion, nordeste, year, contains("xmid"), 
            contains("25"), -contains("std"), -contains("root")) 
  ) %>%
  #select(-season) %>%
  filter(year >2007) 

bra_go_latest_all_mt2_t <- bra_go_latest_all_mt %>%
  select(-harvested, -state, -crop, -contains("wheat"), -contains("soy")) %>%
  left_join(
    select(model_sem_4b, geounit, macroregion, nordeste, year, season, contains("xmid"), 
           contains("25"), -contains("std"), -contains("root"))
  ) %>%
  select(-season)%>%
  filter(year >2007 & year <2018)


varr <- bra_go_latest_all_mt2 %>%
  select(-yield) %>%
  colnames()

aa <- as.formula( paste("yield~ ", "(", paste( varr, collapse = " + " ), ")^2"))

set.seed(131313)

m <- train(aa,
           data = bra_go_latest_all_mt2_t,
           method = "pls",
           tuneLength = 40,
           na.action = na.omit,
           returnData = TRUE,
           preProcess = c('knnImpute', 'zv', 'nzv', 'center', 'scale'),
           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))

m2 <- train(aa,
           data = bra_go_latest_all_mt2_t,
           method = "lasso",
           tuneLength = 40,
           na.action = na.omit,
           preProcess = c('knnImpute', 'zv', 'nzv', 'center', 'scale'),
           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))

m3 <- train(aa,
            data = bra_go_latest_all_mt2_t,
            method = 'xgbLinear',
            #tuneLength = 40,
            na.action = na.omit,
            preProcess = (c('knnImpute','zv','nzv','center','scale')),
            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))

#for an LM, try a spec formula
set.seed(131313)
m4 <- train(yield ~ year + estimate_corn_xmid*precip_febmar + nordeste:(precip_febmar ) + 
              geounit*(ndv_mar + sum_edd_apr) + srad_mar + sum_edd_apr:estimate_corn_xmid,
            data = bra_go_latest_all_mt2_t,
            method = 'lm',
            #tuneLength = 40,
            na.action = na.omit,
            preProcess = (c('knnImpute','zv','nzv','center','scale')),
            trControl = trainControl(method = "repeatedcv", number = 20, repeats = 5))
m4 %>% summary()

m42 <- train(yield ~ year + estimate_corn_xmid + precip_janmar + 
            (ndv_mar + sum_edd_apr) + srad_mar + sum_edd_apr,
            data = bra_go_latest_all_mt2_t,
            method = 'lm',
            #tuneLength = 40,
            na.action = na.omit,
            preProcess = (c('knnImpute','zv','nzv','center','scale')),
            trControl = trainControl(method = "repeatedcv", number = 20, repeats = 5))
m42 %>% summary()

m4_lm <- lm(yield ~ year + estimate_corn_xmid*precip_febmar + nordeste:(precip_febmar ) + 
              geounit*(ndv_mar + sum_edd_apr) + srad_mar + sum_edd_apr:estimate_corn_xmid,
            data = bra_go_latest_all_mt2_t)

m4_lm2 <- lm(yield ~ year + estimate_corn_xmid + prcp_jan + nordeste*(precip_febmar) + 
  nordeste*(ndv_mar + sum_edd_apr) + srad_mar, 
data = bra_go_latest_all_mt2_t) 

summary(m4_lm2)
tidy(m4_lm2, conf.int = TRUE)

AICc(m4_lm2)

m4_lm3 <-lm(yield ~ year + estimate_corn_xmid + geounit*(precip_febmar ) + 
            geounit*(ndv_mar + sum_edd_apr) + srad_mar, 
            data = bra_go_latest_all_mt2_t)

AICc(m4_lm3)
tidy(m4_lm3, conf.int = TRUE)

plot(m$results[,1:2])
varImp(m4_lm2)

pred_tib <- extractPrediction(m)

(bra_go_latest_all_mt2_t) %>% 
  bind_cols(predict(m, newdata = bra_go_latest_all_mt2_t))

