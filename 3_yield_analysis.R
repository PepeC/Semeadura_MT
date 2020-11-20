
library(caret)
library(elasticnet)

bra_go_latest_all_mt2 <- bra_go_latest_all_mt %>%
  select(-harvested, -state, -crop, -contains("wheat"), -contains("soy")) %>%
  left_join(
    select(ungroup(model_sem_4b), geounit, macroregion, year, contains("xmid"), 
            contains("25"), -contains("std"), -contains("root")) 
  ) %>%
  #select(-season) %>%
  filter(year >2007) 

bra_go_latest_all_mt2_t <- bra_go_latest_all_mt %>%
  select(-harvested, -state, -crop, -contains("wheat"), -contains("soy")) %>%
  left_join(
    select(model_sem_4b, geounit, macroregion, year, season, contains("xmid"), 
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
           preProcess = (c('knnImpute','zv','nzv','center','scale')),
           trControl = trainControl(method = "repeatedcv",number = 5,repeats = 5))

m2 <- train(aa,
           data = bra_go_latest_all_mt2_t,
           method = "lasso",
           tuneLength = 40,
           na.action = na.omit,
           preProcess = (c('knnImpute','zv','nzv','center','scale')),
           trControl = trainControl(method = "repeatedcv",number = 5,repeats = 5))


m3 <- train(aa,
            data = bra_go_latest_all_mt2_t,
            method = 'xgbLinear',
            #tuneLength = 40,
            na.action = na.omit,
            preProcess = (c('knnImpute','zv','nzv','center','scale')),
            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))

#for an LM, try a spec formula
m4 <- train(yield ~ geounit *(prcp_jan + estimate_corn_xmid +  precip_febmar) + 
              geounit*ndv_mar + tmax_apr,
            data = bra_go_latest_all_mt2_t,
            method = 'lm',
            #tuneLength = 40,
            na.action = na.omit,
            preProcess = (c('knnImpute','zv','nzv','center','scale')),
            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
m4 %>% summary()

m4_lm <- lm(yield ~ geounit*prcp_jan + estimate_corn_xmid*prcp_jan + 
  geounit*ndv_mar + tmax_apr, 
data = bra_go_latest_all_mt2_t) 

plot(m$results[,1:2])
varImp(m)

pred_tib <- extractPrediction(m)

(bra_go_latest_all_mt2_t) %>% 
  bind_cols(predict(m, newdata = bra_go_latest_all_mt2_t))

