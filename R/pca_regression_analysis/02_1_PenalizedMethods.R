
library(tidyverse)
library(glmnet)
library(faux)
library(lmtest)
library(car)


#LOAD DATA BASE
pca_regression_df_ready_to_use <- readRDS(
  "../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")

set.seed(0)

### LOG FOR POSITIVE VALUES VARIABLES ----

onlynumbersbase <- pca_regression_df_ready_to_use %>% mutate(
  i_name_urban_concentration=NULL,i_name_uca_case=NULL,i_code_urban_concentration=NULL)

prblmtc_detect <- function(x) {
  
  return(ifelse(x %>% min() <= 0,FALSE,TRUE))
  
}

basenumberlog <- onlynumbersbase %>%
  dplyr::mutate_if(prblmtc_detect,log)

xvector <- basenumberlog %>% mutate(y_fuel_consumption_per_capita_2010=NULL,
                                                  y_wghtd_mean_commute_time=NULL,
                                                  i_code_urban_concentration=NULL,i_name_uca_case=NULL,
                                                  i_name_urban_concentration=NULL,d_large_uca_pop=NULL,
                                                  d_tma=NULL)

### TRAINING SET ----
#### HERE WE OBTAIN THE BEST LAMBDA TO REDUCE RSME THROUGH CROSS VALIDATION

train_rows <- sample(1:184, 0.66*184)

yfuel <- basenumberlog$y_fuel_consumption_per_capita_2010
ycomute <- basenumberlog$y_wghtd_mean_commute_time

x.train <- as.matrix(xvector[train_rows,])
x.test <- as.matrix(xvector[-train_rows,])

y.train <- as.matrix(yfuel[train_rows])
y.test <- as.matrix(yfuel[-train_rows])

## RIDGE REGRESSION

alpha0.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha=0, family="gaussian", nfolds = 10)

alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se,newx = x.test)

mean((y.test - alpha0.predicted)^2)

### LASSO REGRESSION

alpha1.fit <- cv.glmnet(x.train,y.train,type.measure = "mse",alpha=1,family="gaussian",nfolds = 10)

alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se,newx = x.test)

mean((y.test - alpha1.predicted)^2)

### ELASTIC NET

alpha0.5fit <- cv.glmnet(x.train,y.train,type.measure = "mse",alpha=0.5,family="gaussian",nfolds = 10)
alpha0.5predicted <- predict(alpha0.fit, s=alpha0.5fit$lambda.1se,newx = x.test)

mean((y.test-alpha0.5predicted)^2)

#### AVAILING THE BEST ALPHA ----

list.of.fits <- list() 
for (i in 0:10) { fit.name <- paste0("alpha",i/10)
                          list.of.fits[[fit.name]] <- 
                          cv.glmnet(x.train,y.train,type.measure = "mse",
                                      alpha=i/10, family="gaussian")}

results <- data.frame()
for (i in 0:10) { fit.name <- paste0("alpha",i/10)
 predicted <- predict(list.of.fits[[fit.name]],s=list.of.fits[[fit.name]]$lambda.1se,newx = x.test)
 mse <- mean((y.test - predicted)^2)
 temp <- data.frame(alpha = i/10, mse=mse,fit.name = fit.name)
 results <- rbind(results,temp)
 }

results

alpha0.6.fit <- cv.glmnet(x.train,y.train,type.measure = "mse",alpha=0.6,family="gaussian",nfolds = 10)

alpha0.6.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se,newx = x.test)

mean((y.test - alpha1.predicted)^2)


#### PENALIZAÇOES ----

### ELASTIC

Elastic_modelfuel <- glmnet(xvector, yfuel, alpha = 0.5, lambda = alpha0.5fit$lambda.min)

coef(Elastic_modelfuel)

##### LASSO

Lasso_modelfuel <- glmnet(xvector, yfuel, alpha = 1, lambda = alpha1.fit$lambda.min)

coef(Lasso_modelfuel)

#### RIDGE

Ridge_modelfuel <- glmnet(xvector, yfuel, alpha = 0, lambda = alpha0.fit$lambda.min)

coef(Ridge_modelfuel)

##### REGRESSÃO COM MELHOR MODELO (BASEADO EM OUTPUTS DO GLMNET) ----

coef(Elastic_modelfuel)

regfuelElastic <- lm(y_fuel_consumption_per_capita_2010 ~ x_pop_2015+x_pop_growth_15_00+x_wghtd_mean_density_rooms_household+
     x_wghtd_mean_density_rooms_household+x_wghtd_mean_density_resident_household+x_prop_dom_urban+
     x_wghtd_mean_household_income_per_capita+x_prop_high_educ+x_prop_motos_pes+x_prop_autos_pes+
     x_prop_work_from_home_res_nucleo+x_prop_work_other_muni_res_not_nucleo+
     x_prop_pop_consolidated_area_2014+x_built_total_total_2014+x_dissimilarity+x_k_avg+
     x_intersection_density_km+x_circuity_avg+x_proportion_largest_patch,data = basenumberlog)

summary(regfuelElastic)

regfuelElastic<- lm(y_fuel_consumption_per_capita_2010 ~ x_pop_growth_15_00 + x_wghtd_mean_density_rooms_household +
                      x_wghtd_mean_density_resident_household+
                       x_prop_dom_urban +x_wghtd_mean_household_income_per_capita+x_prop_high_educ+x_prop_motos_pes +
                       x_prop_autos_pes+x_prop_employed+x_prop_work_from_home_res_nucleo+x_prop_work_other_muni_res_not_nucleo+
                       x_prop_industry+x_prop_pop_consolidated_area_2014+x_dissimilarity+x_k_avg+x_intersection_density_km+
                       x_circuity_avg+x_n_large_patches+x_proportion_largest_patch+x_sd_elevation,data = basenumberlog)


coef(Lasso_modelfuel)

regfuelLasso <- lm(y_fuel_consumption_per_capita_2010 ~ x_prop_dom_urban+x_wghtd_mean_household_income_per_capita+
                     x_prop_motos_pes+x_prop_autos_pes+x_prop_employed+x_prop_work_from_home_res_nucleo+
                     x_prop_pop_consolidated_area_2014+x_dissimilarity+x_intersection_density_km+x_circuity_avg+
                     x_proportion_largest_patch,data = basenumberlog)

summary(regfuelLasso)


setwd("D:/Economia/IPEA/UrbanForm/Outputs")

colunas <- c("Lasso","Elasticnet")

stargazer::stargazer(regfuelLasso,regfuelElastic,column.labels = colunas,
                     type = 'html', out = "Penalityfuelregs")



