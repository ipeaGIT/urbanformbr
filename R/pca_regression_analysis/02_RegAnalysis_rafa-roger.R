set.seed(42)

library(doParallel)
library(foreach)
# library(mboost)
# library(randomForest)

cl <- parallel::makePSOCKcluster(30)
doParallel::registerDoParallel(cl)


# Set training control
# K fold cross validation
train_control <- trainControl(method = "repeatedcv",
                              number = 3,
                              repeats = 100,
                              search = "random",
                              verboseIter = TRUE)

formula_elasticNet <- y_fuel_consumption_per_capita_2010 ~. +
  # x_pop_2015 * x_prop_pop_consolidated_area_2014 +
  x_pop_2015 * x_density_pop_10km_total_2014 +
  x_pop_2015 * x_dissimilarity +
  # x_pop_2015 * x_intersection_density_km +
  # x_pop_2015 * x_circuity_avg +
  # x_pop_2015 * x_proportion_largest_patch +
  x_pop_2015 * x_avg_cell_distance





elastic_net_model <- train(formula_elasticNet,
                           data       = df_fuel,
                           method     = "glmnet",
                           preProcess = c("center", "scale"),
                           tuneLength = 100,
                           trControl  = train_control
                           )

elastic_net_model$bestTune

# Coeficients of the best model
params <- coef(elastic_net_model$finalModel,
               s = elastic_net_model$bestTune$lambda
               )


predictions <- elastic_net_model %>% predict(x_full)

data.frame(
  RMSE = RMSE(predictions, df_fuel$y_fuel_consumption_per_capita_2010),
  Rsquare = R2(predictions, df_fuel$y_fuel_consumption_per_capita_2010)

)




#### OLS dropping weak variables

# identify vars to keep, according to elastic net results
elastic_coefs_full <- params
vars_to_keep <- elastic_coefs_full[,1] %>% abs() != 0
elastic_coefs_full[vars_to_keep,]
vars_to_keep <- rownames(elastic_coefs_full)[vars_to_keep ]
vars_to_keep <- vars_to_keep[!vars_to_keep %like% 'Intercept'] # ignore intercept
vars_to_keep <- stringr::str_replace(vars_to_keep, ':', '*') # fix interaction terms

# write model specification
specification = paste0('y_fuel_consumption_per_capita_2010 ~ ',  # y
                paste(vars_to_keep, collapse  = ' + ')) # x

# run OLS with vars suggested by Elastic net
model_raw <- lm(specification, df_fuel)


model_raw <- lm(y_fuel_consumption_per_capita_2010 ~
                  # d_tma +
                  x_pop_growth_15_00 +
                  x_prop_dom_urban +
                  # x_prop_motos_dom +
                  # x_prop_autos_dom +
                  x_wghtd_mean_household_income_per_capita +
                  # x_prop_industry +
                  x_dissimilarity +
                  x_intersection_density_km +
                  x_circuity_avg +
                  x_proportion_largest_patch +
                  x_prop_pop_consolidated_area_2014 +
                  x_pop_2015*x_dissimilarity +
                  x_pop_2015*x_density_pop_10km_total_2014 +
                  x_pop_2015*x_proportion_largest_patch
                , df_fuel)


interact_plot(model_raw, pred = x_dissimilarity, modx = x_pop_2015, plot.points = TRUE)
interact_plot(model_raw, pred = x_prop_pop_consolidated_area_2014, modx = x_pop_2015, plot.points = TRUE)


cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$y_fuel_consumption_per_capita_2010)
mctest::imcdiag(model_raw, method ='VIF')

cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_prop_dom_urban)



summary(model_raw)
# RMSE
sqrt(mean(model_raw$residuals^2))
0.1677349
0.1871099

AIC(model_raw)
-98.84701
-62.62047
