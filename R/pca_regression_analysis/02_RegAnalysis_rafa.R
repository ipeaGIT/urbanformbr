#' https://tanthiamhuat.files.wordpress.com/2019/06/penalized-regression-essentials.pdf


library(data.table)
library(stargazer)
library(readr)
library(tidyverse)
library(caret)
library(glmnet)
library(mctest)
library(doParallel)
library(foreach)
library(jtools)
library(interactions)

options(scipen = 999)
`%nin%` <- Negate(`%in%`)




############### 1. prep data --------------------------

############### 1.1 read data
df_raw <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")
head(df_raw)



############### 1.2 convert values to log
#' because of non-positive values, we use
#' inverse hyperbolic sine transformation,
#'  which has the same effect and interpretation

# cols not to log

# id columns
id_cols <- c('i_code_urban_concentration', 'i_name_urban_concentration', 'i_name_uca_case')

# cols no to log because of non-positive values
cols_not_to_log <- c( 'd_large_uca_pop',
                      'd_tma',
                      'x_dissimilarity',
                      'x_pop_growth_15_00',
                      'x_prop_work_from_home_res_not_nucleo',
                      'x_prop_work_other_muni_res_nucleo',
                      'x_prop_work_other_muni_res_not_nucleo',
                      'x_n_large_patches')
cols_to_log <- colnames(df_raw)[ colnames(df_raw) %nin% c(id_cols, cols_not_to_log) ]

df_log <- copy(df_raw)
df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x) } ), .SDcols=cols_to_log]
# df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x + sqrt(x^2 + 1) ) } ), .SDcols=cols_to_log]





############### 1.3 select variables to drop
# dropping built area vars because we measure 'compactness / sprawl' with the x_avg_cell_distance var already
drop1 <- c( # 'x_built_total_total_2014'
            'x_urban_extent_size_2014'
           , 'x_prop_built_consolidated_area_2014'
           )


drop2 <- c('x_sd_elevation', 'x_mean_slope' # we already use x_circuity_avg
           , 'x_n_large_patches'            # we already use x_proportion_largest_patch
           , 'd_large_uca_pop'              # we control for pop continuous
           , 'x_rooms_per_household'        # we control for experienced density
           , 'x_residents_per_household'    # we control for experienced density
           , 'x_prop_black'                 # no strong theoretical justification
           )


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# drop on radius in experimented measures
drop3 <- c('x_density_pop_05km_total_2014',
           'x_density_built_05km_total_2014', 'x_density_built_10km_total_2014',
           'x_dissimilarity_5km', 'x_dissimilarity_10km', 'x_dissimilarity_15km'
           )


# other multicolinear variables
drop4 <- c('x_prop_services',  # colinear with x_prop_industry
           'x_prop_employed',  # colinear with x_wghtd_mean_household_income_per_capita
           'x_prop_high_educ', # colinear with x_wghtd_mean_household_income_per_capita
           'x_prop_formal',    # colinear with x_wghtd_mean_household_income_per_capita
           'x_k_avg'           # colinear with x_wghtd_mean_household_income_per_capita
)

# work from home
drop5 <- c(   'x_prop_work_from_home_res_nucleo'
            , 'x_prop_work_from_home_res_not_nucleo'
            , 'x_prop_work_other_muni_res_nucleo'
            , 'x_prop_work_other_muni_res_not_nucleo')




### drop vars
df_fuel <- dplyr::select(df_log, - c('y_wghtd_mean_commute_time', all_of(c(id_cols, drop1, drop2, drop3, drop4, drop5)) ))
df_time <- dplyr::select(df_log, - c('y_fuel_consumption_per_capita_2010', all_of(c(id_cols, drop1, drop2, drop3, drop4, drop5)) ))
head(df_fuel)
# df_fuel <- dplyr::select(df_log, - all_of(c('y_wghtd_mean_commute_time',id_cols)))





############### Fuel - Train model ---------------------------------------------



### Split the data into training and test set
set.seed(42)

# train with 60% to find alpha and lambda
training.samples <- createDataPartition(df_fuel$y_fuel_consumption_per_capita_2010
                                        , p = 0.70
                                        , list = FALSE)

# split
train_data <- df_fuel[training.samples, ]
test_data <- df_fuel[-training.samples, ]

# Outcome and Predictor variables
x_train <- model.matrix(y_fuel_consumption_per_capita_2010~., train_data)[,-1] # without intercept
x_test <- model.matrix(y_fuel_consumption_per_capita_2010~., test_data)[,-1] # without intercept
x_full <- model.matrix(y_fuel_consumption_per_capita_2010~., df_fuel)[,-1] # without intercept

y_test <- test_data$y_fuel_consumption_per_capita_2010
y_train <- train_data$y_fuel_consumption_per_capita_2010
y_full <- df_fuel$y_fuel_consumption_per_capita_2010




# trainning in parallel
library(doParallel)
library(foreach )
cl <- parallel::makePSOCKcluster(20)
doParallel::registerDoParallel(cl)


###### ridge regression (alpha = 0) ----------------

  # Find the best lambda using cross-validation (the one that minimizes the cross-validation prediction error rate)
  set.seed(42)

  cv <- cv.glmnet(x = x_train, y = y_train, alpha = 0, nfolds = 30, parallel = T)

  # Display the best lambda value to adjust the intensity of coefficient shrinkage
  cv$lambda.min

  # Fit the final model on the training data
  model_ridge <- glmnet(x_train, y_train, alpha = 0, lambda = cv$lambda.min)

  # Display regression coefficients
  coef(model_ridge)

  # Make predictions on the test data
  predictions_ridge <- model_ridge %>% predict(x_test) %>% as.vector()

  # Model performance metrics
  data.frame(
    RMSE = RMSE(predictions_ridge, test_data$y_fuel_consumption_per_capita_2010),  #  prediction error, RMSE
    Rsquare = R2(predictions_ridge, test_data$y_fuel_consumption_per_capita_2010)
  )
  ## RMSE Rsquare
  ## 1 0.1810904 0.8641129




###### lasso regression (alpha = 1) ----------------

  # Find the best lambda using cross-validation
  set.seed(42)
  cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 30, parallel = T)

  # Display the best lambda value
  cv$lambda.min

  # Fit the final model on the training data
  model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = cv$lambda.min)

  # Dsiplay regression coefficients
  coef(model_lasso)

  # Make predictions on the test data
  predictions_lasso <- model_lasso %>% predict(x_test) %>% as.vector()

  # Model performance metrics
  data.frame(
    RMSE = RMSE(predictions_lasso, test_data$y_fuel_consumption_per_capita_2010),
    Rsquare = R2(predictions_lasso, test_data$y_fuel_consumption_per_capita_2010)
  )




###### elastic net regression (alpha between 0 and 1) ----------------

  # The best alpha and lambda values are those values that minimize the cross-validation error

  # Build the model using the training set
  set.seed(42)

  model_elastic <- caret::train(
    y_fuel_consumption_per_capita_2010 ~., data = test_data, method = "glmnet",
    trControl = trainControl("cv", number = 20),
    tuneLength = 20 #  test the combination of 10 different values for alpha and lambda.
  )

  # Best tuning parameter
  model_elastic$bestTune
  ## alpha lambda
  ##  1 0.01406487


  # Coefficient of the final model. You need
  # to specify the best lambda
  my_coefs <- coef(model_elastic$finalModel, alpha=model_elastic$bestTune$alpha, lambda=model_elastic$bestTune$lambda)
  my_coefs

  # Make predictions on the test data
  predictions_elastic <- model_elastic %>% predict(x_test)

  # Model performance metrics
  data.frame(
    RMSE = RMSE(predictions_elastic, test_data$y_fuel_consumption_per_capita_2010),
    Rsquare = R2(predictions_elastic, test_data$y_fuel_consumption_per_capita_2010)
  )


#### simple OLS ----------------
  # regression
  model_linear <- lm(y_fuel_consumption_per_capita_2010~., test_data)

  # Make predictions on the test data
  predictions_linear <- model_linear %>% predict(as.data.frame(x_test))

  # Model performance metrics
  data.frame(
    RMSE = RMSE(predictions_linear, test_data$y_fuel_consumption_per_capita_2010),
    Rsquare = R2(predictions_linear, test_data$y_fuel_consumption_per_capita_2010)
  )



###### let caret decide ----------------
# the best model is the one that minimizes the prediction error RMSE (lowest median RMSE - Root Mean Square Error)


# # Setup a grid range of lambda values:
# lambda <- 10^seq(-3, 3, length = 100)
# alpha <- seq(0.1, 0.9, length = 100)

# ridge model
set.seed(42)
ridge <- caret::train(
                y_fuel_consumption_per_capita_2010 ~., data = train_data, method = "glmnet",
                trControl = trainControl("cv", number = 100),
                tuneGrid = expand.grid(alpha = 0, lambda = lambda)
                )

# lasso model
set.seed(42)
lasso <- caret::train(
                y_fuel_consumption_per_capita_2010 ~., data = train_data, method = "glmnet",
                trControl = trainControl("cv", number = 100),
                tuneGrid = expand.grid(alpha = 1, lambda = lambda)
                )

# elastic model
set.seed(42)
elastic <- caret::train(
                        y_fuel_consumption_per_capita_2010 ~., data = train_data, method = "glmnet",
                        trControl = trainControl("cv", number = 100),
                        tuneLength = 100 #  test the combination of 10 different values for alpha and lambda.
                      )


# linear model
  linear <- lm(y_fuel_consumption_per_capita_2010 ~., data = train_data)

  # Make predictions on the test data
  predictions_linear <- model_linear %>% predict(as.data.frame(x_test))

  # Model performance metrics
  data.frame(
    RMSE = RMSE(predictions_linear, test_data$y_fuel_consumption_per_capita_2010),
    Rsquare = R2(predictions_linear, test_data$y_fuel_consumption_per_capita_2010)
  )



### Comparing models performance:
# use the one with the lowest (min and median) RMSE
models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")





### check  multicollinearity-------------

library(mctest)

# regression model
model_raw <- lm(y_fuel_consumption_per_capita_2010~., df_fuel)
summary(model_raw)

# identify which variable present multicollinearity (VIF values above 10 are a bad sign)
mctest::imcdiag(model_raw, method ='VIF')

# check partial correlations
library(ppcor)
pcor(df_fuel, method = "pearson")


library(qgraph)
qgraph::qgraph( cor( dplyr::select(df_fuel, - 'y_fuel_consumption_per_capita_2010') ) , theme='gray', layout='spring')



# update regression model and check multicollinearity (VIF values above 10 are a bad sign)
  model_raw2 <- lm(y_fuel_consumption_per_capita_2010 ~., df_fuel)
  mctest::imcdiag(model_raw2, method ='VIF')

  ### remaining cases of multicolinearity

  # industry Vs service workers
  cor(df_fuel$x_prop_services, df_fuel$x_prop_industry)

  # income Vs education or fleet size
  cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_prop_high_educ)
  cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_prop_autos_pes)

  cor(df_fuel$x_prop_high_educ, df_fuel$x_prop_autos_pes)





############### Elastic for good ---------------------------------------------

### train the model using the training set
set.seed(42)

library(doParallel)
library(foreach)
# library(mboost)
# library(randomForest)

cl <- parallel::makePSOCKcluster(20)
doParallel::registerDoParallel(cl)

system.time(
 elastic_model_train <- caret::train(
            y_fuel_consumption_per_capita_2010 ~., data = train_data
            , method = "glmnet"
            #, method = "glmboost"
            #, method = "rf"
            , trControl = trainControl("cv", number = 10)
            ,  tuneLength = 10  #  test the combination of 20 different values for alpha and lambda.
            )
          )

# stop parallel
stopCluster(cl)
env <- foreach:::.foreachGlobals
rm(list=ls(name=env), pos=env)

# Best tuning parameter
# The best alpha and lambda values are those values that minimize the cross-validation error
elastic_model_train$bestTune

# Coefficient of the final model using the best alpha and lambda values
elastic_coefs_train <- coef(object = elastic_model_train$finalModel,
                            alpha  = elastic_model_train$bestTune$alpha,
                            lambda = elastic_model_train$bestTune$lambda)

# Make predictions on the test data
predictions <- elastic_model_train %>% predict(x_test)

  # plot
  plot(predictions, y_test)
  abline(lm(y_test ~ predictions))

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test_data$y_fuel_consumption_per_capita_2010),
  Rsquare = R2(predictions, test_data$y_fuel_consumption_per_capita_2010)
)




### apply model
elastic_model_full <- glmnet(x=x_full,
                             y=y_full,
                             alpha = elastic_model_train$bestTune$alpha,
                             lambda = elastic_model_train$bestTune$lambda)








#### OLS dropping weak variables

# identify vars to drop, according to elastic net results
elastic_coefs_full <- coef(elastic_model_full)
vars_to_drop <- elastic_coefs_full[,1] %>% abs() == 0
elastic_coefs_full[vars_to_drop,]
cols_to_remove <- rownames(elastic_coefs_full)[vars_to_drop ]

# rn OLS with vars suggested by Elastic net
temp_df <- dplyr::select(df_fuel, - all_of(cols_to_remove))
model_raw <- lm(y_fuel_consumption_per_capita_2010~.,


                temp_df)


temp_df <- dplyr::select(temp_df, - c('x_prop_autos_pes','x_prop_motos_pes'))

summary(model_raw)
# RMSE
sqrt(mean(model_raw$residuals^2))
0.1677349
0.1871099

AIC(model_raw)
-98.84701
-62.62047

R-squared:  0.8739
R-squared:  0.845



# identify which variable present multicollinearity (VIF values above 10 are a bad sign)
mctest::imcdiag(model_raw, method ='VIF')

summary(model_raw)




### check interactions ----------------------------------------------------------

fiti <- lm(y_fuel_consumption_per_capita_2010~ x_rooms_per_household + x_prop_work_from_home_res_nucleo+ x_prop_work_from_home_res_nucleo*d_large_uca_pop + x_avg_cell_distance + x_avg_cell_distance*x_pop_2015, df_raw)# %>% summary()

library(jtools)
library(interactions)

interact_plot(fiti, pred = x_prop_work_from_home_res_nucleo   , modx = d_large_uca_pop, plot.points = TRUE)

interact_plot(fiti, pred = x_avg_cell_distance   , modx = x_pop_2015, plot.points = TRUE)
######################################################################################################




model_lm <- lm(y_fuel_consumption_per_capita_2010~., df_fuel)
summary(model_lm)

# identify which variable present multicollinearity (VIF values above 10 are a bad sign)
mctest::imcdiag(model_lm, method ='VIF')

# check partial correlations
library(ppcor)
pcor(Boston, method = "pearson")





######## duvidas ---------------------------------------------------------



sensibilidade para testar raios
sensibilidade para testar interacoes
sensibilidade para testar quadratico


