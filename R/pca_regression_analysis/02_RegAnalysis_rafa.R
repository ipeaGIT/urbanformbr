#' https://tanthiamhuat.files.wordpress.com/2019/06/penalized-regression-essentials.pdf
#'
#'


library(data.table)
library(stargazer)
library(readr)
library(tidyverse)
library(caret)
library(glmnet)
library(mctest)
library(doParallel)
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
                      'x_pop_growth_15_00',
                      'x_prop_work_from_home_res_not_nucleo',
                      'x_prop_work_other_muni_res_nucleo',
                      'x_prop_work_other_muni_res_not_nucleo',
                      'x_n_large_patches')
cols_to_log <- colnames(df_raw)[ colnames(df_raw) %nin% c(id_cols, cols_not_to_log) ]

df_log <- copy(df_raw)
df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x) } ), .SDcols=cols_to_log]
# df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x + sqrt(x^2 + 1) ) } ), .SDcols=cols_to_log]




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cor(df_raw$x_built_total_total_2014, df_raw$x_avg_cell_distance)

############### 1.3 select variables to drop
# dropping built area vars because we measure 'compactness / sprawl' with the x_avg_cell_distance var already
drop1 <- c('x_built_total_total_2014', 'x_urban_extent_size_2014', 'x_prop_built_consolidated_area_2014')

# we already use x_circuity_avg
drop2 <- c('x_sd_elevation', 'x_mean_slope')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# drop on radius in experimented measures
drop3 <- c('x_density_pop_05km_total_2014',
           'x_density_built_05km_total_2014', 'x_density_built_10km_total_2014',
           'x_dissimilarity_5km', 'x_dissimilarity_15km'
           )


# other multicolinear variables
drop4 <- c('x_prop_services', # colinear with x_prop_industry
           'x_prop_employed' # colinear with x_wghtd_mean_household_income_per_capita
           )

cor(df_raw$x_prop_autos_pes, df_raw$x_prop_employed)

# !!!!!!!!!!!!!!
x_prop_autos_pes
x_prop_high_educ
x_wghtd_mean_household_income_per_capita

cor(df_raw$x_prop_autos_pes, df_raw$x_wghtd_mean_household_income_per_capita)
cor(df_raw$x_prop_autos_pes, df_raw$x_prop_high_educ)


cor(df_raw$x_built_total_total_2014, df_raw$x_avg_cell_distance)


sprawl >> car dependence >> fleet >> longer trips >> fuel

escolaridade << >> income >> fleet


fuel << >> income



### drop vars
df_fuel <- dplyr::select(df_log, - c('y_wghtd_mean_commute_time', all_of(c(id_cols, drop1, drop2, drop3, drop4)) ))
# df_fuel <- copy(df_log)





############### Fuel - Train model --------------------------


### Split the data into training and test set
set.seed(42)

# train with 60% to find alpha and lambda
training.samples <- createDataPartition(df_fuel$y_fuel_consumption_per_capita_2010
                                        , p = 0.6
                                        , list = FALSE)

# split
train_data <- df_fuel[training.samples, ]
test_data <- df_fuel[-training.samples, ]

# Outcome and Predictor variables
x_train <- model.matrix(y_fuel_consumption_per_capita_2010~., train_data)[,-1] # without intercept
x_test <- model.matrix(y_fuel_consumption_per_capita_2010~., test_data)[,-1] # without intercept
x <- model.matrix(y_fuel_consumption_per_capita_2010~., df_fuel)[,-1] # without intercept

y_test <- test_data$y_fuel_consumption_per_capita_2010
y_train <- train_data$y_fuel_consumption_per_capita_2010
y <- df_fuel$y_fuel_consumption_per_capita_2010






###### ridge regression (alpha = 0) ----------------

  # Find the best lambda using cross-validation (the one that minimizes the cross-validation prediction error rate)
  set.seed(42)

  cv <- cv.glmnet(x_train, y_train, alpha = 0)

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
  ## 1 0.2181782 0.8218827





###### lasso regression (alpha = 1) ----------------

  # Find the best lambda using cross-validation
  set.seed(42)
  cv <- cv.glmnet(x_train, y_train, alpha = 1)

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


# Setup a grid range of lambda values:
lambda <- 10^seq(-3, 3, length = 100)
alpha <- seq(0.1, 0.9, length = 100)

# ridge model
set.seed(42)
ridge <- train(
                y_fuel_consumption_per_capita_2010 ~., data = train_data, method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0, lambda = lambda)
                )

# lasso model
set.seed(42)
lasso <- train(
                y_fuel_consumption_per_capita_2010 ~., data = train_data, method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1, lambda = lambda)
                )

# elastic model
set.seed(42)
elastic <- train(
                y_fuel_consumption_per_capita_2010 ~., data = train_data, method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneLength = 10
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
# use the one with the lowest median RMSE
models <- list(ridge = ridge, lasso = lasso, elastic = elastic, elastic2=elastic2)
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





### elastic for good -------------

### train the model using the training set
set.seed(42)

library(doParallel)
# library(mboost)
# library(randomForest)

cl <- parallel::makePSOCKcluster(20)
doParallel::registerDoParallel(cl)

system.time(
            model <- caret::train(
            y_fuel_consumption_per_capita_2010 ~., data = train_data
            , method = "glmnet"
            #, method = "glmboost"
            #, method = "rf"
            , trControl = trainControl("cv", number = 20)
            ,  tuneLength = 20  #  test the combination of 20 different values for alpha and lambda.
            )
          )

# stop parallel
stopCluster(cl)
env <- foreach:::.foreachGlobals
rm(list=ls(name=env), pos=env)

# Best tuning parameter
# The best alpha and lambda values are those values that minimize the cross-validation error
model$bestTune
#         alpha     lambda
# 252 0.6684211 0.01917282

# Coefficient of the final model using the best alpha and lambda values
my_coefs <- coef(model$finalModel, alpha=model$bestTune$alpha, lambda=model$bestTune$lambda)
my_coefs



# Make predictions on the test data
predictions <- model %>% predict(x_test)

  # plot
  plot(predictions, y_test)
  abline(lm(y_test ~ predictions))

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test_data$y_fuel_consumption_per_capita_2010),
  Rsquare = R2(predictions, test_data$y_fuel_consumption_per_capita_2010)
)


# apply model
model_elastic <- glmnet(x=x,
                        y=y,
                        alpha = model$bestTune$alpha,
                        lambda = model$bestTune$lambda)
coef(model_elastic)




############### Fuel - Apply model --------------------------

# Outcome and Predictor variables
xvector <- model.matrix(y_fuel_consumption_per_capita_2010~., df_fuel)[,-1] # without intercept
yvector <- df_fuel$y_fuel_consumption_per_capita_2010


model_elastic <- glmnet(x=x_test, y=y_test, alpha = model$bestTune$alpha, lambda = model$bestTune$lambda)
coef(model_elastic)


# Make predictions on the test data
predictions <- model_elastic %>% predict(x_test)

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, df_fuel$y_fuel_consumption_per_capita_2010),
  Rsquare = R2(predictions, df_fuel$y_fuel_consumption_per_capita_2010)
)



#### OLS dropping weak variables

# identify vars to drop, according to elastic net results
my_coefs <- coef(model_elastic)
vars_to_drop <- my_coefs[,1] %>% abs() == 0
my_coefs[vars_to_drop,]
cols_to_remove <- rownames(my_coefs)[vars_to_drop ]
temp_df <- dplyr::select(df_fuel, - all_of(cols_to_remove))
model_raw <- lm(y_fuel_consumption_per_capita_2010~., temp_df)








### check interactions ----------------------------------------------------------

fiti <- lm(y_fuel_consumption_per_capita_2010~ x_rooms_per_household + x_prop_work_from_home_res_nucleo+ x_prop_work_from_home_res_nucleo*d_large_uca_pop + x_avg_cell_distance + x_avg_cell_distance*x_pop_2015, df_raw)# %>% summary()

library(jtools)
library(interactions)

interact_plot(fiti, pred = x_prop_work_from_home_res_nucleo   , modx = d_large_uca_pop, plot.points = TRUE)

interact_plot(fiti, pred = x_avg_cell_distance   , modx = x_pop_2015, plot.points = TRUE)
######################################################################################################


# Outcome and Predictor variables
xvector <- model.matrix(y_fuel_consumption_per_capita_2010~., df_fuel)[,-1] # without intercept
yvector <- df_fuel$y_fuel_consumption_per_capita_2010


model_elastic <- glmnet(xvector, yvector, alpha = model$bestTune$alpha, lambda = model$bestTune$lambda)
coef(model_elastic)


tmp_coeffs <- coef(model_elastic, s = "lambda.min")
data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)


model_lm <- lm(y_fuel_consumption_per_capita_2010~., df_fuel)
summary(model_lm)

# identify which variable present multicollinearity (VIF values above 10 are a bad sign)
mctest::imcdiag(model_lm, method ='VIF')

# check partial correlations
library(ppcor)
pcor(Boston, method = "pearson")





######## duvidas ---------------------------------------------------------
1. elastic net não dá significancia estatistica? p-value ou t-statistic?


sensibilidade para testar raios
sensibilidade para testar interacoes
sensibilidade para testar quadratico


