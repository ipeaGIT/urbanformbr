#' https://tanthiamhuat.files.wordpress.com/2019/06/penalized-regression-essentials.pdf
#'
# 0. load libraries-----------------

libraries_to_load <- c("data.table",
                       "stargazer",
                       "readr",
                       "tidyverse",
                       "caret",
                       "glmnet",
                       "mctest",
                       "doParallel",
                       "mctest",
                       "qgraph",
                       "jtools",
                       "interactions",
                       "mctest")
new_packages <- libraries_to_load[!(libraries_to_load %in%
                                      installed.packages()[,"Package"])]
if(length(new_packages)>1){install.packages(new_packages) }
# load packages
lapply(libraries_to_load, require, character.only = TRUE)

rm(list=ls())
options(scipen = 999)
`%nin%` <- Negate(`%in%`)




############### 1. prep data --------------------------

## 1.1 read data----------
df_raw <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")
head(df_raw)




## 1.2 convert values to log----------
#' because of non-positive values, we use
#' inverse hyperbolic sine transformation,
#'  which has the same effect and interpretation

# cols not to log
# id columns
id_cols <- c('i_code_urban_concentration',
             'i_name_urban_concentration',
             'i_name_uca_case')

# cols no to log because of non-positive values
cols_not_to_log <- c( 'd_large_uca_pop',
                      'd_tma',
                      'x_pop_growth_15_00',
                      'x_prop_work_from_home_res_not_nucleo',
                      'x_prop_work_other_muni_res_nucleo',
                      'x_prop_work_other_muni_res_not_nucleo',
                      'x_n_large_patches')
cols_to_log <- colnames(df_raw)[ colnames(df_raw) %nin% c(id_cols,
                                                          cols_not_to_log) ]

df_prep <- copy(df_raw)
#df_prep[, (cols_to_log) := lapply(.SD, log), .SDcols= cols_to_log]
my_f <- function(i){log10(i + sqrt(i^2 + 1) )}
df_prep[, (cols_to_log) := lapply(.SD, as.numeric), .SDcols=cols_to_log]
df_prep[, (cols_to_log) := lapply(.SD, my_f), .SDcols=cols_to_log]

## 1.3 select variables to drop----------


## 1.3.1 plot cor-----
png(height = 30
    , width = 30
    , units = "cm"
    , res = 300
    , file = "corrplot.png"
    , type = "cairo")

df_raw %>%
  .[,.SD,.SDcols = !c("i_code_urban_concentration",
                      "i_name_urban_concentration",
                      "i_name_uca_case",
                      "d_large_uca_pop",
                      "d_tma",
                      "y_fuel_consumption_per_capita_2010",
                      "y_wghtd_mean_commute_time")] %>%
  stats::cor() %>%
  corrplot::corrplot(order = 'hclust'
                     , sig.level = .10
                     , addrect = 8
                     , diag = FALSE
                     #, addCoef.col = 'black'
  )

dev.off()

#' dropping built area vars because we measure
#' 'compactness / sprawl' with the x_avg_cell_distance
#' var already

drop1 <- c('x_built_total_total_2014',
           'x_urban_extent_size_2014',
           'x_prop_built_consolidated_area_2014')

# we already use x_circuity_avg
drop2 <- c('x_sd_elevation',
           'x_circuity_avg',
           'x_mean_slope')

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# drop on radius in experimented measures
drop3 <- c('x_density_pop_05km_total_2014',
           'x_density_pop_10km_total_2014',
           'x_density_built_05km_total_2014',
           'x_density_built_10km_total_2014')

# other multicolinear variables
drop4 <- c('x_prop_services', # colinear with x_prop_industry
           'x_prop_employed' # colinear with x_wghtd_mean_household_income_per_capita
)

drop5 <- c('x_dissimilarity_5km',
           'x_dissimilarity_10km',
           'x_dissimilarity_15km')

drop6 <- c('x_rooms_per_household',
           'x_prop_dom_urban',
           'x_wghtd_mean_household_income_per_capita',
           'x_prop_high_educ',
           'x_prop_autos_pes',
           'x_prop_formal',
           'x_prop_employed'
)

drop7 <- c("x_prop_work_from_home_res_nucleo",
           "x_prop_work_from_home_res_not_nucleo",
           "x_prop_work_other_muni_res_nucleo",
           "x_prop_work_other_muni_res_not_nucleo")

drops_comb <- list(drop1,drop2,drop3,drop4,drop5,drop6,drop7) %>%
  cross() %>%
  map(paste)

drops_all <- drops_comb %>% unlist() %>% unique()

# intro-------
my_y = "y_fuel_consumption_per_capita_2010"


tmp_output <- lapply(1:5,function(i){ # i = 1
  # drop variables--------

  message(sprintf("working with interaction # %s/%s"
                  ,i,length(drops_comb)))

  x_to_exclude <- drops_all[!(drops_all %in% drops_comb[[i]])]

  y_to_exclude <-  c("y_fuel_consumption_per_capita_2010",
                     "y_wghtd_mean_commute_time")
  y_to_exclude <- y_to_exclude[y_to_exclude != my_y]

  df <-  data.table::copy(df_prep) %>%
    .[,.SD,.SDcols = !c(x_to_exclude,
                        y_to_exclude,
                        "i_code_urban_concentration",
                        "i_name_urban_concentration",
                        "i_name_uca_case",
                        "x_pop_2015",
                        "x_prop_black",
                        "x_prop_razao_dep")]


  # split variables-----
  set.seed(42)

  # train with 60% to find alpha and lambda
  training.samples <- caret::createDataPartition(
    y = df[,get(my_y)]
    , p = 0.7
    , list = FALSE)

  train_data <- df[training.samples, ]
  test_data <- df[-training.samples, ]

  # 2.2 Outcome and Predictor variables
  x_train <- model.matrix(get(my_y)~.
                          , train_data)[,-1] # without intercept
  x_test <- model.matrix(get(my_y)~.
                         , test_data)[,-1]   # without intercept

  y_test <- test_data[,get(my_y)]
  y_train <- train_data[,get(my_y)]


  ## 2.3  elastic net regression  ----------------
  #' (alpha between 0 and 1)
  #' The best alpha and lambda values are those values that minimize the
  #' cross-validation error

  ### 2.3.1 Build the model using the training set ----------------
  set.seed(42)

  model_elastic <- caret::train(
    y_fuel_consumption_per_capita_2010  ~.
    , data = train_data
    , method = "glmnet"
    , trControl = trainControl("cv", number = 5)
    , tuneLength = 20 #  test the combination of 10 different values for alpha and lambda.
  )

  # Best tuning parameter
  model_elastic$bestTune
  ## alpha lambda
  ##  1 0.01406487

  # finally the model -----
  # Outcome and Predictor variables


  train_model_elastic <- glmnet::glmnet(x = x_train
                                       , y = y_train
                                       , alpha = model_elastic$bestTune$alpha
                                       , lambda = model_elastic$bestTune$lambda)
  #coef(test_model_elastic)


  # Make predictions on the test data
  test_predictions <- train_model_elastic %>% predict(x_test)

  # Model performance metrics
  mod_perf <- data.table::data.table(
    "RMSE" = RMSE(test_predictions,  test_data[,get(my_y)]),
    "Rsquare" = R2(test_predictions,  test_data[,get(my_y)]) %>% as.vector(),
    "n" = i
  )

  return(mod_perf)
}) %>% data.table::rbindlist()
tmp_output

my_reg_output <- lapply(head(drop_comb),my_reg,df)
## 1.4 drop vars ----------
df_fuel <- dplyr::select(df_prep, - c('y_wghtd_mean_commute_time'
                                      , all_of(c(id_cols, drop1,
                                                 drop2, drop3, drop4))))





# 2. Train model --------------------------


## 2.1 Split the data into training and test set----------------------
set.seed(42)

# train with 60% to find alpha and lambda
training.samples <- createDataPartition(df_fuel$y_fuel_consumption_per_capita_2010
                                        , p = 0.6
                                        , list = FALSE)

# split
train_data <- df_fuel[training.samples, ]
test_data <- df_fuel[-training.samples, ]

## 2.2 Outcome and Predictor variables ----------------------
x_train <- model.matrix(y_fuel_consumption_per_capita_2010~.
                        , train_data)[,-1] # without intercept
x_test <- model.matrix(y_fuel_consumption_per_capita_2010~.
                       , test_data)[,-1]   # without intercept
x <- model.matrix(y_fuel_consumption_per_capita_2010~.
                  , df_fuel)[,-1] # without intercept

y_test <- test_data$y_fuel_consumption_per_capita_2010
y_train <- train_data$y_fuel_consumption_per_capita_2010
y <- df_fuel$y_fuel_consumption_per_capita_2010


## 2.3  elastic net regression  ----------------
#' (alpha between 0 and 1)
#' The best alpha and lambda values are those values that minimize the
#' cross-validation error

### 2.3.1 Build the model using the training set ----------------
set.seed(42)

model_elastic <- caret::train(
  y_fuel_consumption_per_capita_2010 ~.
  , data = test_data
  , method = "glmnet"
  , trControl = trainControl("cv", number = 20)
  , tuneLength = 20 #  test the combination of 10 different values for alpha and lambda.
)

# Best tuning parameter
model_elastic$bestTune
## alpha lambda
##  1 0.01406487


# Coefficient of the final model. You need
# to specify the best lambda
my_coefs <- coef(model_elastic$finalModel,
                 alpha = model_elastic$bestTune$alpha,
                 lambda = model_elastic$bestTune$lambda)
my_coefs

### 2.3.2 Predict on test----------------
predictions_elastic <- model_elastic %>% predict(x_test)

### 2.3.3 Model performance metrics ----------------
data.frame(
  RMSE = RMSE(predictions_elastic, test_data$y_fuel_consumption_per_capita_2010),
  Rsquare = R2(predictions_elastic, test_data$y_fuel_consumption_per_capita_2010)
)

#
# #### simple OLS
# # regression
# model_linear <- lm(y_fuel_consumption_per_capita_2010~., test_data)
#
# # Make predictions on the test data
# predictions_linear <- model_linear %>% predict(as.data.frame(x_test))
#
# # Model performance metrics
# data.frame(
#   RMSE = RMSE(predictions_linear, test_data$y_fuel_consumption_per_capita_2010),
#   Rsquare = R2(predictions_linear, test_data$y_fuel_consumption_per_capita_2010)
# )



## 2.4 Let caret decide ----------------
#' the best model is the one that minimizes the prediction error RMSE
#' (lowest median RMSE - Root Mean Square Error)


# Setup a grid range of lambda values:
lambda <- 10^seq(-3, 3, length = 100)
alpha <- seq(0.1, 0.9, length = 100)

# elastic model
set.seed(42)
elastic <- caret::train(
  y_fuel_consumption_per_capita_2010 ~.
  , data = train_data
  , method = "glmnet"
  , trControl = trainControl("cv", number = 10)
  , tuneLength = 10
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
models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")



break()

### check  multicollinearity-------------


# regression model
model_raw <- lm(y_fuel_consumption_per_capita_2010~., df_fuel)
summary(model_raw)

# identify which variable present multicollinearity (VIF values above 10 are a bad sign)
mctest::imcdiag(model_raw, method ='VIF')

# check partial correlations
library(ppcor)
pcor(df_fuel, method = "pearson")


library(qgraph)
qgraph::qgraph( cor( dplyr::select(df_raw,
                                   - 'y_fuel_consumption_per_capita_2010') ) , theme='gray', layout='spring')



#' update regression model and check multicollinearity
#' (VIF values above 10 are a bad sign)
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




cor(df_fuel$x_prop_employed       , df_fuel$x_wghtd_mean_household_income_per_capita                    )



#### OLS dropping weak variables

# identify vars to drop, according to elastic net results
vars_to_drop <- my_coefs[,1] %>% abs() == 0
my_coefs[vars_to_drop,]
cols_to_remove <- rownames(my_coefs)[vars_to_drop ]
temp_df <- dplyr::select(df_fuel, - cols_to_remove)
lm(y_fuel_consumption_per_capita_2010~., temp_df) %>% summary()


sum(df_fuel$d_large_uca_pop)


### check interactions ----------------------------------------------------------

fiti <- lm(y_fuel_consumption_per_capita_2010 ~
             x_rooms_per_household +
             x_prop_work_from_home_res_nucleo +
             x_prop_work_from_home_res_nucleo*d_large_uca_pop +
             x_avg_cell_distance + x_avg_cell_distance*x_pop_2015
           , df_raw)# %>% summary()



interact_plot(fiti
              , pred = x_prop_work_from_home_res_nucleo
              , modx = d_large_uca_pop, plot.points = TRUE)

interact_plot(fiti
              , pred = x_avg_cell_distance
              , modx = x_pop_2015
              , plot.points = TRUE)
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


