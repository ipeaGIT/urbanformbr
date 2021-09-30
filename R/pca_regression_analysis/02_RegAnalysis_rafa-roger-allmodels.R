

# 11.Models

#########  organize model components ---------------------------------------
vars_dep <- 'y_fuel_energy_per_capita'
vars_forma <- '~ x_avg_cell_distance + x_density_pop_10km_total_2014 + x_land_use_mix + x_proportion_largest_patch + x_intersection_density_km + x_circuity_avg' #  + x_prop_pop_consolidated_area_2014
vars_controls <-'+ x_prop_dom_urban + x_wghtd_mean_household_income_per_capita + x_prop_razao_dep + x_prop_industry' # x_prop_autos_dom + x_prop_motos_dom
vars_interactions <- '+ I(x_density_pop_10km_total_2014^2)'


######### create all model specifications ---------------------------------------
model_all <- paste(vars_dep, '~.')
model_cntrl <- paste(vars_dep, vars_forma, vars_controls)
model_cntrl_inter <- paste(vars_dep, vars_forma, vars_controls, vars_interactions)

all_models <- list(model_all, model_cntrl, model_cntrl_inter)
names(all_models) <- list( quote(model_all), quote(model_cntrl), quote(model_cntrl_inter))



######### create function to run ElasticNet model ------------------------------------
elastic_model <- function(model){ # model <- all_models[[3]]

  # cl <- parallel::makePSOCKcluster(30)
  # doParallel::registerDoParallel(cl)

  # Set training control
  # K fold cross validation
  train_control <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 100,
                                search = "random",
                                verboseIter = TRUE)

  elastic_net_model <- train(as.formula(model),
                             data       = df_fuel,
                             method     = "glmnet",
                             # preProcess = c("center", "scale"), # rogerio. Ja esta em log, precisa disso? isso muda o tunning
                             tuneLength = 100,
                             trControl  = train_control)

  # # stop parallel
  # stopCluster(cl)
  # env <- foreach:::.foreachGlobals
  # rm(list=ls(name=env), pos=env)

  # Coeficients of the best model
  params <- coef(elastic_net_model$finalModel,
                 s = elastic_net_model$bestTune$lambda)


  #### OLS dropping weak variables

  # identify vars to keep, according to elastic net results
  elastic_coefs_full <- params
  vars_to_keep <- elastic_coefs_full[,1] %>% abs() != 0
  elastic_coefs_full[vars_to_keep,]
  vars_to_keep <- rownames(elastic_coefs_full)[vars_to_keep ]
  vars_to_keep <- vars_to_keep[!vars_to_keep %like% 'Intercept'] # ignore intercept
  vars_to_keep <- stringr::str_replace(vars_to_keep, ':', '*') # fix interaction terms

  # write model specification
  specification = paste0('y_fuel_energy_per_capita ~ ',  # y
                         paste(vars_to_keep, collapse  = ' + ')) # x

  # run OLS with vars suggested by Elastic net
  model_elastic <- lm(specification, df_fuel)
  return(model_elastic)
}

######### Run all models ------------------------------------

cl <- parallel::makePSOCKcluster(35)
doParallel::registerDoParallel(cl)

all_models_output_nested <- pblapply(X=1:length(all_models),
                              FUN=function(i){ # i <- 1
                                # get model names
                                model_name_lm <- paste0(names(all_models)[i],'_lm')
                                model_name_el <- paste0(names(all_models)[i],'_el')


                                # run models
                                estimates_lm <- lm(formula = all_models[[i]], data=df_fuel)
                                estimates_el <- elastic_model( all_models[[i]] )

                                # organize models output
                                output <- list(estimates_lm, estimates_el)
                                names(output) <- list(model_name_lm, model_name_el)
                                return(output)
                                }
                              )

# unnest models recover their names
all_models_output <- unlist(all_models_output_nested, recursive = FALSE)
models_names <- unlist(all_models_output) %>% names()
models_names <- gsub("\\..*","", models_names) %>% unique()

# get AIC values
aic_list <- lapply(all_models_output, FUN = function(i){round(AIC(i),1)})

# save output table
stargazer(all_models_output,
          #object.names = TRUE,
          #column.labels= c('a', 'b', 'c', 'd', 'e', 'f'),
          #column.labels= models_names,
          out.header=F,
          out = "energy_all_models_noveic.html",
          p.auto=F,
          add.lines = list(c('AIC', paste0(aic_list)))
          )
# a
2+2


##### problemas de multicol. --------------------------------------------------

# fragmentacao e tamanho
cor(df_fuel$x_proportion_largest_patch, df_fuel$x_avg_cell_distance)

# densidade e tamanho
cor(df_fuel$x_density_pop_10km_total_2014, df_fuel$x_avg_cell_distance)


# renda e automovel
cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_prop_autos_dom)


 x_intersection_density_km , NAO iamos usar a medida de rosa do ventos?


# renda e forma
 cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_density_pop_10km_total_2014)
 cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_proportion_largest_patch)
 cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_avg_cell_distance)
 cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_land_use_mix)
 cor(df_fuel$x_wghtd_mean_household_income_per_capita, df_fuel$x_prop_pop_consolidated_area_2014)



 cor(df_fuel$x_density_pop_10km_total_2014, df_fuel)


temp_spec <- paste(vars_dep, vars_forma)
linear_raw <- lm(formula = temp_spec, data=temp_df)
summary(linear_raw)

#VIF
vif_test <- mctest::imcdiag(linear_raw, method ='VIF')
vif_test

# identify multicolinear variables
colinear_varnames <- subset(as.data.frame(vif_test$alldiag), VIF   ==T) %>% rownames()
colinear_varnames <- names(temp_df)[names(temp_df) %in% colinear_varnames]
qgraph::qgraph( cor( dplyr::select(temp_df, colinear_varnames) ) ,
                #theme='gray',
                vsize=10,
                label.cex=4,
                labels=names(dplyr::select(temp_df, colinear_varnames)),
                edge.labels = TRUE,
                layout='spring')



