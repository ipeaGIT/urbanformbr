# library(caret)
# library(margins)
# library(interactions)
# # library(stargazer)
# # library(fixest)
#
#
# #########  model specifications based on DAG ---------------------------------------
#
# all_models_specs <- model_spec_all
#
#
# ######### create function to run ElasticNet model ------------------------------------
# cross_validation <- function(model, ntimes = 10){ # model <- all_models[[1]]
#
#   # cl <- parallel::makePSOCKcluster(30)
#   # doParallel::registerDoParallel(cl)
#
#   # Set training control
#   # K fold cross validation
#   train_control <- trainControl(method = "repeatedcv",
#                                 number = 2,            # sample groups
#                                 repeats = ntimes,
#                                 search = "random",
#                                 verboseIter = TRUE)
#
#   temp_model <- train(as.formula(model),
#                              data       = df_fuel,
#                              method     = "lm",  # lm rlm  glmnet[elastic]
#                              preProcess = c("center", "scale"),
#                              tuneLength = ntimes,
#                              trControl  = train_control)
#
#   # we need this to use stargazer
#   ols <- lm(as.formula(model), data = df_fuel)
#   temp_model$finalModel$call <- ols$call
#
#   # # Coeficients of the best model
#   # params <- coef(temp_model$finalModel,
#   #                s = temp_model$bestTune$lambda)
#
#   # # stop parallel
#   # stopCluster(cl)
#   # env <- foreach:::.foreachGlobals
#   # rm(list=ls(name=env), pos=env)
#
#   return(temp_model$finalModel)
# }
#
# ######### Run all models ------------------------------------
#
# cl <- parallel::makePSOCKcluster(30)
# doParallel::registerDoParallel(cl)
#
# all_models_output_nested <- pblapply( X= all_models_specs, FUN=cross_validation )
#
#
#
# # get AIC values
# aic_list <- lapply(all_models_output_nested, FUN = function(i){round(AIC(i),1)})
#
#
#
# # save output table
# stargazer(
#   all_models_output_nested,
#   out = "./output/regression_table_minimum.html",
#   # column.labels= c('all', 'landuse', 'density', 'fcompact', 'circuity', 'closeness'),
#   type = 'text',
#   p.auto=F,
#   ci=T,
#   t.auto=F,
#   add.lines = list(c('AIC', paste0(aic_list)))
# )
#
# # save model
# write_rds(all_models_output_nested[[1]], './output/model_energy_dag_lm_sem-fronteira_cross-validation.rds')
#
#
#
#
#
#  # plot model -------------
# library(ggeffects)
# library(sjPlot)
# plot_model(model_all)
#
#
#
#
#
#
# # interaction plot  -------------
# cor(df_fuel$y_energy_per_capita, df_fuel$x_compacity)
# cor(df_fuel$y_energy_per_capita, df_fuel$x_intersection_density_km)
# cor(df_fuel$y_energy_per_capita, df_fuel$x_contiguity)
#
# cor(df_fuel$y_energy_per_capita, df_fuel$f_compact_contig_inter_dens)
#
#
#
#
# ### problema compacity positivo 666
# aaa <-  lm( y_energy_per_capita ~x_compacity+x_intersection_density_km+x_contiguity, data=df_fuel)
# summary(aaa)
#
# x_compacity
# f_compact_contig_inter_dens
# x_pop_2010
# interactions::interact_plot(aaa,
#                             pred = 'f_compact_contig_inter_dens',
#                             modx = 'x_urban_extent_size_2014',
#                             plot.points = T)
#
# df_fuel$x_urban_extent_size_2014 <- as.numeric(df_fuel$x_urban_extent_size_2014)
#
#
#
#
#
