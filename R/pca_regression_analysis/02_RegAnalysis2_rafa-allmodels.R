library(caret)
library(margins)

# 11.Models

#########  organize model components ---------------------------------------
vars_dep <- 'y_energy_per_capita'  # y_wghtd_mean_commute_time y_energy_per_capita
vars_forma <- '~ f_compact_contig_inter_dens + x_density_pop_01km_2015 + x_land_use_mix  + x_circuity_avg + x_closeness_centrality_avg' #  + x_built_area_coverage_01km_2014 +x_urban_extent_size_2014 + x_prop_pop_consolidated_area_2015
vars_controls <-'+ x_pop_2010 + x_prop_dom_urban + x_wghtd_mean_household_income_per_capita + x_prop_razao_dep + x_mean_slope + x_street_pop + x_pop_growth_1975_2015' # x_prop_high_educ
vars_auto <- '+ x_prop_autos_dom + x_prop_motos_dom'
vars_interactions <- '+ x_density_pop_01km_2015*x_pop_2010 + f_compact_contig_inter_dens*x_pop_2010'


######### create all model specifications ---------------------------------------
model_all <- paste(vars_dep, '~.', vars_interactions)
model_form_inter <- paste(vars_dep, vars_forma, vars_interactions)
model_cntrl <- paste(vars_dep, vars_forma, vars_controls)
model_cntrl_inter <- paste(vars_dep, vars_forma, vars_controls, vars_interactions)
model_cntrl_inter_auto <- paste(vars_dep, vars_forma, vars_controls, vars_interactions, vars_auto)

all_models <- list(model_all, model_form_inter, model_cntrl, model_cntrl_inter, model_cntrl_inter_auto)
names(all_models) <- list( quote(model_all), quote(model_form_inter), quote(model_cntrl), quote(model_cntrl_inter), quote(model_cntrl_inter_auto))


### Marginal effect ------------------------
6666666666666666

# library(margins)
library(marginaleffects)

tem_model <- lm( model_all, data=df_fuel)
summary(tem_model)


tem_modely <- lm( y_energy_per_capita        ~f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg +x_land_use_mix+x_mean_slope+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_wghtd_mean_household_income_per_capita, data=df_fuel)
tem_modelx <- lm( x_density_pop_01km_2015    ~f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_land_use_mix+x_mean_slope+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_wghtd_mean_household_income_per_capita,  data=df_fuel)

resy <- residuals(tem_modely)
resx <- residuals(tem_modelx)

plot(resx, resy)

a <- data.frame(x=resx,y=resy)

ggplot(data=a, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method = 'lm',  color='black')+
  geom_smooth(method = 'lm',  formula = y ~ x + I(x^2))+
geom_smooth(color='red')



# units::units_options(allow_mixed = TRUE)
# m <- margins::margins(model = tem_model)

m <- marginaleffects::marginaleffects(model = tem_model)

# average marginal effect
summary(m)
tidy(m)






# ?
pred <- marginaleffects::predictions(model = tem_model, newdata = df_fuel)


# counterfactual /// https://vincentarelbundock.github.io/marginaleffects/articles/mfx.html
nd <- counterfactual(x_pop_2010 = log(2000000), model = tem_model)

marginaleffects(tem_model, newdata = nd) %>%
  group_by(term) %>%
  summarize(across(dydx:std.error, median))


df_raw[order(y_energy_per_capita), .(i_name_urban_concentration , y_energy_per_capita)] %>% head()
df_raw[order(y_energy_per_capita), .(i_name_urban_concentration , y_energy_per_capita)] %>% tail()

df_raw[ i_name_urban_concentration %like% 'Arara']
summary(df_raw$y_energy_per_capita)

cor(df_fuel$x_urban_extent_size_2014,
    df_fuel$x_circuity_avg)

plot(df_fuel$x_betweenness_centrality_avg,
     df_fuel$y_energy_per_capita)




# interaction ----------------------------------
6666666666666666

sd(df_raw $x_pop_2010) - mean(df_raw $x_pop_2010)

interactions::interact_plot(tem_model,
              pred = x_density_pop_01km_2015, # x_density_pop_01km_2015 f_compact_contig_inter_dens
              modx = x_pop_2010,
              plot.points = T, jitter = 0.1,
              #int.width = .9, interval=T,
              linearity.check = F)

plot_cme(tem_model, effect = "x_density_pop_01km_2015", condition = c("x_pop_2010"))
plot_cme(tem_model, effect = "f_compact_contig_inter_dens", condition = c("x_pop_2010"))


pred <- ggpredict(model = tem_model, terms = c('x_density_pop_01km_2015', 'x_pop_2010'))

ggplot() +
  geom_line(data=pred, aes(x=exp(x), y=predicted, ymax=conf.high, ymin=conf.low, color=group, fill=group)) +
  geom_ribbon(data=pred, aes(x=exp(x), y=predicted, ymax=conf.high, ymin=conf.low, color=group, fill=group),alpha=.1, color=NA)
#  geom_point(data=df_fuel, aes(x=x_pop_2010), y=y_energy_per_capita)
#  scale_x_continuous(limits = c(100000, 20000000))





######### create function to run ElasticNet model ------------------------------------
elastic_model <- function(model, ntimes = 10){ # model <- all_models[[2]]

  # cl <- parallel::makePSOCKcluster(30)
  # doParallel::registerDoParallel(cl)

  # Set training control
  # K fold cross validation
  train_control <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = ntimes,
                                search = "random",
                                verboseIter = TRUE)

  elastic_net_model <- train(as.formula(model),
                             data       = df_fuel,
                             method     = "glmnet",
                             preProcess = c("center", "scale"),
                             tuneLength = ntimes,
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
  specification = paste0('y_energy_per_capita ~ ',  # y
                         paste(vars_to_keep, collapse  = ' + ')) # x

  # run OLS with vars suggested by Elastic net
  model_elastic <- lm(specification, df_fuel)
  return(model_elastic)
}

######### Run all models ------------------------------------

cl <- parallel::makePSOCKcluster(35)
doParallel::registerDoParallel(cl)

all_models_output_nested <- pblapply(X=1:length(all_models),
                              FUN=function(i){ # i <- 4
                                # get model names
                                model_name_lm <- paste0(names(all_models)[i],'_lm')
                                model_name_el <- paste0(names(all_models)[i],'_el')


                                # run models
                                estimates_el <- elastic_model( all_models[[i]] )
                                estimates_lm <- lm(formula = all_models[[i]], data=df_fuel)
                                # summary(estimates_lm)


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
          # column.labels= models_names,
           out = "energy_all_models_noveic.html",
          type = 'text',
          p.auto=F,
          ci=F,
          t.auto=F,
          add.lines = list(c('AIC', paste0(aic_list)))
        #  keep.stat=c('aic')
          )





##### problemas de multicol. --------------------------------------------------


 cor(df_log$x_density_pop_01km_2015, df_log$x_pop_2010)

 ggplot(data=df_raw, aes(x=x_density_pop_01km_2015, y=y_energy_per_capita)) +
   geom_point() +
   geom_smooth()

 df_raw %>% subset(x_pop_2010>200000) %>%
 ggplot( aes(x=x_density_pop_01km_2015, y=y_energy_per_capita)) +
   geom_point() +
   geom_smooth() +
   scale_x_log10() +
   scale_y_log10()

 df_raw %>% subset(x_density_pop_01km_2015 == min(df_raw$x_density_pop_01km_2015))

### check  multicollinearity-------------
 library(mctest)
 library(qgraph)




library(ggeffects)
library(sjPlot)

 # plot model
plot_model(tem_model)



#VIF
vif_test <- mctest::imcdiag(tem_model, method ='VIF')
vif_test

# identify multicolinear variables
colinear_varnames <- subset(as.data.frame(vif_test$alldiag), VIF   ==T) %>% rownames()
colinear_varnames <- names(df_fuel)[names(df_fuel) %in% colinear_varnames]
qgraph::qgraph( cor( dplyr::select(df_fuel, colinear_varnames) ) ,
                #theme='gray',
                vsize=10,
                label.cex=4,
                labels=names(dplyr::select(df_fuel, colinear_varnames)),
                edge.labels = TRUE,
                layout='spring')


# check partial correlations
library(ppcor)
pcor(df_fuel, method = "pearson")



# compare models performance ----------------------
tem_model <- lapply(X=all_models, FUN= lm, data=df_fuel)
performance::check_model(tem_model[[2]])
performance::compare_performance(tem_model, rank = T) # %>% plot()

test_bf(tem_model)

