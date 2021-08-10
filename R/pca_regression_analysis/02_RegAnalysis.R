
library('DataExplorer')
library('dplyr')
library('tidyverse')
library('caret')
library('data.table')
library('lmtest')
library('faux')
library('corrplot')
library('car')
library('ggcorrplot')
library('doParallel')
library('normtest')

#LOAD DATA BASE
pca_regression_df_ready_to_use <- readRDS(
  "../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")

onlynumbersbase <- pca_regression_df_ready_to_use %>% mutate(
  i_name_urban_concentration=NULL,i_name_uca_case=NULL,i_code_urban_concentration=NULL)

plot_qq(onlynumbersbase)


### THIS TRANSFORMATION FOR ONLYNUMBERS BASE IS BASED ON BRIEF CONCEPTUAL DISCUSSION ----

onlynumbersbase <- onlynumbersbase %>% mutate(x_pib_capita_2010=NULL,
x_prop_yellow_men=NULL,x_prop_pes_urban=NULL, x_prop_not_specified_men=NULL,
x_prop_not_specified_women=NULL, x_prop_yellow_women=NULL, x_prop_indigenous_men=NULL,
x_prop_indigenous_women=NULL,x_prop_low_educ,x_prop_informal=NULL,
x_prop_work_same_muni_not_home_office=NULL,x_built_total_geom_growth_1975_2014_consolidada=NULL,
x_built_total_geom_growth_1975_2014_expansao=NULL,x_built_total_geom_growth_1975_2014_total=NULL,
x_density_pop_10km2_consolidada_2014=NULL,x_density_built_10km2_expansao_2014=NULL,
x_density_pop_10km2_total_2014=NULL,x_density_built_10km2_consolidada_2014=NULL,
x_density_built_10km2_expansao_2014=NULL,x_density_built_10km2_total_2014=NULL,
x_dissimilarity_15km=NULL,x_dissimilarity_10km=NULL,
x_pop_total_geom_growth_1975_2015_consolidada=NULL,
x_pop_total_geom_growth_1975_2015_total=NULL,x_pop_total_geom_growth_1975_2015_expansao=NULL,
x_prop_age_65_more=NULL)

plot_qq(onlynumbersbase)

normtest::kurtosis.norm.test(onlynumbersbase,nrepl = 1000)

## logaritmizing ASSIMETRIC VARIABLES (base on qqplot)

lnreghib <- onlynumbersbase %>% dplyr::mutate(
  x_avg_cell_distance=log(onlynumbersbase$x_avg_cell_distance),
  x_avg_cell_distane_w_pop=log(onlynumbersbase$x_avg_cell_distance_w_pop),
  x_mean_slope=log(onlynumbersbase$x_mean_slope),
  x_intersection_density_km=log(onlynumbersbase$x_intersection_density_km),
x_n_patches=log(onlynumbersbase$x_n_patches),x_circuity_avg=log(onlynumbersbase$x_circuity_avg),
x_intersection_count=log(onlynumbersbase$x_intersection_count),
x_density_pop_05km2_consolidada_2014=log(onlynumbersbase$x_density_pop_05km2_consolidada_2014),
x_dissimilarity_5km=log(onlynumbersbase$x_dissimilarity_5km),
x_prop_car_motorcycle_pes=log(onlynumbersbase$x_prop_car_motorcycle_pes),
x_built_total_total_2014=log(onlynumbersbase$x_built_total_total_2014),
x_built_total_consolidada_2014=log(onlynumbersbase$x_built_total_consolidada_2014),
x_prop_work_other_muni=log(onlynumbersbase$x_prop_work_other_muni),
x_prop_formal=log(onlynumbersbase$x_prop_formal),x_prop_dom_urban=log(
  onlynumbersbase$x_prop_dom_urban),
x_prop_black_men=log(onlynumbersbase$x_prop_black_men),x_prop_white_women=log(
  onlynumbersbase$x_prop_white_women),
x_wghtd_mean_density_resident_household=log(
  onlynumbersbase$x_wghtd_mean_density_resident_household),
x_prop_car_motorcycle_dom=log(onlynumbersbase$x_prop_car_motorcycle_dom),
x_pop_2015=log(onlynumbersbase$x_pop_2015),
x_wghtd_mean_density_resident_bedroom=log(onlynumbersbase$x_wghtd_mean_density_resident_bedroom),
y_fuel_consumption_per_capita_2010=log(onlynumbersbase$y_fuel_consumption_per_capita_2010),
y_wghtd_mean_commute_time=log(onlynumbersbase$y_wghtd_mean_commute_time))

summary(lnreghib)

plot_intro(lnreghib)


##########CARET############ ----

###### CARET SAFS CRITERIA APPLICATION

x <- lnreghib %>% dplyr::mutate(y_fuel_consumption_per_capita_2010=NULL,
                                y_wghtd_mean_commute_time=NULL)

y1 <- pca_regression_df_ready_to_use$y_fuel_consumption_per_capita_2010
y2 <- pca_regression_df_ready_to_use$y_wghtd_mean_commute_time

summary(x)
DataExplorer::plot_intro(x)
DataExplorer::plot_histogram(x)
DataExplorer::plot_missing(x)
DataExplorer::plot_qq(x)
DataExplorer::plot_boxplot(x)

# SEGUINDO CRITERIO DO CARET PARA REMOVER VARIÁVEIS COM ALTA COLINEARIDADE

comboInfo <- findLinearCombos(x)
comboInfo
## PARALELIZANDO

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

### CARET
#ctrl <- safsControl(functions = caretSA, improve = 10)

#obj <- safs(x = x,y = y1,
#            iters = 100,
#            safsControl = ctrl,
            ## Now pass options to `train`

#           method = "lm")
#obj

# By cross validation with 0.8 split sample
rfsacontrl <- safsControl(functions = rfSA,
                       method = "repeatedcv",
                       repeats = 10,
                       improve = 15)

set.seed(154)
rf_safuel <- safs(x = x, y = y1,
              iters = 100,
              safsControl = rfsacontrl)
rf_safuel

ggplot(rf_safuel) + theme_bw()


set.seed(100)
rf_sacomute <- safs(x = x, y = y2,
                  iters = 100,
                  safsControl = rfsacontrl)
rf_sacomute

stopCluster(cl)

#REGRESSION WITH OPTIMAL subSET ----

opsetfuel <- rf_safuel$sa$final

depfuel <- "y_fuel_consumption_per_capita_2010"

fuelset <- as.formula(
  paste(depfuel,
        paste(opsetfuel, collapse = " + "),
        sep = " ~ "))
print(fuelset)

regfuel <- lm(fuelset,data=lnreghib)

summary(regfuel)

vif(regfuel)

regfuelmtcln <- lm(y_fuel_consumption_per_capita_2010~x_pop_growth_15_00+x_urban_extent_size_2014+
                      x_wghtd_mean_density_rooms_household+x_prop_white_men+x_prop_low_educ+x_prop_age_65_more+
                      x_prop_work_home_office+x_prop_built_consolidated_area_2014+x_density_pop_05km2_expansao_2014+
                      x_dissimilarity+x_theil_h+x_n_large_patches+x_ratio_circle+D_SistBMT
                      ,data=lnreghib)




summary(regfuelmtcln)

### CORRECTING HETEROCEDASCITITY

lmtest::bptest(regfuel)
lmtest::coeftest(regfuel)

lmtest::bptest(regfuelmtcl)
lmtest::coeftest(regfuelmtcl)


### FOR THE AVG COMMUTING TIME

opsetcomute <- rf_sacomute$sa$final
depfuel <- "y_wghtd_mean_commute_time"

comuteset <- as.formula(
  paste(depfuel,
        paste(opsetcomute, collapse = " + "),
        sep = " ~ "))
print(fuelset)

regcomute <- lm(comuteset,data=lnreghib)

summary(regcomute)

vif(regcomute)

regcomuttlmtcln <- lm(y_fuel_consumption_per_capita_2010~x_pop_growth_15_00+x_urban_extent_size_2014+
                     x_wghtd_mean_density_rooms_household+x_prop_white_men+x_prop_low_educ+x_prop_age_65_more+
                     x_prop_work_home_office+x_prop_built_consolidated_area_2014+x_density_pop_05km2_expansao_2014+
                     x_dissimilarity+x_theil_h+x_n_large_patches+x_ratio_circle+D_SistBMT
                   ,data=lnreghib)




summary(regcomuttlmtcln)

### CORRECTING HETEROCEDASCITITY

lmtest::bptest(regcomute)
lmtest::coeftest(regcomute)

lmtest::bptest(regcomutemtcln)
lmtest::coeftest(regcomutemtcln)


### DETECTING AND CORRECTING HETEROCEDASTICITY

lmtest::bptest(reglist)
lmtest::coeftest(reglist)

lmtest::bptest(reglist2)
lmtest::coeftest(reglist2)

#### CORRELATION analysis ----


cor.test(pca_regression_df_ready_to_use$y_wghtd_mean_commute_time,pca_regression_df_ready_to_use$x_wghtd_mean_household_income_per_capita)

dfx <- select()

ggcorrplot(cor(dfx),tl.cex = 8)


#SAVING MODELS ----

setwd("//storage6/usuarios/Proj_acess_oport/git_luiz/urbanformbr/Outputs")

stargazer::stargazer(regfuel,regfuel1,regcomutime, type = 'html', out = "caretregs")

stargazer::stargazer(regfuelmtcl,regfuel1mtcln,regcomutimemtcl, type = 'html', out = "cleaneregs")

car::scatterplot(y_wghtd_mean_commute_time ~
    log(x_urban_extent_size_2014) | D_SistBMT, data=pca_regression_df_ready_to_use,
    xlab = 'Extensão Urbana em KM', ylab = 'Tempo Médio de Comutação ao Trabalho')



