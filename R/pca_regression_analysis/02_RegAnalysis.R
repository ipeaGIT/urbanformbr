
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
## Data Transforming
onlynmbrhip <- onlynumbersbase %>%
  dplyr::mutate_if(is.numeric,~log(((.x + .x^2+1)^(1/2))))

##########CARET############ ----

###### CARET SAFS CRITERIA APPLICATIon

x <- onlynmbrhip %>% dplyr::mutate(y_fuel_consumption_per_capita_2010=NULL,
                                y_wghtd_mean_commute_time=NULL)

y1 <- onlynmbrhip$y_fuel_consumption_per_capita_2010
y2 <- onlynmbrhip$y_wghtd_mean_commute_time

summary(x)
DataExplorer::plot_intro(x)
DataExplorer::plot_histogram(x)
DataExplorer::plot_missing(x)
DataExplorer::plot_qq(x)

# SEGUINDO CRITERIO DO CARET PARA REMOVER VARIÁVEIS COM ALTA COLINEARIDADE

comboInfo <- findLinearCombos(x)
comboInfo
## PARALELIZANDO

cl <- makePSOCKcluster(3)
registerDoParallel(cl)

### CARET
#ctrl <- safsControl(functions = caretSA, improve = 10)

#obj <- safs(x = x,y = y1,
#            iters = 100,
#            safsControl = ctrl,
            ## Now pass options to `train`

#           method = "lm")
#obj

# By cross validation with 80% split sample
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


set.seed(154)
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
regfuel <- lm(fuelset,data=lnreghib)

summary(regfuel)

vif(regfuel)

viffuel <- as.numeric(vif(regfuel))
dfviffuel <- as.numeric(cbind(viffuel,opsetfuel))

coefiviffuel <- filter(dfviffuel, viffuel>=10)
coefiviffuel <- as.data.frame(coefiviffuel)

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



