
library('DataExplorer')
library('tidyverse')
library('caret')
library('lmtest')
library('faux')
library('corrplot')
library('car')
library('ggcorrplot')
library('doParallel')
library('normtest')
library('VGAM')
library('pivottabler')

#LOAD DATA BASE
pca_regression_df_ready_to_use <- readRDS(
  "../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")

onlynumbersbase <- pca_regression_df_ready_to_use %>% mutate(
  i_name_urban_concentration=NULL,i_name_uca_case=NULL,i_code_urban_concentration=NULL)

plot_qq(onlynumbersbase)


plot_qq(onlynumbersbase,nrow = 2, ncol = 2) + theme(axis.text =)

normtest::kurtosis.norm.test(onlynumbersbase$x_pop_2015,nrepl = 100)



## Data Transforming -----
## YEO JOHNSON TRANSFORMATIOn
basenumberyeo <- VGAM::yeo.johnson(onlynumbersbase,lambda = 0) ##ILHEUS DA PROBLEMA

basenumberyeo <- basenumberyeo  %>% mutate(D_SistBMT=onlynumbersbase$D_SistBMT,
d_isolated_muni=onlynumbersbase$d_isolated_muni,d_large_uca_pop=onlynumbersbase$d_large_uca_pop)

basenumberyeo <- basenumberyeo[-c(72)]
## HIPERBOLIC TRANSFORMATION
basenumberhip <- onlynumbersbase %>%
  dplyr::mutate_if(is.numeric,~log(((.x + .x^2+1)^(1/2))))
basenumberhip  <- basenumberhip  %>% mutate(D_SistBMT=onlynumbersbase$D_SistBMT,
d_isolated_muni=onlynumbersbase$d_isolated_muni,d_large_uca_pop=onlynumbersbase$d_large_uca_pop)

### LOG FOR POSITIVE VALUES VARIABLES
prblmtc_detect <- function(x) {

  return(ifelse(x %>% min() <= 0,FALSE,TRUE))

}

basenumberlog <- onlynumbersbase %>%
  dplyr::mutate_if(prblmtc_detect,log)

## DATA PLOT

matplot(basenumberhip$y_fuel_consumption_per_capita_2010,
        onlynumbersbase$y_fuel_consumption_per_capita_2010, type = "l", ylim = c(-10, 500)
        , lwd = 2, lty = 1:lltry,
        ylab = "Yeo-Johnson transformation", col = 1:lltry, las = 1,
        main = "Yeo-Johnson transformation with some values of lambda")
abline(v = 0, h = 0)
legend(x = 1, y = -0.5, lty = 1:lltry, legend = as.character(ltry),
       lwd = 2, col = 1:lltry)

###CARET############ ----

### FEATURE ANNEALING OUTPUTS

rf_safuellog1000 <- readRDS("../urbanformbr/Outputs/Caret/rf_safuellog1000iteracoes")

rf_sacomutelog1000 <- readRDS("../urbanformbr/Outputs/Caret/rf_sacomutelog1000iteracoes")

###### CARET SAFS CRITERIA APPLICATIon

xlog <- basenumberlog %>% dplyr::mutate(y_fuel_consumption_per_capita_2010=NULL,
                                y_wghtd_mean_commute_time=NULL)

y1log <- basenumberlog$y_fuel_consumption_per_capita_2010
y2log <- basenumberlog$y_wghtd_mean_commute_time

summary(x)
DataExplorer::plot_intro(x)
DataExplorer::plot_histogram(x)
DataExplorer::plot_missing(x)
DataExplorer::plot_qq(x)

# SEGUINDO CRITERIO DO CARET PARA REMOVER VARIÁVEIS COM ALTA COLINEARIDADE

comboInfo <- findLinearCombos(xlog)
comboInfo

xhip <- onlynmbrhip %>% mutate(x_prop_car_pes=NULL,
x_prop_motorcycle_pes=NULL,x_prop_car_and_motorcycle_pes=NULL)

## PARALELIZANDO

cl <- makePSOCKcluster(4)
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

set.seed(0)

rf_safuellog1000 <- safs(x = xlog, y = y1log,
              iters = 1000,
              safsControl = rfsacontrl)

#### PLOTING MEAN SQUARED ERROR X NUMBER OF ITERACTIONS

ggplot(rf_sacomutelog200) + theme_bw()

### FEATURE ANNEALING FOR OPTIMAL COMMUTE SET PREDICTOR

set.seed(0)
rf_sacomutelog1000 <- safs(x = xlog, y = y2log,
                  iters = 1000,
                  safsControl = rfsacontrl)

saveRDS(rf_sacomutelog1000,file = "rf_sacomutelog1000iteracoes")


### DESATIVANDO A PARALELIZACAO

stopCluster(cl)

#REGRESSION WITH CARET'S FEATURE ANNEALING OPTIMAL SUBSETs ----

opxsetfuellog <- rf_safuellog1000$sa$final

depfuel <- "y_fuel_consumption_per_capita_2010"

setfuellog <- as.formula(
  paste(depfuel,
        paste(opxsetfuellog, collapse = " + "),
        sep = " ~ "))
regfuellog1000 <- lm(setfuellog,data=basenumberlog)

summary(regfuellog1000)

### MODEL CORRECTING FOR MULTICOLINEARITY

dfxviffuellog <- as.data.frame(vif(regfuellog3))
dfxviffuellog <- filter(dfxviffuellog, vif(regfuellog3) <= 10)
dfxviffuellog <- row.names(dfxviffuellog)

depfuel <- "y_fuel_consumption_per_capita_2010"

fuellogvifset <- as.formula(
  paste(depfuel,
        paste(dfxviffuellog, collapse = " + "),
        sep = " ~ "))
regfuellogmtcl3 <- lm(fuellogvifset,data=basenumberlog)

summary(regfuellogmtcl3)



### CORRECTING HETEROCEDASCITITY

lmtest::bptest(regfuelyeo)
regfuellogmtclseed10<- lmtest::coeftest(regfuelyeo)

lmtest::bptest(regfuelyeomtcl)
lmtest::coeftest(regfuelyeomtcl)


### FOR THE AVG COMMUTING TIME

opsetcomutelog <- rf_sacomutelog1000$sa$final
depcomute <- "y_wghtd_mean_commute_time"

comutesetlog <- as.formula(
  paste(depfuel,
        paste(opsetcomutelog, collapse = " + "),
        sep = " ~ "))
regcomutelog <- lm(comutesetlog,data=basenumberlog)

summary(regcomutelog)

### MODEL CORRECTING FOR MULTICOLINEARITY

dfxvifcomutelog<- as.data.frame(vif(regcomutelog))
dfxvifcomutelog <- filter(dfxvifcomutelog, vif(regcomutelog) <= 10)
dfxvifcomutelog <- row.names(dfxvifcomutelog)

depfuel <- "y_wghtd_mean_commute_time"

comuteviflogset <- as.formula(
  paste(depfuel,
        paste(dfxvifcomutelog, collapse = " + "),
        sep = " ~ "))
regcomutelogmtcl <- lm(comuteviflogset,data=basenumberlog)

summary(regcomutelogmtcl)


### CORRECTING HETEROCEDASCITITY

lmtest::bptest(regcomute)
lmtest::coeftest(regcomute)

lmtest::bptest(regcomutemtcln)
lmtest::coeftest(regcomutemtcln)


#### CORRELATION analysis ----


cor.test(pca_regression_df_ready_to_use$y_wghtd_mean_commute_time,pca_regression_df_ready_to_use$x_wghtd_mean_household_income_per_capita)

dfx <- select(opsetcomutehip)

ggcorrplot(cor(dfx),tl.cex = 8)


#SAVING MODELS ----

setwd("//storage6/usuarios/Proj_acess_oport/git_luiz/urbanformbr/Outputs/Regs/")

stargazer::stargazer(regfuellog200,regfuellog500,regfuellog1000,type = 'html', out = "caretfuelregs")

stargazer::stargazer(regfuellogmtclseed10,regfuellogmtclseed100,regfuellogmtcl3,
                     type = 'html', out = "cleaneregs")

car::scatterplot(y_wghtd_mean_commute_time ~
    log(x_urban_extent_size_2014) | D_SistBMT, data=pca_regression_df_ready_to_use,
    xlab = 'Extensão Urbana em KM', ylab = 'Tempo Médio de Comutação ao Trabalho')




