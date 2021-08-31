
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

normtest::kurtosis.norm.test(onlynumbersbase$x_pop_2015,nrepl = 100)

## Data Transforming -----
## YEO JOHNSON TRANSFORMATIOn
basenumberyeo <- VGAM::yeo.johnson(onlynumbersbase,lambda = 0) ##ILHEUS DA PROBLEMA
basenumberyeo <- basenumberyeo[-c(72)]

## HIPERBOLIC TRANSFORMATION
onlynmbrhip <- onlynumbersbase %>%
  dplyr::mutate_if(is.numeric,~log(((.x + .x^2+1)^(1/2))))
onlynmbrhip  <- onlynmbrhip  %>% mutate(D_SistBMT=onlynumbersbase$D_SistBMT)

### LOG FOR POSITIVE VALUES VARIABLES
prblmtc_detect <- function(x) {

  return(ifelse(x %>% min() <= 0,FALSE,TRUE))

}

lognumbrbase <- onlynumbersbase %>%
  dplyr::mutate_if(prblmtc_detect,log)
## DATA PLOT

matplot(onlynmbrhip$y_fuel_consumption_per_capita_2010,
        onlynumbersbase$y_fuel_consumption_per_capita_2010, type = "l", ylim = c(-10, 500)
        , lwd = 2, lty = 1:lltry,
        ylab = "Yeo-Johnson transformation", col = 1:lltry, las = 1,
        main = "Yeo-Johnson transformation with some values of lambda")
abline(v = 0, h = 0)
legend(x = 1, y = -0.5, lty = 1:lltry, legend = as.character(ltry),
       lwd = 2, col = 1:lltry)

##########CARET############ ----

### FEATURE ANNEALING OUTPUTS - YOU CAN GO STRAIGHT TO THE NEXT SECTION IF YOU LOAD THEM -

rf_safuellog1000 <- readRDS("../urbanformbr/Outputs/Caret/rf_safuellog1000iteracoes")

rf_sacomutelog1000 <- readRDS("../urbanformbr/Outputs/Caret/rf_sacomutelog1000iteracoes")


###### CARET SAFS CRITERIA APPLICATION (FROM THE BEGINNING)

xlog <- basenumberyeo %>% dplyr::mutate(y_fuel_consumption_per_capita_2010=NULL,
                                y_wghtd_mean_commute_time=NULL)

y1yeo <- basenumberyeo$y_fuel_consumption_per_capita_2010
y2yeo <- basenumberyeo$y_wghtd_mean_commute_time

summary(x)
DataExplorer::plot_intro(x)
DataExplorer::plot_histogram(x)
DataExplorer::plot_missing(x)
DataExplorer::plot_qq(x)

# SEGUINDO CRITERIO DO CARET PARA REMOVER VARIÁVEIS COM ALTA COLINEARIDADE

comboInfo <- findLinearCombos(xyeo)
comboInfo
## PARALELIZANDO 

cl <- makePSOCKcluster(3)
registerDoParallel(cl)

### CARET
#ctrl <- safsControl(functions = "lm", improve = 10)

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

set.seed(1540)
rf_safuel <- safs(x = xyeo, y = y1yeo,
              iters = 100,
              safsControl = rfsacontrl)
rf_safuelyeo <- rf_safuel
rf_safuelyeo

ggplot(rf_safuel) + theme_bw()


set.seed(154)
rf_sacomutehip <- safs(x = xhip, y = y2hip,
                  iters = 100,
                  safsControl = rfsacontrl)
rf_sacomute

stopCluster(cl)

#REGRESSION WITH OPTIMAL SUbSET ----

opsetfuelyeo <- rf_safuelyeo$sa$final

depfuel <- "y_fuel_consumption_per_capita_2010"

fuelhipset <- as.formula(
  paste(depfuel,
        paste(opsetfuelyeo, collapse = " + "),
        sep = " ~ "))
regfuelyeo <- lm(fuelhipset,data=basenumberyeo)

summary(regfuelyeo)

### MODEL CORRECTING FOR MULTICOLINEARITY

dfxviffuelyeo <- as.data.frame(vif(regfuelyeo))
dfxviffuelyeo <- filter(dfxviffuelyeo, vif(regfuelyeo) <= 10)
dfxviffuelyeo <- row.names(dfxviffuelyeo)

depfuel <- "y_fuel_consumption_per_capita_2010"

fuelyeovifset <- as.formula(
  paste(depfuel,
        paste(dfxviffuelyeo, collapse = " + "),
        sep = " ~ "))
regfuelyeomtcl <- lm(fuelyeovifset,data=onlynmbryeo)

summary(regfuelyeomtcl)



### CORRECTING HETEROCEDASCITITY

lmtest::bptest(regfuelyeo)
lmtest::coeftest(regfuelyeo)

lmtest::bptest(regfuelmtcl)
lmtest::coeftest(regfuelmtcl)


### FOR THE AVG COMMUTING TIME

opsetcomuteyeo <- rf_sacomuteyeo$sa$final
depcomute <- "y_wghtd_mean_commute_time"

comutesetyeo <- as.formula(
  paste(depfuel,
        paste(opsetcomuteyeo, collapse = " + "),
        sep = " ~ "))
regcomuteyeo <- lm(comutesetyeo,data=basenumberyeo)

summary(regcomuteyeo)

### MODEL CORRECTING FOR MULTICOLINEARITY

dfxvifcomutehip<- as.data.frame(vif(regcomutehip))
dfxvifcomutehip <- filter(dfxvifcomutehip, vif(regcomutehip) <= 10)
dfxvifcomutehip <- row.names(dfxvifcomutehip)

depfuel <- "y_wghtd_mean_commute_time"

comutevifyeoset <- as.formula(
  paste(depfuel,
        paste(dfxvifcomuteyeo, collapse = " + "),
        sep = " ~ "))
regcomuteyeomtcl <- lm(comutevifyeoset,data=onlynmbryeo)

summary(regcomuteyeomtcl)


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

stargazer::stargazer(regfuelhip,regfuelyeo,regcomutehip,regcomuteyeo, type = 'html', out = "caretfuelregs")

stargazer::stargazer(regfuelyeomtcl,regfuelhipmtcl, regcomuteyeomtcl,
                     regcomutehipmtcl,type = 'html', out = "cleaneregs")

car::scatterplot(y_wghtd_mean_commute_time ~
    log(x_urban_extent_size_2014) | D_SistBMT, data=pca_regression_df_ready_to_use,
    xlab = 'Extensão Urbana em KM', ylab = 'Tempo Médio de Comutação ao Trabalho')



