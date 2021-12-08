rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(tabulizer)
library(Hmisc)
library(patchwork)

# read ------

df_fleet <- readr::read_rds(file = "../../data/urbanformbr/denatran/fleet_age_by_vehicle.rds")

df_pop <- readr::read_rds("../../data/urbanformbr/pca_regression_df/censo.rds")
df_pop <- df_pop[,c("code_urban_concentration","pop_2010")]

# processing -------




tmp_dt <- list(data.table::copy(df_fleet)
               ,data.table::copy(df_fleet)[,classe := "all"]) %>%
  data.table::rbindlist() %>%
  data.table::setnames(.,"V1","num_frota") %>%
  .[,lapply(.SD,sum,na.rm = TRUE)
    ,by = .(status,classe,code_urban_concentration)
    ,.SDcols = "num_frota"] %>%
  .[,prop_veh_status := num_frota/sum(num_frota)
    ,by = .(classe, code_urban_concentration)] %>%
  data.table::setkeyv(.,cols = c("code_urban_concentration","classe","status"))
# .[,lapply(.SD,weighted.mean,V1)
#   ,by = .(code_urban_concentration,classe),.SDcols = "age"] %>%
# .[,weighted.mean(x = age_2020,w = V1)
#   ,by = .(code_urban_concentration,classe)] %>%

tmp_age <- list(data.table::copy(df_fleet)
                ,data.table::copy(df_fleet)[,classe := "all"]) %>%
  data.table::rbindlist() %>%
  data.table::setnames(.,"V1","num_frota") %>%
  .[,lapply(.SD,weighted.mean,num_frota)
    ,by = .(code_urban_concentration,classe),.SDcols = "age"] %>%
  data.table::setkeyv(.,cols = c("code_urban_concentration","classe"))



prop_car_new <- data.table::copy(tmp_dt)[classe == "moto"  & status == "0-10"]
prop_car_old <- data.table::copy(tmp_dt)[classe == "moto"  & status != "0-10"]
prop_mot_new <- data.table::copy(tmp_dt)[classe == "carro" & status == "0-10"]
prop_mot_old <- data.table::copy(tmp_dt)[classe == "carro" & status != "0-10"]


readr::write_rds(tmp_age
                 ,"../../data/urbanformbr/pca_regression_df/fleet_age_mean_df.rds")

readr::write_rds(tmp_dt[,!"num_frota"]
                 ,"../../data/urbanformbr/pca_regression_df/fleet_age_class_df.rds")

