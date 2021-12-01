rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(tabulizer)
library(Hmisc)
library(patchwork)

# read ------

tmp_dt <- readr::read_rds(file = "../../data/urbanformbr/denatran/list_cars_motos.rds")

df_raw <- readr::read_rds('../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds')


# processing -------


tmp_dt <- list(tmp_dt
                ,data.table::copy(tmp_dt)[,classe := "all"]) %>%
  data.table::rbindlist() %>%
  .[,lapply(.SD,sum,na.rm = TRUE)
    ,by = .(status,classe,code_urban_concentration)
    ,.SDcols = "V1"] %>%
  .[,prop := V1/sum(V1)
    ,by = .(classe, code_urban_concentration)] %>%
  # .[,lapply(.SD,weighted.mean,V1)
  #   ,by = .(code_urban_concentration,classe),.SDcols = "age"] %>%
  # .[,weighted.mean(x = age_2020,w = V1)
  #   ,by = .(code_urban_concentration,classe)] %>%
  .[order(code_urban_concentration),]








tmp_dt[classe == "moto" & status == "0-10",]$prop %>% summary()
tmp_dt[classe == "carro" & status == "0-10",]$prop %>% summary()
tmp_dt[df_raw,on = c("code_urban_concentration"=
                       "i_code_urban_concentration")
       ,`:=`(
         income  = i.x_wghtd_mean_household_income_per_capita,
         x_pop_2010 = i.x_pop_2010,
         y_energy_per_capita = i.y_energy_per_capita)]


tmp_dt[,x_log_pop := log(x_pop_2010)]
tmp_dt[,veh_capita := V1/x_pop_2010]


tmp_dt1 <- data.table::copy(tmp_dt) %>%
  .[classe != "all",] %>%
  .[,sum(V1),by = .(code_urban_concentration,classe)] %>%
  .[,prop := V1/sum(V1),by = .(code_urban_concentration)] %>%
  .[order(code_urban_concentration),]

tmp_dt1[df_raw,on = c("code_urban_concentration"=
                       "i_code_urban_concentration")
       ,`:=`(
         income  = i.x_wghtd_mean_household_income_per_capita,
         x_pop_2010 = i.x_pop_2010,
         y_energy_per_capita = i.y_energy_per_capita)]


ggplot(tmp_dt1[classe == "carro",])+
  geom_point(aes(x = income,y = prop))+
  labs(y= "razao carro/(moto + carro)")

cor.test(tmp_dt1[classe == "carro",]$income,
         tmp_dt1[classe == "carro",]$prop)


p1 <- ggplot(tmp_dt[classe == "carro" & status == "0-10" ,])+
  geom_point(aes(x = income,y=veh_capita))+
  labs(y= "carro novo per cap.")
p2 <- ggplot(tmp_dt[classe == "carro" & status == "11+120" ,])+
  geom_point(aes(x = income,y=veh_capita))+
  labs(y= "carro velho per cap.")
p3 <- ggplot(tmp_dt[classe == "carro" & status == "0-10" ,])+
  geom_point(aes(x = income,y=prop))+
  labs(y= "proporcao carro nova")
p4 <- ggplot(tmp_dt[classe == "carro" & status == "11+120" ,])+
  geom_point(aes(x = income,y=prop))+
  labs(y= "proporcao carro velha")

(p1 + p2) / (p3 + p4)


p1 <- ggplot(tmp_dt[classe == "moto" & status == "0-10" ,])+
  geom_point(aes(x = income,y=veh_capita))+
  labs(y= "moto novo per cap.")
p2 <- ggplot(tmp_dt[classe == "moto" & status == "11+120" ,])+
  geom_point(aes(x = income,y=veh_capita))+
  labs(y= "moto velho per cap.")
p3 <- ggplot(tmp_dt[classe == "moto" & status == "0-10" ,])+
  geom_point(aes(x = income,y=prop))+
  labs(y= "proporcao moto nova")
p4 <- ggplot(tmp_dt[classe == "moto" & status == "11+120" ,])+
  geom_point(aes(x = income,y=prop))+
  labs(y= "proporcao moto velha")

(p1 + p2) / (p3 + p4)


# energy
p1 <- ggplot(tmp_dt[classe == "carro" & status == "0-10" ,])+
  geom_point(aes(x = y_energy_per_capita,y=veh_capita))+
  labs(y= "carro novo per cap.")
p2 <- ggplot(tmp_dt[classe == "carro" & status == "11+120" ,])+
  geom_point(aes(x = y_energy_per_capita,y=veh_capita))+
  labs(y= "carro velho per cap.")
p3 <- ggplot(tmp_dt[classe == "carro" & status == "0-10" ,])+
  geom_point(aes(x = y_energy_per_capita,y=prop))+
  labs(y= "proporcao carro nova")
p4 <- ggplot(tmp_dt[classe == "carro" & status == "11+120" ,])+
  geom_point(aes(x = y_energy_per_capita,y=prop))+
  labs(y= "proporcao carro velha")

(p1 + p2) / (p3 + p4)


cor.test(tmp_dt[classe == "carro" & status == "11+120" ,y_energy_per_capita],
         tmp_dt[classe == "carro" & status == "11+120" ,veh_capita])
