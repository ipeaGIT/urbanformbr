# Import packages ----

rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(tabulizer)
library(Hmisc)


# Read data -----

# * Engine size by vehicle (abraciclo) -----
engine_moto_df <- readr::read_rds("../../data/urbanformbr/emissions/fleet_mc.rds")


# * Fleet by age (DENATRAN) -----
fleet_car_raw <- readr::read_rds("../../data/urbanformbr/emissions/fleet_pc_by_age.rds")
fleet_mot_raw <- readr::read_rds("../../data/urbanformbr/emissions/fleet_mc_by_age.rds")


# * Fleet by age and fuel (CETESB) -----
fuel_age_raw <- readr::read_rds("../../data/urbanformbr/emissions/fuel_by_veh-age.R")

# * Fuel ratio by urban area (ANP) -----
ratio_urban_raw <- readr::read_rds("../../data/urbanformbr/fuel/fuel_urban_areas.rds")

# Treat data-----

data.table::setnames(fleet_mot_raw,"V1","total")
data.table::setnames(fleet_car_raw,"V1","total")


fuel_age_raw[,max(2011-year),by = veh_type]
fleet_mot_raw[,max(age),by = classe]
fleet_car_raw[,max(age),by = classe]

#  * adjust by age -----

fuel_age <- data.table::copy(fuel_age_raw) %>%
  .[,age := 2011 - year] %>%
  .[, age_adj := data.table::fifelse(age > 31,31,age)]

fleet_mot <- data.table::copy(fleet_mot_raw) %>%
  .[, age_adj := data.table::fifelse(age > 31,31,age)]

fleet_car <- data.table::copy(fleet_car_raw) %>%
  .[, age_adj := data.table::fifelse(age > 31,31,age)]

# * estimate total again -----

fuel_age <- fuel_age[,consumo := sum(consumo)
                     ,by = .(year,fuel,veh_class,veh_type,age_adj)] %>%
  .[,.SD[1],by = .(year,fuel,veh_class,veh_type, age_adj)]

fleet_mot <- fleet_mot[,total := sum(total)
                       ,by = .(classe,code_urban_concentration,age_adj)] %>%
  .[,.SD[1],by =  .(classe,code_urban_concentration,age_adj)]

fleet_car <- fleet_car[,total := sum(total)
                       ,by = .(classe,code_urban_concentration,age_adj)] %>%
  .[,.SD[1],by =  .(classe,code_urban_concentration,age_adj)]


# * Add composition -----


# moto
engine_moto_df <- engine_moto_df[ano < 2011,age := 2011 - as.numeric(ano)]
engine_moto_df <- engine_moto_df[!is.na(age)]


# * MOTO - add engine size (ABRACICLO -> DENATRAN) -----

df_moto <- lapply(unique(engine_moto_df$age),function(i){ #
  dt_tmp <- engine_moto_df[age == i]
  dt_output <- data.table::copy(fleet_mot) %>%
    .[age_adj == i,] %>%
    .[,`:=`(s1 = dt_tmp[size == "s1",perc],
            s2 = dt_tmp[size == "s2",perc],
            s3 = dt_tmp[size == "s3",perc])]

  dt_output[,norm_fleet := NULL]
}) %>% data.table::rbindlist()


# * CARRO - add engine size (CETESB -> DENATRAN) -----

dt_tmp <- fuel_age[veh_class  == "carro"] %>%
  data.table::dcast(.,year+veh_class+age_adj  ~ veh_type + fuel
                    ,value.var = "norm")

df_carro <- fleet_car[dt_tmp,on = c("age_adj")]


# * MOTO - add fuel composition
# (CETESB -> ABRACICLO + DENATRAN )

for(i in c("gasolina","flex")){
  df_moto[fuel_age[veh_type == "moto" & fuel == i],
          on = c("age_adj"="age_adj","classe"="veh_type")
          ,sprintf("ratio_%s",i) := i.norm]
}


# * MOTO / CARRO - add ratio gasolina / etanol -----
# (ANP -> CETESB + ABRACICLO + DENATRAN)
df_moto[1]
df_carro[1]
ratio_urban_raw[1]

df_moto[ratio_urban_raw,on = "code_urban_concentration"
        ,ratio_anp := i.ratio_gasoline]

df_moto[,ratio_flex_gasolina := ratio_flex * ratio_anp]
df_moto[,ratio_flex_etanol := ratio_flex * (1-ratio_anp)]


# add carro
df_carro[ratio_urban_raw,on = "code_urban_concentration"
        ,ratio_anp := i.ratio_gasoline]

df_carro[,lcv_flex_gasolina := lcv_flex * ratio_anp]
df_carro[,lcv_flex_etanol := lcv_flex * (1-ratio_anp)]
df_carro[,pc_flex_gasolina := pc_flex  * ratio_anp]
df_carro[,pc_flex_etanol := pc_flex  * (1-ratio_anp)]


# excluding columns, and saving -----

df_carro[,norm_fleet := NULL]
df_carro[,age := NULL]

df_moto[,age := NULL]
df_moto[is.na(df_moto)] <- 0


readr::write_rds(x = list("moto" = df_moto,"carro" = df_carro)
                 ,file = "../../data/urbanformbr/emissions/merged_fleet.rds")
