# Import packages ----

rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(tabulizer)
library(Hmisc)
library(vein)


# Read data ------
message("Reading data \n")
message("---------------------")


list_df <- readr::read_rds("../../data/urbanformbr/emissions/merged_fleet.rds")
anp_data_raw <- readr::read_rds("../../data/urbanformbr/fuel/fuel_urban_areas.rds")
anp_data_raw
data(fkm)




# FC -----

# * MOTO ----
message("Aggregating moto EF \n")
message("---------------------")
# * * Add fkm | co2 ----
df_co2_moto_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_MOTO_E25(age = 1:25),"km")
                                          ,"age" = 1:25
                                          , "MC_150_G" = vein::ef_cetesb(p = "CO2",veh = "MC_150_G",year = 2010,agemax = 25)
                                          , "MC_150_500_G" = vein::ef_cetesb(p = "CO2",veh = "MC_150_500_G",year = 2010,agemax = 25)
                                          , "MC_500_G" = vein::ef_cetesb(p = "CO2",veh = "MC_500_G",year = 2010,agemax = 25)
                                          , "MC_150_FG" = vein::ef_cetesb(p = "CO2",veh = "MC_150_FG",year = 2010,agemax = 25)
                                          , "MC_150_500_FG" = vein::ef_cetesb(p = "CO2",veh = "MC_150_500_FG",year = 2010,agemax = 25)
                                          , "MC_500_FG" = vein::ef_cetesb(p = "CO2",veh = "MC_500_FG",year = 2010,agemax = 25)
                                          , "MC_150_FE" = vein::ef_cetesb(p = "CO2",veh = "MC_150_FE",year = 2010,agemax = 25)
                                          , "MC_150_500_FE" = vein::ef_cetesb(p = "CO2",veh = "MC_150_500_FE",year = 2010,agemax = 25)
                                          , "MC_500_FE" = vein::ef_cetesb(p = "CO2",veh = "MC_500_FE",year = 2010,agemax = 25)
                                          , "pol" = "CO2")
df_fc_moto_fkm <- data.table::data.table("fkm" = units::set_units(fkm$KM_MOTO_E25(age = 1:25),"km")
                                         ,"age" = 1:25
                                         , "MC_150_G" = vein::ef_cetesb(p = "FC",veh = "MC_150_G",year = 2010,agemax = 25)
                                         , "MC_150_500_G" = vein::ef_cetesb(p = "FC",veh = "MC_150_500_G",year = 2010,agemax = 25)
                                         , "MC_500_G" = vein::ef_cetesb(p = "FC",veh = "MC_500_G",year = 2010,agemax = 25)
                                         , "MC_150_FG" = vein::ef_cetesb(p = "FC",veh = "MC_150_FG",year = 2010,agemax = 25)
                                         , "MC_150_500_FG" = vein::ef_cetesb(p = "FC",veh = "MC_150_500_FG",year = 2010,agemax = 25)
                                         , "MC_500_FG" = vein::ef_cetesb(p = "FC",veh = "MC_500_FG",year = 2010,agemax = 25)
                                         , "MC_150_FE" = vein::ef_cetesb(p = "FC",veh = "MC_150_FE",year = 2010,agemax = 25)
                                         , "MC_150_500_FE" = vein::ef_cetesb(p = "FC",veh = "MC_150_500_FE",year = 2010,agemax = 25)
                                         , "MC_500_FE" = vein::ef_cetesb(p = "FC",veh = "MC_500_FE",year = 2010,agemax = 25)
                                         , "pol" = "FC")

df_fkm <- rbind(df_co2_moto_fkm,df_fc_moto_fkm)

# merge
df_moto <- list_df$moto
df_moto[1]
df_fkm[1]

# add EF
df_moto[df_fc_moto_fkm,on = c("age_adj" = "age"),
        `:=`(fkm = i.fkm
             , EF_FC_MC_150_G = i.MC_150_G
             , EF_FC_MC_150_500_G = i.MC_150_500_G
             , EF_FC_MC_500_G = i.MC_500_G
             , EF_FC_MC_150_FG = i.MC_150_FG
             , EF_FC_MC_150_500_FG = i.MC_150_500_FG
             , EF_FC_MC_500_FG = i.MC_500_FG
             , EF_FC_MC_150_FE = i.MC_150_FE
             , EF_FC_MC_150_500_FE = i.MC_150_500_FE
             , EF_FC_MC_500_FE = i.MC_500_FE)]

df_moto[df_co2_moto_fkm,on = c("age_adj" = "age"),
        `:=`(fkm = i.fkm
             , EF_CO2_MC_150_G = i.MC_150_G
             , EF_CO2_MC_150_500_G = i.MC_150_500_G
             , EF_CO2_MC_500_G = i.MC_500_G
             , EF_CO2_MC_150_FG = i.MC_150_FG
             , EF_CO2_MC_150_500_FG = i.MC_150_500_FG
             , EF_CO2_MC_500_FG = i.MC_500_FG
             , EF_CO2_MC_150_FE = i.MC_150_FE
             , EF_CO2_MC_150_500_FE = i.MC_150_500_FE
             , EF_CO2_MC_500_FE = i.MC_500_FE)]
# * * Estimate FC -----
message("Estimating FC - Moto \n")
message("---------------------")
df_moto[,`:=`(FC_MC_150_G       = fkm * total * (s1/100) * ratio_gasolina  * EF_FC_MC_150_G
              , FC_MC_150_500_G  = fkm * total * (s2/100) * ratio_gasolina  * EF_FC_MC_150_500_G
              , FC_MC_500_G      = fkm * total * (s3/100) * ratio_gasolina  * EF_FC_MC_500_G
              , FC_MC_150_FG     = fkm * total * (s1/100) * ratio_flex * ratio_flex_gasolina * EF_FC_MC_150_FG
              , FC_MC_150_500_FG = fkm * total * (s2/100) * ratio_flex * ratio_flex_gasolina * EF_FC_MC_150_500_FG
              , FC_MC_500_FG     = fkm * total * (s3/100) * ratio_flex * ratio_flex_gasolina * EF_FC_MC_500_FG
              , FC_MC_150_FE     = fkm * total * (s1/100) * ratio_flex * ratio_flex_etanol * EF_FC_MC_150_FE
              , FC_MC_150_500_FE = fkm * total * (s2/100) * ratio_flex * ratio_flex_etanol * EF_FC_MC_150_500_FE
              , FC_MC_500_FE     = fkm * total * (s3/100) * ratio_flex * ratio_flex_etanol * EF_FC_MC_500_FE)]

# CARRO ----
message("Aggregating carro EF \n")
message("---------------------")
# * * Add fkm | co2 ----
df_fc_pc_e25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_E25(age = 1:31),"km")
                                           ,"age" = 1:31
                                           , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_G",year = 2010,agemax = 31)
                                           , "pol" = "FC"
                                           , "veh" = "PC_G")
df_fc_pc_fe25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_FLEX(age = 1:31),"km")
                                            ,"age" = 1:31
                                            , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_FG",year = 2010,agemax = 31)
                                            , "pol" = "FC"
                                            , "veh" = "PC_FG")
df_fc_pc_fe100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_FLEX(age = 1:31),"km")
                                             ,"age" = 1:31
                                             , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_FE",year = 2010,agemax = 31)
                                             , "pol" = "FC"
                                             , "veh" = "PC_FE")
df_fc_pc_e100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_E100(age = 1:31),"km")
                                            ,"age" = 1:31
                                            , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_E",year = 2010,agemax = 31)
                                            , "pol" = "FC"
                                            , "veh" = "PC_E")
df_fc_ldv_e25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_E25(age = 1:31),"km")
                                            ,"age" = 1:31
                                            , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_G",year = 2010,agemax = 31)
                                            , "pol" = "FC"
                                            , "veh" = "LCV_G")
df_fc_ldv_fe25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_FLEX(age = 1:31),"km")
                                             ,"age" = 1:31
                                             , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_FG",year = 2010,agemax = 31)
                                             , "pol" = "FC"
                                             , "veh" = "LCV_FG")
df_fc_ldv_e100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_E25(age = 1:31),"km")
                                             ,"age" = 1:31
                                             , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_E",year = 2010,agemax = 31)
                                             , "pol" = "FC"
                                             , "veh" = "LCV_E")
df_fc_ldv_fe100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_FLEX(age = 1:31),"km")
                                              ,"age" = 1:31
                                              , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_FE",year = 2010,agemax = 31)
                                              , "pol" = "FC"
                                              , "veh" = "LCV_FE")
df_fc_ldv_d_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_B5(age = 1:31),"km")
                                          ,"age" = 1:31
                                          , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_D",year = 2010,agemax = 31)
                                          , "pol" = "FC"
                                          , "veh" = "LCV_D")

#  co2

df_co2_pc_e25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_E25(age = 1:31),"km")
                                            ,"age" = 1:31
                                            , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_G",year = 2010,agemax = 31)
                                            , "pol" = "CO2"
                                            , "veh" = "PC_G")
df_co2_pc_fe25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_FLEX(age = 1:31),"km")
                                             ,"age" = 1:31
                                             , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_FG",year = 2010,agemax = 31)
                                             , "pol" = "CO2"
                                             , "veh" = "PC_FG")
df_co2_pc_fe100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_FLEX(age = 1:31),"km")
                                              ,"age" = 1:31
                                              , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_FE",year = 2010,agemax = 31)
                                              , "pol" = "CO2"
                                              , "veh" = "PC_FE")
df_co2_pc_e100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_PC_E100(age = 1:31),"km")
                                             ,"age" = 1:31
                                             , "EF" = vein::ef_cetesb(p = "FC",veh = "PC_E",year = 2010,agemax = 31)
                                             , "pol" = "CO2"
                                             , "veh" = "PC_E")
df_co2_ldv_e25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_E25(age = 1:31),"km")
                                             ,"age" = 1:31
                                             , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_G",year = 2010,agemax = 31)
                                             , "pol" = "CO2"
                                             , "veh" = "LCV_G")
df_co2_ldv_fe25_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_FLEX(age = 1:31),"km")
                                              ,"age" = 1:31
                                              , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_FG",year = 2010,agemax = 31)
                                              , "pol" = "CO2"
                                              , "veh" = "LCV_FG")
df_co2_ldv_e100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_E25(age = 1:31),"km")
                                              ,"age" = 1:31
                                              , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_E",year = 2010,agemax = 31)
                                              , "pol" = "CO2"
                                              , "veh" = "LCV_E")
df_co2_ldv_fe100_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_FLEX(age = 1:31),"km")
                                               ,"age" = 1:31
                                               , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_FE",year = 2010,agemax = 31)
                                               , "pol" = "CO2"
                                               , "veh" = "LCV_FE")
df_co2_ldv_d_fkm <- data.table::data.table("fkm" =  units::set_units(fkm$KM_LCV_B5(age = 1:31),"km")
                                           ,"age" = 1:31
                                           , "EF" = vein::ef_cetesb(p = "FC",veh = "LCV_D",year = 2010,agemax = 31)
                                           , "pol" = "CO2"
                                           , "veh" = "LCV_D")

# Bind DF
df_fc_carro_fkm <- list(df_fc_pc_fe25_fkm  # fc
                        ,df_fc_pc_e25_fkm
                        ,df_fc_pc_e100_fkm
                        ,df_fc_pc_fe100_fkm
                        ,df_fc_ldv_d_fkm
                        ,df_fc_ldv_e100_fkm
                        ,df_fc_ldv_e25_fkm
                        ,df_fc_ldv_fe25_fkm
                        ,df_fc_ldv_fe100_fkm
                        ,df_co2_pc_fe25_fkm # co2
                        ,df_co2_pc_e25_fkm
                        ,df_co2_pc_e100_fkm
                        ,df_co2_pc_fe100_fkm
                        ,df_co2_ldv_d_fkm
                        ,df_co2_ldv_e100_fkm
                        ,df_co2_ldv_e25_fkm
                        ,df_co2_ldv_fe25_fkm
                        ,df_co2_ldv_fe100_fkm) %>% data.table::rbindlist()

d_fc_ef <- dcast(data = df_fc_carro_fkm[pol == "FC"]
              ,formula = age + pol ~ paste0("EF_FC_",veh)
              ,value.var = c("EF"))
d_co2_ef <- dcast(data = df_fc_carro_fkm[pol == "CO2"]
              ,formula = age + pol ~ paste0("EF_CO2_",veh)
              ,value.var = c("EF"))
d_fkm <- dcast(data = df_fc_carro_fkm[pol == "FC"]
               ,formula = age + pol ~ paste0("fkm_",veh)
               ,value.var = c("fkm"))

df_fc_carro_fkm <- do.call(what = cbind,list(d_fc_ef,d_co2_ef[,-c(1:2)],d_fkm[,-c(1:2)]))


rm(list=c("df_fc_pc_fe25_fkm","df_fc_pc_e25_fkm","df_fc_pc_e100_fkm"
          ,"df_fc_pc_fe100_fkm","df_fc_ldv_d_fkm","df_fc_ldv_e100_fkm"
          ,"df_fc_ldv_e25_fkm","df_fc_ldv_fe25_fkm","df_fc_ldv_fe100_fkm"
          ,"df_co2_pc_fe25_fkm","df_co2_pc_e25_fkm","df_co2_pc_e100_fkm"
          ,"df_co2_pc_fe100_fkm","df_co2_ldv_d_fkm","df_co2_ldv_e100_fkm"
          ,"df_co2_ldv_e25_fkm","df_co2_ldv_fe25_fkm","df_co2_ldv_fe100_fkm"))


df_carro <- list_df$carro
df_carro[anp_data_raw,on = "code_urban_concentration",ratio_gasoline_anp := i.ratio_gasoline]

# * * fix cities that has no etanol use -----
df_carro[ratio_gasoline_anp == 1,pc_gasolina := pc_gasolina + pc_etanol]
df_carro[ratio_gasoline_anp == 1,lcv_gasolina := lcv_gasolina + lcv_etanol]
df_carro[ratio_gasoline_anp == 1,pc_etanol := 0]
df_carro[ratio_gasoline_anp == 1,lcv_etanol := 0]


df_carro <- df_carro[df_fc_carro_fkm,on = c("age_adj" = "age")]
df_carro[,`:=`(pol = NULL)]


# check
# df_carro[,teste := lcv_diesel + lcv_etanol + lcv_flex_gasolina + lcv_flex_etanol + lcv_gasolina +
#           pc_etanol + pc_flex_gasolina + pc_flex_etanol + pc_gasolina]
# summary(df_carro$teste)
# df_carro[,teste := NULL]
names(df_carro)

# * * Estimate FC -----
message("Estimating FC - Carro \n")
message("---------------------")

df_carro[,`:=`( FC_LCV_D  = fkm_LCV_D  * total * lcv_diesel        * EF_FC_LCV_D
               ,FC_LCV_E  = fkm_LCV_E  * total * lcv_etanol        * EF_FC_LCV_E
               ,FC_LCV_FE = fkm_LCV_FE * total * lcv_flex_etanol   * EF_FC_LCV_FE
               ,FC_LCV_FG = fkm_LCV_FG * total * lcv_flex_gasolina * EF_FC_LCV_FG
               ,FC_LCV_G  = fkm_LCV_G  * total * lcv_gasolina      * EF_FC_LCV_G
               ,FC_PC_G   = fkm_PC_G   * total * pc_gasolina       * EF_FC_PC_G
               ,FC_PC_FG  = fkm_PC_FG  * total * pc_flex_gasolina  * EF_FC_PC_FG
               ,FC_PC_FE  = fkm_PC_FE  * total * pc_flex_etanol    * EF_FC_PC_FE
               ,FC_PC_E   = fkm_PC_E   * total * pc_etanol         * EF_FC_PC_E
)]


# Adjustment factor ------------------
message("Comparing estimated fuel with ANP observed data \n")
message("--------------------------------------------------")


names_car_g <- c("FC_LCV_FG","FC_LCV_G","FC_PC_G","FC_PC_FG")
names_car_e <- c("FC_LCV_E","FC_LCV_FE","FC_PC_FE","FC_PC_E")
names_moto_g <- c("FC_MC_150_G","FC_MC_150_500_G","FC_MC_500_G","FC_MC_150_FG"
                  ,"FC_MC_150_500_FG","FC_MC_500_FG")
names_moto_e <- c("FC_MC_150_FE","FC_MC_150_500_FE"
                  ,"FC_MC_500_FE")

total_car  <- data.table::copy(df_carro)[,lapply(.SD,sum),.SDcols = c(names_car_g,names_car_e), by = code_urban_concentration]
total_moto <- data.table::copy(df_moto)[,lapply(.SD,sum),.SDcols = c(names_moto_g,names_moto_e), by = code_urban_concentration]

merged_df <- total_moto[total_car, on = "code_urban_concentration"]

merged_df[,etanol_total := FC_LCV_E + FC_LCV_FE + FC_PC_FE + FC_PC_E +
            FC_MC_150_FE + FC_MC_150_500_FE + FC_MC_500_FE]
merged_df[,gasolina_total := FC_LCV_FG + FC_LCV_G + FC_PC_G + FC_PC_FG +
            FC_MC_150_G + FC_MC_150_500_G + FC_MC_500_G + FC_MC_150_FG +
            FC_MC_150_500_FG + FC_MC_500_FG]

# * total anp ----

p_etanol <- units::set_units(809,"kg/m^3")
p_gasolina <- units::set_units(742,"kg/m^3")

anp_data_raw[,etanol_peso := etanol * p_etanol]
anp_data_raw[,gasolina_peso := gasolina_c * p_gasolina]


merged_df[anp_data_raw,on = "code_urban_concentration"
          ,`:=`(etanol_anp = i.etanol_peso,
                gasolina_anp = i.gasolina_peso)]

merged_df[,adj_f_g := gasolina_anp / gasolina_total]
merged_df[,adj_f_e := etanol_anp / etanol_total]

summary(merged_df$adj_f_e)
summary(merged_df$adj_f_g)

merged_df[is.na(adj_f_e), adj_f_e := 0]

# Check fuel consumption ------


merged_df[1]
df_carro[1]

# * add adj factor  ------
df_carro[merged_df, on = "code_urban_concentration"
         ,`:=`(adj_f_g = i.adj_f_g
               ,adj_f_e = i.adj_f_e)]

df_moto[merged_df, on = "code_urban_concentration"
         ,`:=`(adj_f_g = i.adj_f_g
               ,adj_f_e = i.adj_f_e)]

df_carro[,`:=`(  adj_FC_LCV_D  = 1       * fkm_LCV_D  * total * lcv_diesel        * EF_FC_LCV_D
                ,adj_FC_LCV_E  = adj_f_e * fkm_LCV_E  * total * lcv_etanol        * EF_FC_LCV_E
                ,adj_FC_LCV_FE = adj_f_e * fkm_LCV_FE * total * lcv_flex_etanol   * EF_FC_LCV_FE
                ,adj_FC_LCV_FG = adj_f_g * fkm_LCV_FG * total * lcv_flex_gasolina * EF_FC_LCV_FG
                ,adj_FC_LCV_G  = adj_f_g * fkm_LCV_G  * total * lcv_gasolina      * EF_FC_LCV_G
                ,adj_FC_PC_G   = adj_f_g * fkm_PC_G   * total * pc_gasolina       * EF_FC_PC_G
                ,adj_FC_PC_FG  = adj_f_g * fkm_PC_FG  * total * pc_flex_gasolina  * EF_FC_PC_FG
                ,adj_FC_PC_FE  = adj_f_e * fkm_PC_FE  * total * pc_flex_etanol    * EF_FC_PC_FE
                ,adj_FC_PC_E   = adj_f_e * fkm_PC_E   * total * pc_etanol         * EF_FC_PC_E
)]

df_moto[,`:=`(  adj_FC_MC_150_G      = adj_f_g * fkm * total * (s1/100) * ratio_gasolina  * EF_FC_MC_150_G
              , adj_FC_MC_150_500_G  = adj_f_g * fkm * total * (s2/100) * ratio_gasolina  * EF_FC_MC_150_500_G
              , adj_FC_MC_500_G      = adj_f_g * fkm * total * (s3/100) * ratio_gasolina  * EF_FC_MC_500_G
              , adj_FC_MC_150_FG     = adj_f_g * fkm * total * (s1/100) * ratio_flex * ratio_flex_gasolina * EF_FC_MC_150_FG
              , adj_FC_MC_150_500_FG = adj_f_g * fkm * total * (s2/100) * ratio_flex * ratio_flex_gasolina * EF_FC_MC_150_500_FG
              , adj_FC_MC_500_FG     = adj_f_g * fkm * total * (s3/100) * ratio_flex * ratio_flex_gasolina * EF_FC_MC_500_FG
              , adj_FC_MC_150_FE     = adj_f_e * fkm * total * (s1/100) * ratio_flex * ratio_flex_etanol * EF_FC_MC_150_FE
              , adj_FC_MC_150_500_FE = adj_f_e * fkm * total * (s2/100) * ratio_flex * ratio_flex_etanol * EF_FC_MC_150_500_FE
              , adj_FC_MC_500_FE     = adj_f_e * fkm * total * (s3/100) * ratio_flex * ratio_flex_etanol * EF_FC_MC_500_FE)]

# * checking fuel comsumption -----
# * * gasoline -----
names_adj_fc_g_moto <- c("adj_FC_MC_150_G","adj_FC_MC_150_500_G","adj_FC_MC_500_G"
                    ,"adj_FC_MC_150_FG","adj_FC_MC_150_500_FG","adj_FC_MC_500_FG")
names_adj_fc_g_carro <- c("adj_FC_LCV_FG","adj_FC_LCV_G","adj_FC_PC_G","adj_FC_PC_FG")

total_carro_g <- df_carro[,sum(.SD),.SDcols = names_adj_fc_g_carro]
total_moto_g <- df_moto[,sum(.SD),.SDcols = names_adj_fc_g_moto]

message("ANP gasoline comsumption: \n")
anp_gasolina <- anp_data_raw$gasolina_peso %>% units::set_units("g") %>% sum()
anp_gasolina
message("Estimated gasoline consumption:\n")
sum(total_carro_g) + sum(total_moto_g)
# * * etanol -----
names_adj_fc_e_moto <- c("adj_FC_MC_150_FE","adj_FC_MC_150_500_FE","adj_FC_MC_500_FE")
names_adj_fc_e_carro <- c("adj_FC_LCV_FE","adj_FC_LCV_E","adj_FC_PC_E","adj_FC_PC_FE")

total_carro_e <- df_carro[,sum(.SD),.SDcols = names_adj_fc_e_carro]
total_moto_e <- df_moto[,sum(.SD),.SDcols = names_adj_fc_e_moto]

message("ANP etanol comsumption: \n")
anp_etanol <- anp_data_raw$etanol_peso %>% units::set_units("g") %>% sum()
anp_etanol
message("Estimated etanol consumption:\n")
sum(total_carro_e) + sum(total_moto_e)

# Estimate CO2

df_carro[,`:=`(  CO2_LCV_D  = adj_f_g * fkm_LCV_D  * total * lcv_diesel        * EF_CO2_LCV_D
               , CO2_LCV_E  = adj_f_e * fkm_LCV_E  * total * lcv_etanol        * EF_CO2_LCV_E
               , CO2_LCV_FE = adj_f_e * fkm_LCV_FE * total * lcv_flex_etanol   * EF_CO2_LCV_FE
               , CO2_LCV_FG = adj_f_g * fkm_LCV_FG * total * lcv_flex_gasolina * EF_CO2_LCV_FG
               , CO2_LCV_G  = adj_f_g * fkm_LCV_G  * total * lcv_gasolina      * EF_CO2_LCV_G
               , CO2_PC_G   = adj_f_g * fkm_PC_G   * total * pc_gasolina       * EF_CO2_PC_G
               , CO2_PC_FG  = adj_f_g * fkm_PC_FG  * total * pc_flex_gasolina  * EF_CO2_PC_FG
               , CO2_PC_FE  = adj_f_e * fkm_PC_FE  * total * pc_flex_etanol    * EF_CO2_PC_FE
               , CO2_PC_E   = adj_f_e * fkm_PC_E   * total * pc_etanol         * EF_CO2_PC_E
)]

df_moto[,`:=`(  CO2_MC_150_G      = adj_f_g * fkm * total * (s1/100) * ratio_gasolina  * EF_CO2_MC_150_G
              , CO2_MC_150_500_G  = adj_f_g * fkm * total * (s2/100) * ratio_gasolina  * EF_CO2_MC_150_500_G
              , CO2_MC_500_G      = adj_f_g * fkm * total * (s3/100) * ratio_gasolina  * EF_CO2_MC_500_G
              , CO2_MC_150_FG     = adj_f_g * fkm * total * (s1/100) * ratio_flex      * ratio_flex_gasolina * EF_CO2_MC_150_FG
              , CO2_MC_150_500_FG = adj_f_g * fkm * total * (s2/100) * ratio_flex      * ratio_flex_gasolina * EF_CO2_MC_150_500_FG
              , CO2_MC_500_FG     = adj_f_g * fkm * total * (s3/100) * ratio_flex      * ratio_flex_gasolina * EF_CO2_MC_500_FG
              , CO2_MC_150_FE     = adj_f_e * fkm * total * (s1/100) * ratio_flex      * ratio_flex_etanol   * EF_CO2_MC_150_FE
              , CO2_MC_150_500_FE = adj_f_e * fkm * total * (s2/100) * ratio_flex      * ratio_flex_etanol   * EF_CO2_MC_150_500_FE
              , CO2_MC_500_FE     = adj_f_e * fkm * total * (s3/100) * ratio_flex      * ratio_flex_etanol   * EF_CO2_MC_500_FE)]

# Save ------

readr::write_rds(x = df_moto,"../../data/urbanformbr/emissions/motocycle_emissions.rds")
readr::write_rds(x = df_carro,"../../data/urbanformbr/emissions/passanger_cars_emissions.rds")
