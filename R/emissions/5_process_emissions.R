# plot emissions
# Import packages ----

rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(patchwork)

# Read data ------
message("Reading data \n")
message("---------------------")

df_moto_raw <-  readr::read_rds("../../data/urbanformbr/emissions/motocycle_emissions.rds")
df_carro_raw <- readr::read_rds("../../data/urbanformbr/emissions/passanger_cars_emissions.rds")

uca <- geobr::read_urban_concentrations(simplified = TRUE) %>%
  data.table::setDT() %>%
  .[,code_muni := as.character(code_muni)]

geobr_regions <- geobr::read_state() %>% data.table::setDT()

df_energy <- readr::read_rds("../../data/urbanformbr/pca_regression_df/energy_per_capita_2010.rds")

df_pop <- readr::read_rds("../../data/urbanformbr/pca_regression_df/fleet_and_pop.rds")
df_pop <- df_pop[ANO == 2010,.SD,.SDcols = c("code_urban_concentration","POP")]
data.table::setnames(df_pop,"POP","pop")


# Prep -----
# * Moto -----
df_moto_prep <- data.table::copy(df_moto_raw)
colPols <- names(df_moto_prep)[names(df_moto_prep) %like% "^CO2"]
df_moto_prep <- df_moto_prep[,.SD
                             ,.SDcols = c("code_urban_concentration"
                                          ,"classe","age_adj",colPols)]

df_moto_prep <- melt(df_moto_prep, measure= colPols
     ,value.name = "Emissions",variable.name = "type_vehicle")
df_moto_prep[,type_vehicle := gsub("CO2_","",type_vehicle)]
# * Carro -----
df_carro_prep <- data.table::copy(df_carro_raw)
colPols <- names(df_carro_prep)[names(df_carro_prep) %like% "^CO2"]
df_carro_prep <- df_carro_prep[,.SD
                             ,.SDcols = c("code_urban_concentration"
                                          ,"classe","age_adj",colPols)]
df_carro_prep <- melt(df_carro_prep, measure= colPols
                     ,value.name = "Emissions",variable.name = "type_vehicle")
df_carro_prep[,type_vehicle := gsub("CO2_","",type_vehicle)]


# * Bind -----

dt_bind <- rbind(df_carro_prep,df_moto_prep)


# plot 1 -----

df_plot1 <- data.table::copy(dt_bind)
df_plot1[1]
df_plot1 <- df_plot1[,sum(Emissions),by = .(age_adj,classe,type_vehicle)]
df_plot1[,V1 := units::set_units(V1,"t")]
df_plot1[,V1 := as.numeric(V1)]

df_plot1[classe == "moto",classe_name := "Motocicleta"]
df_plot1[classe == "carro",classe_name := "Automóvel"]
df_plot1$type_vehicle %>% unique()
df_plot1[type_vehicle == "LCV_D",name_veh := "VCL (Diesel)"]
df_plot1[type_vehicle == "LCV_E",name_veh := "VCL (Etanol)"]
df_plot1[type_vehicle == "LCV_FE",name_veh := "VCL (Flex-Etanol)"]
df_plot1[type_vehicle == "LCV_FG",name_veh := "VCL (Flex-Gasolina)"]
df_plot1[type_vehicle == "LCV_G",name_veh := "VCL (Gasolina)"]
df_plot1[type_vehicle == "PC_G",name_veh := "VLP (Gasolina)"]
df_plot1[type_vehicle == "PC_FG",name_veh := "VLP (Flex-Gasolina)"]
df_plot1[type_vehicle == "PC_FE",name_veh := "VLP (Flex-Etanol)"]
df_plot1[type_vehicle == "PC_E",name_veh := "VLP (Etanol)"]

df_plot1[type_vehicle == "MC_150_G"     ,name_veh := "Baixa cc (Gasolina)"]
df_plot1[type_vehicle == "MC_150_500_G" ,name_veh := "Média cc (Gasolina)"]
df_plot1[type_vehicle == "MC_500_G"     ,name_veh := "Alta cc (Gasolina)"]
df_plot1[type_vehicle == "MC_150_FG"    ,name_veh := "Baixa cc (Flex-Gasolina)"]
df_plot1[type_vehicle == "MC_150_500_FG",name_veh := "Média cc (Flex-Gasolina)"]
df_plot1[type_vehicle == "MC_500_FG"    ,name_veh := "Alta cc (Gasolina)"]
df_plot1[type_vehicle == "MC_150_FE"    ,name_veh := "Baixa cc (Flex-Etanol)"]
df_plot1[type_vehicle == "MC_150_500_FE",name_veh := "Média cc (Flex-Etanol)"]
df_plot1[type_vehicle == "MC_500_FE"    ,name_veh := "Alta cc (Flex-Etanol)"]

p1 <- ggplot() +
  geom_area(data = df_plot1[classe == "carro"]
            ,aes(x= age_adj,y= V1,fill = name_veh))+
  scale_fill_viridis_d()+
  labs(title = "Emissões de CO2 nas áreas de concentração \nurbana conforme tipo de veículo, para ano base de 2010"
       ,x = "Fleet age",y = "Emissions (t of CO2)",fill = "Categoria"
       ,caption = "VCL: Veículos Comercial Level \n VLP: Veículo Leve de Passageiros")+
  facet_wrap(~classe_name,scales = "free_x")

p2 <- ggplot() +
  geom_area(data = df_plot1[classe == "moto"],aes(x= age_adj,y= V1,fill = name_veh))+
  scale_fill_viridis_d()+
  labs(x = "Fleet age",y = "Emissions (t of CO2)",fill = "Categoria")+
  facet_wrap(~classe_name,scales = "free_x")

p1 / p2

p3 <- ggplot() +
  geom_area(data = df_plot1,aes(x= age_adj,y= V1,fill = name_veh))+
  scale_fill_viridis_d()+
  labs(x = "Fleet age",y = "Emissions (t of CO2)",fill = "Categoria")+
  facet_wrap(~classe_name,scales = "free_x",nrow = 2)
p3


# plot 2 -----
dt_bind[1]
dt_pol <- data.table::copy(dt_bind)[,sum(Emissions),by = .(code_urban_concentration)]
data.table::setnames(dt_pol,"V1","Emissions")
dt_pol[,Emissions := units::set_units(Emissions,"t")]

dt_pol[df_pop,on = "code_urban_concentration",pop := i.pop]
dt_pol[df_energy,on = "code_urban_concentration",tep := i.tep]
dt_pol[,Emissions := as.numeric(Emissions) / pop]

dt_pol[1]

ggplot()+
  geom_point(data = dt_pol,aes(x = tep,y = Emissions,color = pop))+
  labs(x = "energia per capita",y = "emissoes per capita")+
  scale_color_viridis_c()


cor.test(dt_pol$Emissions,dt_pol$tep)

# plot 3----
df_plot3 <- data.table::copy(dt_bind)
df_plot3[1]
df_plot3 <- df_plot3[,sum(Emissions),by = .(age_adj,code_urban_concentration,classe)]
df_plot3[,V1 := units::set_units(V1,"t")]
df_plot3[,V1 := as.numeric(V1)]

df_plot3[classe == "moto",classe_name := "Motocicleta"]
df_plot3[classe == "carro",classe_name := "Automóvel"]

df_plot3[uca,on = "code_urban_concentration",uf := i.abbrev_state]
df_plot3[geobr_regions,on = c("uf" = "abbrev_state"),region_br := i.name_region]
df_plot3[df_pop,on = c("code_urban_concentration"),pop := i.pop]

df_plot3 <- df_plot3[,lapply(.SD,sum),by = .(age_adj,region_br,classe)
                     ,.SDcols = c("V1","pop")]
df_plot3[,emi_pop := V1/pop]

ggplot() +
  geom_line(data = df_plot3
            ,aes(x= age_adj,y= emi_pop
                 ,color = region_br),lwd = 1)+
  scale_color_viridis_d()+
  labs(x = "Idade da frota"
       ,y = "Co2 per capita (t/pessoa)"
       ,color = "Região")+
  facet_wrap(~classe)
