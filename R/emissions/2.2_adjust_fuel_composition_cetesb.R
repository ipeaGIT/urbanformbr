
# Import packages ----

rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(tabulizer)
library(Hmisc)

#' data source: CETESB
#'
#' @ https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/

source_path <- "data-raw/emissions/Frota-Circulante-Estado-de-Sao-Paulo-2020.xlsx"

openxlsx::getSheetNames(source_path)

# Read pc ----

names_fuel <- c("Automóvel_gasolina","Automóvel_etanol","Automóvel_flex"
                ,"Coml Leve_gasolina","Coml Leve_etanol","Coml Leve_flex",
                "Coml Leve_diesel","Moto_gasolina","Moto_flex")


fuel_dt <- lapply(names_fuel,function(i){ # i = names_fuel[2]

  message(i)

  tmp_fuel <- openxlsx::read.xlsx(source_path
                                  ,sheet = i) %>%
    data.table::setDT() %>%
    .[Municipio == "Total",]


  tmp_fuel <- data.table::melt(data = tmp_fuel
                   ,id_vars = names(tmp_fuel)[-1]
                   ,variable.name = "year"
                   ,value.name = "consumo")
  tmp_fuel[,name_fuel := i]

  # fill 'NA' values
  tmp_fuel[is.na(tmp_fuel)] <- 0

  return(tmp_fuel)
}) %>% data.table::rbindlist()


# Adjust data ----
# * remove year == total, years > 2010  ----

fuel_dt1 <- data.table::copy(fuel_dt) %>%
  .[year != "TOTAL",] %>%
  .[,year := as.character(year)] %>%
  .[,year := as.numeric(year)] %>%
  .[year <= 2010,]

# * new configuration  ----

fuel_dt1 <- fuel_dt1 %>%
  .[name_fuel %like% "Automóvel",veh_type := "pc"] %>%
  .[name_fuel %like% "Coml Leve",veh_type := "lcv"] %>%
  .[name_fuel %like% "Automóvel",veh_class := "carro"] %>%
  .[name_fuel %like% "Coml Leve",veh_class := "carro"] %>%
  .[name_fuel %like% "Moto",veh_type := "moto"] %>%
  .[name_fuel %like% "Moto",veh_class := "moto"] %>%
  .[name_fuel %like% "_gasolina",fuel := "gasolina"] %>%
  .[name_fuel %like% "_etanol",fuel := "etanol"] %>%
  .[name_fuel %like% "_flex",fuel := "flex"] %>%
  .[name_fuel %like% "_diesel",fuel := "diesel"] %>%
  .[,name_fuel := NULL]

# * norm to 1  ----

fuel_dt1[,norm := consumo / sum(consumo),by = .(year,veh_class)]
fuel_dt1[is.nan(norm),norm := 0]

fuel_dt1[,sum(norm),by = .(year,veh_class)]

# * plot to check  ----

ggplot() +
  geom_area(data = fuel_dt1,aes(x = year,y = norm,fill = fuel))+
  facet_wrap(~veh_type)

# Save ----

readr::write_rds(fuel_dt1,"data/emissions/fuel_by_veh-age.R")



