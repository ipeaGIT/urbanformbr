# 1) load packages--------
rm(list=ls())
library(magrittr)
library(data.table)
library(geobr)

# 2) read files-----------

uca <- geobr::read_urban_concentrations(simplified = TRUE) %>%
  data.table::setDT() %>%
  .[,code_muni := as.character(code_muni)]

br <- geobr::read_municipality() %>%
  data.table::setDT() %>%
  .[,code_muni := as.character(code_muni)]

anp <- readr::read_rds("data/fuel_cities.rds") %>%
  .[fuel != "diesel",] %>%
  .[year == 2010,] %>%
  .[,cod_ibge := as.character(cod_ibge)]


# 3) processing -------

anp2 <- data.table::dcast(data = anp
                          , formula =  cod_ibge + municipios ~ fuel
                          , value.var = "volume")

# TEP ---------
p_etanol <- units::set_units(809,"kg/m^3")
p_gasolina <- units::set_units(742,"kg/m^3")
pci_etanol <- units::set_units(6300,"kcal/kg")
pci_gasolina <- units::set_units(10400,"kcal/kg")

anp2[,gasolina := as.numeric(gasolina)]
anp2[,etanol := as.numeric(etanol)]

anp2[,gasolina := units::set_units(gasolina,"m^3")]
anp2[,etanol := units::set_units(etanol,"m^3")]

anp2[,tep := etanol * p_etanol * pci_etanol +
       gasolina * p_gasolina * pci_gasolina]

units::install_unit(symbol = "toe"
                    , def = "41 868 GJ"
                    , name = "Tonne of Oil Equivalent")

anp2[,tep := units::set_units(tep,"toe")]


# add uca info
anp2[uca, on = c("cod_ibge" = "code_muni")
     , code_urban_concentration := i.code_urban_concentration]

anp2 <- anp2[!is.na(code_urban_concentration),]


# sum values
varCols <- c("etanol","gasolina","tep")
anp3 <- data.table::copy(anp2)[,
                               lapply(.SD,sum,na.rm = TRUE)
                               , .SDcols = varCols
                               , by = code_urban_concentration]


anp3[uca, on = "code_urban_concentration"
     , name_urban_concentration := i.name_urban_concentration]

anp3[order(tep,decreasing = TRUE)]


# 4) save-----
readr::write_rds(anp3,"data/tep_energy_2010.rds")
