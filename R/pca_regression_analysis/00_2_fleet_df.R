#
# prep DENATRAN fleet
#
library(readr)
library(data.table)
library(geobr)
library(magrittr)

#### read ----
# read urban concentration
uca <- geobr::read_urban_concentrations() %>%
  data.table::setDT()
uca[,code_muni := as.character(code_muni)]

# read denatran fleet
den <- readr::read_rds("data-raw/DENATRAN/DENATRAN_jan.rds")

#### analysis

# add info from urban concentration to denatran
den <- den[uca,on = c("CODE"="code_muni"),
           code_urban_concentration := i.code_urban_concentration]
den <- den[!is.na(code_urban_concentration)]

# aggregate into urban concentration

den1 <- data.table::copy(den)

den2 <- den1[,`:=`(TOTAL_AUTOS = sum(TOTAL_AUTOS),
                   TOTAL_MOTOS = sum(TOTAL_MOTOS),
                   POP = sum(POP)),
             by = .(code_urban_concentration)]
den2 <- den2[,.SD[1],by = .(code_urban_concentration)]
den2[,`:=`(MOTOS_PER_POP = TOTAL_MOTOS / POP,
           AUTOS_PER_POP = TOTAL_AUTOS / POP,
           MOTO_RATE = (TOTAL_MOTOS + TOTAL_AUTOS) / POP)]
den2 <- den2[,.(UF,  ANO, TOTAL_AUTOS, TOTAL_MOTOS,
                POP, AUTOS_PER_POP, MOTOS_PER_POP,
                MOTO_RATE, code_urban_concentration)]
## save
readr::write_rds(den2,"../../data/urbanformbr/pca_regression_df/fleet.rds",
                 compress = "gz")
