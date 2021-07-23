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


#### analysis ----


# add info from urban concentration to denatran
den <- den[uca
           , on = c("CODE"="code_muni")
           , code_urban_concentration := i.code_urban_concentration]


# remove cities that has no urban concentration
# association
den <- den[!is.na(code_urban_concentration)]


# aggregate into urban concentration
#
col_vector <- c("TOTAL_AUTOS","TOTAL_MOTOS","POP")
den1 <- data.table::copy(den)
# estimate total of 'col_vector' by code_urban_concentration
den1[,(col_vector) := lapply(.SD,sum)
     , by = .(code_urban_concentration,ANO)
     , .SDcols = col_vector]


den1 <- den1[,.SD[1],by = .(code_urban_concentration,ANO)]

den1[,`:=`(MOTOS_PER_POP = TOTAL_MOTOS / POP,
           AUTOS_PER_POP = TOTAL_AUTOS / POP,
           MOTORIZATION_RATE = (TOTAL_MOTOS + TOTAL_AUTOS) / POP)]

den1 <- den1[,.(UF,  ANO, TOTAL_AUTOS, TOTAL_MOTOS,
                POP, AUTOS_PER_POP, MOTOS_PER_POP,
                MOTORIZATION_RATE, code_urban_concentration)]
## save
readr::write_rds(den1,"../../data/urbanformbr/pca_regression_df/fleet_and_pop.rds",
                 compress = "gz")
