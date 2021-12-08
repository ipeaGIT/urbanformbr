

## load packages-------
rm(list=ls())
#setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
library(vein)
library(stringi) # remove accents
library(data.table)
library(stringr)
library(dplyr)
library(sf)
library(openxlsx)

## prep -----
honda_filepath = "L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo/dados/Abraciclo/honda.xlsx"
abas <- openxlsx::getSheetNames(honda_filepath)
abas

## read ----

tmp_ds  <- lapply(abas,function(i){

  message(i)
  tmp_fleet <- openxlsx::read.xlsx(honda_filepath
                                   , sheet = i
                                   , colNames = FALSE
                                   , rowNames = FALSE)
  data.table::setDT(tmp_fleet)


  onew <- which(is.na(tmp_fleet$X18) & !is.na(tmp_fleet$X17))
  twow <- which(is.na(tmp_fleet$X17))
  if(ncol(tmp_fleet) == 19){
    trew <- which(!is.na(tmp_fleet$X18) & is.na(tmp_fleet$X19))
    ind <- which(!is.na(tmp_fleet$X19))
    dt <- data.table::data.table("ano" = i,
                                 "cm3" = c(tmp_fleet$X3[onew]
                                           , tmp_fleet$X2[twow]
                                           , tmp_fleet$X4[trew]
                                           , tmp_fleet$X5[ind]),
                                 "total" = c(tmp_fleet$X16[onew]
                                             , tmp_fleet$X15[twow]
                                             , tmp_fleet$X17[trew]
                                             , tmp_fleet$X18[ind]))

  }else{
    trew <- which(!is.na(tmp_fleet$X18))
    dt <- data.table::data.table("ano" = i,
                                 "cm3" = c(tmp_fleet$X3[onew]
                                           ,tmp_fleet$X2[twow]
                                           ,tmp_fleet$X4[trew]),
                                 "total" = c(tmp_fleet$X16[onew]
                                             ,tmp_fleet$X15[twow]
                                             ,tmp_fleet$X17[trew]))
  }
  dt[,total := as.numeric(total)]
}) %>% data.table::rbindlist()

ds <- data.table::copy(tmp_ds)
# classify by size
ds[cm3 <= 150,size := "s1"]
ds[cm3 > 150 & cm3 <= 500,size := "s2"]
ds[cm3 > 500,size :="s3"]

# sum
ds[,total := as.numeric(total)]
ds <- ds[,lapply(.SD,sum),by = c("size","ano"),.SDcols = "total"]
ds[,perc := round(100 * total / sum(total),2),by = c("ano")]

ds

# write rds-----
dir.create("data/emissions/")
readr::write_rds(ds,"data/emissions/fleet_mc.rds")
