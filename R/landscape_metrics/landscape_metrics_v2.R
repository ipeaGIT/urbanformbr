library(raster)
library(landscapemetrics)
library(tidyverse)
library(sf)

data_folder <- "../../data/urbanformbr/ghsl/results"

year <- "1975"

ghsl_data <- read_rds(sprintf("%s/urban_extent_uca_%s_cutoff20.rds", data_folder, year))

ghsl_data %>% filter(name_uca_case == "pelotas_rs") %>% mapview::mapview()

ghsl_data



