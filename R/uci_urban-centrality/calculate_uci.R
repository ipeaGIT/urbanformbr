# Calculate UCI - Urban Centrality Index

## load libraries
source('./R/setup.R')

## Load UCI function
source('./R/uci_urban-centrality/uci_fun.R')

library(parallelDist)

##### Read data input ---------------------


## urban concentration areas
urban_areas <- geobr::read_urban_concentrations()


## Load grid data
  ## read from geobr
  # df <- read_statistical_grid(code_grid = 'all')

  # read locally
  df <- list.files('//storage1/geobr/data_gpkg/statistical_grid/2010',full.names = T)[1] %>%
    pbapply::pblapply(., FUN = st_read)  %>%
    rbindlist()  %>%
    st_sf()

  # subset columns
  df <- select(df, ID_UNICO, nome_1KM, nome_5KM, POP, geom)

  # Keep only grid in urban areas
    # convert projection to UTM
    df <- st_transform(df, crs = 3857)
    urban_areas <- st_transform(urban_areas, crs = 3857)
    st_crs(df)$units # distances in meters
    head(df)

    # only grids in urban areas
    grid_urban_area <- st_intersection(df, urban_areas)
    summary(grid_urban_area$POP)


#rm(df)
gc(reset = T, full = T)

##### calculate UCI of population ---------------------

areas <- unique(grid_urban_area$code_urban_concentration)
code <- "4316907" ## Santa Maria, RS
code <- "3550308" ## Sao paulo, SP

# function
# get_uci <- function(code){


tictoc::tic()
for( code in areas){ # states
  # select area
  temp <- subset(grid_urban_area, code_urban_concentration == code)

  # calculate the mono/poly centrality of the spatial distribution of BIR74
  output_df <- uci(sf_object = temp, var_name = 'POP' )
  gc(reset = T, full = T)
  gc(reset = T, full = T)

  # add code info
  output_df$code_urban_concentration <- code
  fwrite(output_df, paste0('../../data/urbanformbr/urban_centrality/output_urban_centrality_',code,'.csv') )
}

tictoc::toc()



# system.time(output_list <- pbapply::pblapply(X=areas, FUN=get_uci))

# # set number of cores
# options(mc.cores=20)
#
# # Apply function in parallel
# options( future.globals.maxSize = 10000 * 1024 ^ 2 )
# future::plan(strategy = 'multisession', workers=10)
#
# system.time( output_list <- furrr::future_map(.x= areas,
#                                               .f=get_uci, .progress =T) )








