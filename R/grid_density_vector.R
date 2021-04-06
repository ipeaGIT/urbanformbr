# load libraries
source('./R/setup.R')




###### read urban concentration areas
urban_areas <- geobr::read_urban_concentrations()
states <- unique(metros$abbrev_state)

  # how many spatial units
  urban_areas$code_urban_concentration %>% unique() %>% length()
  urban_areas$code_muni %>% unique() %>% length()

  # how much population
  sum(urban_areas$pop_total_2010)

  urban_areas %>%
    group_by(code_urban_concentration) %>%
    summarise(p=sum(pop_total_2010)) %>%
    summary(.$p)


###### load grid data
 # read from geobr
 # df <- read_statistical_grid(code_grid = 'all')

 # read locally
 df <- list.files('//storage1/geobr/data_gpkg/statistical_grid/2010',full.names = T) %>%
         pbapply::pblapply(., FUN = st_read)  %>%
         rbindlist()  %>%
         st_sf()


# subset non-empty cells
df <- subset(df, POP >0)

# subset columns
df <- select(df, ID_UNICO, POP, geom)

# get centroids (faster)
df <- st_centroid(df)



###### Keep only grid in urban areas

  # convert projection to UTM
  df <- st_transform(df, crs = 3857)
  urban_areas <- st_transform(urban_areas, crs = 3857)
  st_crs(df)$units # distances in meters
  head(df)

  # only grids in urban areas
  df_urb_concentration <- st_intersection(df, urban_areas)
  summary(df_urb_concentration$POP)


# Check population size of each urban area
setDT(df_urb_concentration)[, pop_urban_area := sum(POP), by= code_urban_concentration]

summary(df_urb_concentration$pop_urban_area)
#> Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> 75108   230796   674172  3325105  3314029 19142938

summary(df_urb_concentration$POP)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#> 1.0     9.0    41.0   140.8   195.0  6062.0


# Create function, vector version -----------------------------------------
df_urb_concentration <- st_sf(df_urb_concentration)


# Same as the matrix version, but cell by cell
get_density_vector <- function(i, df_urban_areas, points_latlon){


 # message("Calculating distance matrix")
  system.time(distance_matrix <- geodist::geodist(points_latlon[i,], points_latlon, measure = "cheap"))

  ### 10 kilometers
  pop_10km <- sum(df_urban_areas$POP[distance_matrix <= 10000], na.rm=T)

  ### 5 kilometers
  pop_05km <- sum(df_urban_areas$POP[distance_matrix <= 5000], na.rm=T)

  ### 1 kilometer
  pop_01km <- sum(df_urban_areas$POP[distance_matrix <= 1000], na.rm=T)

  temp_output <- data.table('ID_UNICO' = df_urban_areas$ID_UNICO[i],
                            'code_muni' = df_urban_areas$code_muni[i],
                            'code_urban_concentration' = df_urban_areas$code_urban_concentration[i],
                            'name_urban_concentration '= df_urban_areas$name_urban_concentration[i],
                            'pop_01km' = pop_01km,
                            'pop_05km' = pop_05km,
                            'pop_10km' = pop_10km)
  return(temp_output)
}
gc(reset = T, full = T)


# set number of cores
options(mc.cores=20)


# Apply function in parallel
options( future.globals.maxSize = 10000 * 1024 ^ 2 )
future::plan(strategy = 'multisession', workers=10)

areas <- unique(df_urb_concentration$code_urban_concentration)
s <- "4316907" ## Santa Maria, RS
s <- "3550308" ## Sao paulo, SP

tictoc::tic()
for( s in areas){ # states

  message(paste0("\n working on ", s,"\n"))

  # subset area
  df_urban_areas <- subset(df_urb_concentration, code_urban_concentration == s)

  # points to lat lon, to be used in distance calculation by geodist
  points_latlon <- suppressWarnings(
    sf::st_transform(df_urban_areas, 4326) %>%
      sf::st_coordinates()
  )

  # Apply function in parallel
  system.time( output_list <- furrr::future_map(.x=1:nrow(df_urban_areas),
                                                df_urban_areas,
                                                points_latlon,
                                                .f=get_density_vector, .progress =T) )

  output_df <- rbindlist(output_list)
  # Tempo Santa Maria (Vetor)
  # user  system elapsed
  # 3.246   0.381 142.615

  # system.time(output_df <- get_density_vector(1, df_urban_areas, points_latlon))
  # Tempo Santa Maria (Matriz)
  # user  system elapsed
  # 10.404   4.306  14.911

  # calculate area of buffers
  output_df$area10km2 <- pi * 10^2
  output_df$area05km2 <- pi *  5^2
  output_df$area01km2 <- pi *  1^2

  # calculate pop density
  output_df$pop_density10km2 <- output_df$pop_10km / output_df$area10km2
  output_df$pop_density05km2 <- output_df$pop_05km / output_df$area05km2
  output_df$pop_density01km2 <- output_df$pop_01km / output_df$area01km2

  # save
  fwrite(output_df, paste0('../../data/urbanformbr/density-experienced/output_density_vector_',s,'.csv') )

}

tictoc::toc()



##### bring geometry back
# read density estimates
output_files <- list.files('../../data/urbanformbr/density-experienced',pattern = '.csv',full.names = T  )
output_df <- lapply(output_files, fread) %>% rbindlist()
head(output_df)

summary(output_df$area10km2)
summary(output_sf$area10km2)
nrow(output_df)
nrow(df_urb_concentration)
nrow(output_sf)


# merge spatial geometries
output_sf <- left_join(df_urb_concentration, output_df)
head(output_sf)


# save
readr::write_rds(output_sf, '../../data/urbanformbr/density-experienced/density-experienced_urban-concentrations.rds',compress = 'gz')


