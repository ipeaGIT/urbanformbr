
library(geobr)
library(sf)
library(data.table)
library(dplyr)
library(mapview)
library(furrr)
library(future)
library(tidyverse)
library(geodist)
# options
mapviewOptions(platform = 'mapdeck')


# read metro areas
metros <- read_metro_area()
states <- unique(metros$abbrev_state)
metros$name_metro %>% unique() %>% length()
metros$code_muni %>% unique() %>% length()


immediate <- read_immediate_region()
immediate$code_immediate %>% unique() %>% length()


# load grid data
df <- read_statistical_grid()

# subset non-empty cells
df <- subset(df, POP >0)

# subset columns
df <- select(df, ID_UNICO, POP, geom)

# get centroids (faster)
df <- st_centroid(df)





# convert projection to UTM
df <- st_transform(df, crs = 3857)
immediate <- st_transform(immediate, crs = 3857)
st_crs(df)$units # distances in meters
head(df)

# only grids in metro areas
df_immediate <- st_intersection(df, immediate)



# only metro areas with population above 300K
setDT(df_immediate)[, pop_immediate := sum(POP), by=code_immediate]
summary(df_immediate$pop_immediate)
#> Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#>    4   164420   292324  1304424   686686 19197127

df_immediate[ pop_immediate >400000]$code_immediate  %>% unique() %>% length()
#> 84 immediate areas
df_immediate400K <- df_immediate[ pop_immediate >=400000]
df_immediate400K <- st_sf(df_immediate400K)

summary(df_immediate400K$pop_immediate)
#>   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> 405052   540887   946129  3016062  2987653 19197127

summary(df_immediate400K$POP)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#>    1       6      22     110     127    6062



subset(df_immediate400K, pop_immediate < 400000)$name_immediate %>% unique()

nrow(df)

nrow(df_immediate400K)

df_immediate400K %>% mapview(zcol = "name_immediate")
st_crs(df_immediate400K)



# Create function, vector version -----------------------------------------

# Create function, vector version -----------------------------------------
# Same as the matrix version, but cell by cell
get_density_vector <- function(i, df_metro_temp, points_latlon){


  message("Calculating distance matrix")
  system.time(distance_matrix <- geodist::geodist(points_latlon[i,], points_latlon, measure = "cheap"))

  ### 10 kilometers
  pop_10km <- sum(df_metro_temp$POP[distance_matrix <= 10000])

  ### 5 kilometers
  pop_05km <- sum(df_metro_temp$POP[distance_matrix <= 5000])

  ### 1 kilometer
  pop_01km <- sum(df_metro_temp$POP[distance_matrix <= 1000])

  temp_output <- data.table('ID_UNICO' = df_metro_temp$ID_UNICO[i],
                            'code_muni' = df_metro_temp$code_muni[i],
                            'abbrev_state' = df_metro_temp$abbrev_state[i],
                            'name_metro'= df_metro_temp$name_metro[i],
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

areas <- unique(df_immediate400K$code_immediate)
s <- "430011" ## Santa Maria, RS
for( s in areas){ # states

  message(paste0("\n working on ", s,"\n"))

  # subset area
  df_metro_temp <- subset(df_immediate400K, code_immediate == s)

  # points to lat lon, to be used in distance calculation by geodist
  points_latlon <- suppressWarnings(
    sf::st_transform(df_metro_temp, 4326) %>%
      sf::st_coordinates()
  )

  # Apply function in parallel
  system.time( output_list <- furrr::future_map(.x=1:nrow(df_metro_temp),
                                                df_metro_temp,
                                                points_latlon,
                                                .f=get_density_vector, .progress =T) )

  output_df <- rbindlist(output_list)
  # Tempo Santa Maria (Vetor)
  # user  system elapsed
  # 3.246   0.381 142.615

  # system.time(output_df <- get_density_vector(1, df_metro_temp, points_latlon))
  # Tempo Santa Maria (Matriz)
  # user  system elapsed
  # 10.404   4.306  14.911

  # save
  fwrite(output_df, paste0('output_density_vector_',s,'.csv') )

}








