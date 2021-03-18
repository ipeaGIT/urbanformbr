
library(geobr)
library(sf)
library(data.table)
library(dplyr)
library(mapview)
library(furrr)
library(future)

# options
mapviewOptions(platform = 'deckgl')


# read metro areas
metros <- read_metro_area()
states <- unique(metros$abbrev_state)
metros$name_metro %>% unique() %>% length()
metros$code_muni %>% unique() %>% length()


immediate <- read_immediate_region()
immediate$code_immediate %>% unique() %>% length()


# load grid data
df <- read_statistical_grid(code_grid = 'all')

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



subset(df_immediate300K, pop_immediate < 400000)$name_immediate %>% unique()

nrow(df)

nrow(df_immediate400K)




# Create function
get_density <- function(i, df_metro_temp){

  # select grid row
  x <- df_metro_temp[i,]

  # generate buffer
  buff_10km <- st_buffer(x, dist = 10000)
  buff_05km <- st_buffer(x, dist = 5000)
  buff_01km <- st_buffer(x, dist = 1000)

  # subset pop in buffer
  df_10km <- st_intersection(df_metro_temp, buff_10km)
  df_05km <- st_intersection(df_10km, buff_05km)
  df_01km <- st_intersection(df_05km, buff_01km)
  # mapview(df_10km) + buff_10km + buff_05km + buff_01km

  # sum pop in buffer
  pop_10km <- sum(df_10km$POP)
  pop_05km <- sum(df_05km$POP)
  pop_01km <- sum(df_01km$POP)

  temp_output <- data.table('ID_UNICO' = x$ID_UNICO,
                            'code_muni' = x$code_muni,
                            'abbrev_state' = x$abbrev_state,
                            'name_metro'= x$name_metro,
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

for( s in areas[1:3]){ # states

  message(paste0("\n working on ", s,"\n"))

# subset area
df_metro_temp <- subset(df_immediate400K, code_immediate == s)

# Apply function in parallel
system.time( output_list <- furrr::future_map(.x=1:nrow(df_metro_temp), df_metro_temp, .f=get_density, .progress =T) )

output_df <- rbindlist(output_list)

# save
fwrite(output_df, paste0('output_density_',s,'.csv') )

}





