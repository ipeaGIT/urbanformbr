# UCI - Urban Centrality Index

library(sf)
library(fields)


#################### UCI function

uci <- function(sf_object, var_name){

# change projection to UTM
sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))


###### pre-calculations ######

### Determine which polygons are on the border

  # get perimeter of whole area
  boundry <- st_union(sf_object) %>% sf::st_boundary()

  # Determine border polygons
  int <- st_intersects(sf_object, boundry)
  border <- sapply(1:length(int),function(i){length(int[[i]])})
  sf_object$border <- border

  ## plot
  # plot(sf_object, col=border)

  # ggplot() +
  #   geom_sf(data=sf_object, aes(fill=factor(border))) +
  #   geom_sf(data=boundry, color='red', size=2) +
  #   theme_void()

    # ggplot() +
    # geom_sf(data=sf_object, aes(fill=factor(POP))) +
    # geom_sf(data=boundry, color='red', size=2) +
    # theme_void()


### normalize distribution of variable
  var_x <- sf_object[var_name][[1]]
  var_x <- matrix(var_x, length(var_x),1)
  var_x_norm <- var_x/sum(var_x) # normalization


### calculate distance matrix
  coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))
  distance <- fields::rdist(coords)
  # distance <- as.matrix(distance)
  plot(coords)

  # # which is faster?
  # system.time( distance <- geodist::geodist(coords, measure = "cheap") )
  # system.time( b <- rdist::rdist(coords) )


  # self distance >> REF Crafts & Mulatu (2006)
  # IMPORTANT: the area must be in the same unit as the distance matrix####
  n_reg <- nrow(distance)
  poly_areas <- st_area(sf_object)
  self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
  distance <- distance+self ## Sum distance matrix and self-distance




###### Support functions ######

  # Venables, Spatial Separation index
  venables <- function(b){
                          v <- t(b) %*% distance %*% b
                          return(v[1])
                          }

  # Location Coefficient (LC)
  location_coef <- function(x){
                               cl <-(sum(abs(x-(1/length(x)))))/2
                               return(cl)
                               }


###### UCI and its components ######

  ## location coefficient
  LC <- location_coef(var_x_norm)

  # Spatial separation index (venables)
  v <- venables(var_x_norm)

  ## MAX spatial separation
  # with all activities equally distributed along the border
  b <- border
  b[is.na(b)] <- 0
  b[b==1] <- 1/length(b[b==1])
  v_max <- venables(b)

  # Proximity Index PI
  proximity_idex <- 1-(v/(v_max))

  # UCI
  UCI <- LC * proximity_idex

  #return(UCI[1])
  output_df <- data.frame(
                    UCI = UCI,
                    location_coef = LC,
                    spatial_separation = v,
                    spatial_separation_max = v_max
                    )

  return(output_df)
}




### load grid data
# read locally
df <- list.files('//storage1/geobr/data_gpkg/statistical_grid/2010',full.names = T)[1] %>%
  pbapply::pblapply(., FUN = st_read)  %>%
  rbindlist()  %>%
  st_sf()


df2 <- df[1:25,]
plot(df2)


uci(sf_object = df2, var_name = 'POP')

uci(sf_object = df, var_name = 'POP')
