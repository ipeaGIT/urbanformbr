# UCI - Urban Centrality Index

library(sf)
library(fields)


#################### UCI function

uci <- function(sf_object, var_name){

# # change projection to UTM
# sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))


###### pre-calculations ######

### Determine which polygons are on the border

  # make sure we have valid geometries
  sf_object <- sf::st_make_valid(sf_object)

  # aggregate to smaller resolution
  sf_object <- sf_object %>% group_by(nome_1KM) %>% summarise(POP = sum(POP),
                                                                geom = st_union(geom))

  # get perimeter of whole area
  sf_object$wholegroup <- 1
  boundry <-  dissolve_polygons(mysf=sf_object, group_column='wholegroup') %>%
    st_union() %>% sf::st_boundary()
  # boundry <- st_union(sf_object) %>% sf::st_boundary()
  # plot(boundry)


  # Determine border polygons
  int <- st_intersects(sf_object, boundry)
  border <- sapply(1:length(int),function(i){length(int[[i]])})
  sf_object$border <- border

  ## plot
  # plot(sf_object['border'])


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
  # get centroids
  coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))

  # get distances

    # distance <- fields::rdist(coords)
    # distance <- geodist::geodist(coords, measure = "cheap")
    # distance <- stats::dist(coords)
    distance <- parallelDist::parDist(coords, method = "euclidean")
    distance <- as.matrix(distance)
    # distance <- bigmemory::as.big.matrix(distance)


   # plot(coords)

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




# support functions
###### convert to MULTIPOLYGON -----------------
to_multipolygon <- function(temp_sf){

  # get geometry types
  geom_types <- st_geometry_type(temp_sf) %>% unique() %>% as.character()

  # checks
  if (length(geom_types) > 1 | any(  !( geom_types %like% "MULTIPOLYGON"))) {

    # remove linstring
    temp_sf <- subset(temp_sf, st_geometry_type(temp_sf) %>% as.character() != "LINESTRING")

    # get polyons
    temp_sf <- st_collection_extract(temp_sf, "POLYGON")
    temp_sf <- sf::st_cast(temp_sf, "MULTIPOLYGON")
    return(temp_sf)

  } else {
    return(temp_sf) }
}


##### Dissolve borders temp_sf -----------------

## Function to clean and dissolve the borders of polygons by groups
dissolve_polygons <- function(mysf, group_column){


  # a) make sure we have valid geometries
  temp_sf <- sf::st_make_valid(mysf)
  temp_sf <- temp_sf %>% st_buffer(0)

  # b) make sure we have sf MULTIPOLYGON
  #temp_sf1 <- temp_sf %>% st_cast("MULTIPOLYGON")
  temp_sf1 <- to_multipolygon(temp_sf)

  # c) long but complete dissolve function
  dissolvefun <- function(grp){

    # c.1) subset region
    temp_region <- subset(mysf, get(group_column, mysf)== grp )


    # c.2) create attribute with the number of points each polygon has
    points_in_each_polygon = sapply(1:dim(temp_region)[1], function(i)
      length(st_coordinates(temp_region$geom[i])))

    temp_region$points_in_each_polygon <- points_in_each_polygon
    mypols <- subset(temp_region, points_in_each_polygon > 0)

    # d) convert to sp
    sf_regiona <- mypols %>% as("Spatial")
    sf_regiona <- rgeos::gBuffer(sf_regiona, byid=TRUE, width=0) # correct eventual topology issues

    # c) dissolve borders to create country file
    result <- maptools::unionSpatialPolygons(sf_regiona, rep(TRUE, nrow(sf_regiona@data))) # dissolve


    # d) get rid of holes
    outerRings = Filter(function(f){f@ringDir==1},result@polygons[[1]]@Polygons)
    outerBounds = sp::SpatialPolygons(list(sp::Polygons(outerRings,ID=1)))

    # e) convert back to sf data
    outerBounds <- st_as_sf(outerBounds)
    outerBounds <- st_set_crs(outerBounds, st_crs(mysf))
    st_crs(outerBounds) <- st_crs(mysf)

    # retrieve code_region info and reorder columns
    outerBounds <- dplyr::mutate(outerBounds, group_column = grp)
    outerBounds <- dplyr::select(outerBounds, group_column, geometry)
    names(outerBounds)[1] <- group_column
    return(outerBounds)
  }


  # Apply sub-function
  groups_sf <- pbapply::pblapply(X = unique(get(group_column, mysf)), FUN = dissolvefun )

  # rbind results
  temp_sf <- do.call('rbind', groups_sf)
  return(temp_sf)
}

