library(sf)
library(parallelDist)
library(magrittr)
library(dplyr)
library(furrr)
library(readr)
library(data.table)
library(ggplot2)

# change projection to UTM
df3 <- suppressWarnings(sf::st_transform(df3, 3857))


uci_max <- function(round, sf_object=df3){

  set.seed(round)
  sf_object$POP <- runif(25, min=0, max=10000)

    ###### pre-calculations ######

  ### Determine which polygons are on the border

  # get perimeter of whole area
  boundry <- st_union(sf_object) %>% sf::st_boundary()

  # Determine border polygons
  int <- st_intersects(sf_object, boundry)
  border <- sapply(1:length(int),function(i){length(int[[i]])})
  sf_object$border <- border


  ### normalize distribution of variable
  var_x <- sf_object['POP'][[1]]
  var_x <- matrix(var_x, length(var_x),1)
  var_x_norm <- var_x/sum(var_x) # normalization


  ### calculate distance matrix
  coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))
  distance <- parallelDist::parDist(coords)
  distance <- as.matrix(distance)
  #plot(coords)

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
    round = round,
    UCI = UCI,
    location_coef = LC,
    proximity_idex = proximity_idex,
    spatial_separation = v,
    spatial_separation_max = v_max
  )

  fwrite(output_df, paste0('C:/Users/user/Desktop/tests/output_',round,'.csv'))
  readr::write_rds(sf_object, paste0('C:/Users/user/Desktop/tests/round_',round,'.rds'))
  #return(output_df)
}

future::plan('multisession')

system.time(res <- furrr::future_map(.x=1:5000, .f=uci, .progress = T) )


f <- list.files('C:/Users/user/Desktop/tests/', full.names = T, pattern = '.csv')
res <- pbapply::pblapply(X=f, FUN=fread)
res <- rbindlist(res)
gc()
head(res)

any(res$spatial_separation > 626)

ggplot(data=res) +
  geom_point(aes(x=location_coef, y=proximity_idex), alpha=.1)
#  geom_point(aes(x=UCI, y=spatial_separation_max), alpha=.1, color='red')

ggplot(data=res) +
  geom_point(aes(x= UCI, y=proximity_idex), alpha=.1)

ggplot(data=res) +
  geom_point(aes(x= UCI, y=location_coef), alpha=.1)

ggplot(data=res) +
  geom_point(aes(x=UCI, y=spatial_separation), alpha=.1) +
  geom_point(aes(x=UCI, y=spatial_separation_max), alpha=.1, color='red')



# check max / min values
max <- res$round[which.max(res$UCI)]
min <- res$round[which.min(res$UCI)]

max <- read_rds(paste0('C:/Users/user/Desktop/tests/round_',max,'.rds'))
min <- read_rds(paste0('C:/Users/user/Desktop/tests/round_',min,'.rds'))
max$round <- 'max'
min$round <- 'min'

df <- rbind(max, min)
ggplot(df) + geom_sf(aes(fill=POP)) +
  facet_grid(~round) +
  theme_void()






plot(df3['POP'])

sum(df3$POP)

df3$POP <-
c(500,0,0,0,500,
  0,0,0,0,0,
  0,0,0,0,0,
  0,0,0,0,0,
  500,0,0,0,500)

uci(df3, 'POP')


df3 <- structure(list(ID_UNICO = c("200ME53000N96322", "200ME53002N96322",
                                   "200ME53004N96322", "200ME53006N96322", "200ME53008N96322", "200ME53000N96324",
                                   "200ME53002N96324", "200ME53004N96324", "200ME53006N96324", "200ME53008N96324",
                                   "200ME53000N96326", "200ME53002N96326", "200ME53004N96326", "200ME53006N96326",
                                   "200ME53008N96326", "200ME53000N96328", "200ME53002N96328", "200ME53004N96328",
                                   "200ME53006N96328", "200ME53008N96328", "200ME53000N96330", "200ME53002N96330",
                                   "200ME53004N96330", "200ME53006N96330", "200ME53008N96330"),
                      POP = c(673.712232848629, 94.8578554671258, 492.596120806411,
                              461.551840649918, 375.216530868784, 991.099219536409, 176.350713707507,
                              813.435208518058, 68.4466371312737, 400.44974675402, 141.144325723872,
                              193.309862399474, 841.351716779172, 719.913988374174, 267.212083097547,
                              495.001644827425, 83.1138978246599, 353.884240612388, 969.208805356175,
                              624.714189674705, 664.618249749765, 312.489656498656, 405.689612729475,
                              996.077371528372, 855.082356370986), geom = structure(list(
                                structure(list(list(structure(c(-51.1684037445372, -51.168422850634,
                                                                -51.1665352617928, -51.1665161431055, -51.1684037445372,
                                                                -15.2639638327646, -15.2621811516388, -15.262163101346,
                                                                -15.2639457824719, -15.2639638327646), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1665161431055, -51.1665352617928,
                                                                                   -51.1646476729517, -51.1646285416738, -51.1665161431055,
                                                                                   -15.2639457824719, -15.262163101346, -15.2621450393621,
                                                                                   -15.2639277195885, -15.2639457824719), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1646285416738, -51.1646476729517,
                                                                -51.1627600841106, -51.1627409402421, -51.1646285416738,
                                                                -15.2639277195885, -15.2621450393621, -15.2621269647876,
                                                                -15.2639096450141, -15.2639277195885), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1627409402421, -51.1627600841106,
                                                                                   -51.1608724961687, -51.1608533388105, -51.1627409402421,
                                                                                   -15.2639096450141, -15.2621269647876, -15.2621088785219,
                                                                                   -15.2638915587484, -15.2639096450141), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1608533388105, -51.1608724961687,
                                                                -51.1589849082269, -51.1589657382781, -51.1608533388105,
                                                                -15.2638915587484, -15.2621088785219, -15.262090780565,
                                                                -15.2638734607916, -15.2638915587484), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.168422850634, -51.1684419567309,
                                                                                   -51.1665543804802, -51.1665352617928, -51.168422850634,
                                                                                   -15.2621811516388, -15.2603984732109, -15.2603804229182,
                                                                                   -15.262163101346, -15.2621811516388), .Dim = c(5L,
                                                                                                                                  2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1665352617928, -51.1665543804802,
                                                                -51.1646668042296, -51.1646476729517, -51.1665352617928,
                                                                -15.262163101346, -15.2603804229182, -15.2603623609342,
                                                                -15.2621450393621, -15.262163101346), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1646476729517, -51.1646668042296,
                                                                                   -51.1627792288783, -51.1627600841106, -51.1646476729517,
                                                                                   -15.2621450393621, -15.2603623609342, -15.260344287259,
                                                                                   -15.2621269647876, -15.2621450393621), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1627600841106, -51.1627792288783,
                                                                -51.1608916526276, -51.1608724961687, -51.1627600841106,
                                                                -15.2621269647876, -15.260344287259, -15.2603262009933,
                                                                -15.2621088785219, -15.2621269647876), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1608724961687, -51.1608916526276,
                                                                                   -51.1590040772763, -51.1589849082269, -51.1608724961687,
                                                                                   -15.2621088785219, -15.2603262009933, -15.2603081030365,
                                                                                   -15.262090780565, -15.2621088785219), .Dim = c(5L,
                                                                                                                                  2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1684419567309, -51.1684610619284,
                                                                -51.1665734991676, -51.1665543804802, -51.1684419567309,
                                                                -15.2603984732109, -15.258615797481, -15.2585977480875,
                                                                -15.2603804229182, -15.2603984732109), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1665543804802, -51.1665734991676,
                                                                                   -51.1646859355075, -51.1646668042296, -51.1665543804802,
                                                                                   -15.2603804229182, -15.2585977480875, -15.2585796861036,
                                                                                   -15.2603623609342, -15.2603804229182), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1646668042296, -51.1646859355075,
                                                                -51.1627983727466, -51.1627792288783, -51.1646668042296,
                                                                -15.2603623609342, -15.2585796861036, -15.2585616124284,
                                                                -15.260344287259, -15.2603623609342), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1627792288783, -51.1627983727466,
                                                                                   -51.1609108099859, -51.1608916526276, -51.1627792288783,
                                                                                   -15.260344287259, -15.2585616124284, -15.2585435261628,
                                                                                   -15.2603262009933, -15.260344287259), .Dim = c(5L,
                                                                                                                                  2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1608916526276, -51.1609108099859,
                                                                -51.159023247225, -51.1590040772763, -51.1608916526276,
                                                                -15.2603262009933, -15.2585435261628, -15.2585254282059,
                                                                -15.2603081030365, -15.2603262009933), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1684610619284, -51.1684801680253,
                                                                                   -51.1665926169557, -51.1665734991676, -51.1684610619284,
                                                                                   -15.258615797481, -15.2568331253484, -15.2568150750556,
                                                                                   -15.2585977480875, -15.258615797481), .Dim = c(5L,
                                                                                                                                  2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1665734991676, -51.1665926169557,
                                                                -51.164705065886, -51.1646859355075, -51.1665734991676,
                                                                -15.2585977480875, -15.2568150750556, -15.256797013971,
                                                                -15.2585796861036, -15.2585977480875), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1646859355075, -51.164705065886,
                                                                                   -51.1628175157157, -51.1627983727466, -51.1646859355075,
                                                                                   -15.2585796861036, -15.256797013971, -15.2567789402958,
                                                                                   -15.2585616124284, -15.2585796861036), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1627983727466, -51.1628175157157,
                                                                -51.1609299655454, -51.1609108099859, -51.1627983727466,
                                                                -15.2585616124284, -15.2567789402958, -15.2567608540301,
                                                                -15.2585435261628, -15.2585616124284), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1609108099859, -51.1609299655454,
                                                                                   -51.1590424162745, -51.159023247225, -51.1609108099859,
                                                                                   -15.2585435261628, -15.2567608540301, -15.2567427560733,
                                                                                   -15.2585254282059, -15.2585435261628), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1684801680253, -51.1684992732228,
                                                                -51.1666117347437, -51.1665926169557, -51.1684801680253,
                                                                -15.2568331253484, -15.2550504550144, -15.255032405621,
                                                                -15.2568150750556, -15.2568331253484), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1665926169557, -51.1666117347437,
                                                                                   -51.1647241971639, -51.164705065886, -51.1665926169557,
                                                                                   -15.2568150750556, -15.255032405621, -15.255014343637,
                                                                                   -15.256797013971, -15.2568150750556), .Dim = c(5L,
                                                                                                                                  2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.164705065886, -51.1647241971639,
                                                                -51.1628366595841, -51.1628175157157, -51.164705065886,
                                                                -15.256797013971, -15.255014343637, -15.2549962699619,
                                                                -15.2567789402958, -15.256797013971), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg")), structure(list(
                                                                  list(structure(c(-51.1628175157157, -51.1628366595841,
                                                                                   -51.1609491220044, -51.1609299655454, -51.1628175157157,
                                                                                   -15.2567789402958, -15.2549962699619, -15.2549781845955,
                                                                                   -15.2567608540301, -15.2567789402958), .Dim = c(5L,
                                                                                                                                   2L)))), class = c("XY", "MULTIPOLYGON", "sfg")),
                                structure(list(list(structure(c(-51.1609299655454, -51.1609491220044,
                                                                -51.1590615853239, -51.1590424162745, -51.1609299655454,
                                                                -15.2567608540301, -15.2549781845955, -15.2549600866386,
                                                                -15.2567427560733, -15.2567608540301), .Dim = c(5L, 2L
                                                                )))), class = c("XY", "MULTIPOLYGON", "sfg"))), class = c("sfc_MULTIPOLYGON",
                                                                                                                          "sfc"), precision = 0, bbox = structure(c(xmin = -51.1684992732228,
                                                                                                                                                                    ymin = -15.2639638327646, xmax = -51.1589657382781, ymax = -15.2549600866386
                                                                                                                          ), class = "bbox"), crs = structure(list(input = "SIRGAS 2000",
                                                                                                                                                                   wkt = "GEOGCRS[\"SIRGAS 2000\",\n    DATUM[\"Sistema de Referencia Geocentrico para las AmericaS 2000\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"Latin America - Central America and South America - onshore and offshore. Brazil - onshore and offshore.\"],\n        BBOX[-59.87,-122.19,32.72,-25.28]],\n    ID[\"EPSG\",4674]]"), class = "crs"), n_empty = 0L)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           25L), class = c("sf", "data.frame"), sf_column = "geom", agr = structure(c(ID_UNICO = NA_integer_,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      POP = NA_integer_), .Label = c("constant", "aggregate", "identity"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ), class = "factor"))
