# devtools::install_github("hadley/lineprof")


library(parallelDist)
library(geodist)
library(fields)
library(stats)
library(bigmemory)
library(Rcpp)

library(lineprof)
library(geobr)
library(sf)
library(ggplot2)
library(data.table)


# data input
df <- geobr::read_weighting_area()
gc(reset = T)

# convert projection to UTM
df <- st_transform(df, crs = 3857)

# get spatial coordinates
coords <- suppressWarnings(st_coordinates( st_centroid(df) ))

# prepare customized rcpp function
sourceCpp("euc_dist.cpp")

bigMatrixEuc <- function(bigMat){
  zeros <- big.matrix(nrow = nrow(bigMat)-1,
                      ncol = nrow(bigMat)-1,
                      init = 0,
                      type = typeof(bigMat))
  BigArmaEuc(bigMat@address, zeros@address)
  return(zeros)
}




### Start tests
perf_fields  <- lineprof(dist_fields <- fields::rdist(coords) )
perf_geodist <- lineprof(dist_geodist <- geodist::geodist(coords, measure = "cheap") )
perf_stats   <- lineprof(dist_stats <- stats::dist(coords) )
perf_parDist <- lineprof(dist_parDist <- parallelDist::parDist(coords, method = "euclidean") )
perf_rcpp <- lineprof(dist_rcpp <- bigMatrixEuc( as.big.matrix(coords) ) )

perf_fields$package  <- 'fields'
perf_geodist$package <- 'geodist'
perf_stats$package   <- 'stats'
perf_parDist$package <- 'parDist'
perf_rcpp$package <- 'rcpp'


# gather results
benchmrk <- rbind(perf_fields, perf_geodist, perf_stats , perf_parDist, perf_rcpp)
benchmrk <- setDT(benchmrk)[, .(time  =sum(time), alloc = sum(alloc)), by=package][order(alloc)]
benchmrk
#>   package   time        alloc
#>1: parDist  0.298 5.369186e-04
#>2:  fields  1.079 9.486198e-03
#>3:    rcpp 54.422 2.161113e+00
#>4:   stats  0.770 5.788603e+01
#>5: geodist  2.513 1.157635e+02

# plot
ggplot(benchmrk, aes(x=alloc , y=time, color= package, label=package)) +
  geom_label(alpha=.5) +
  coord_trans(x="log10", y="log10") +
  theme(legend.position = "none")



check_mem <- function(distance){
n_reg <- nrow(distance)
self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
d <- distance+self ## Sum distance matrix and self-distance
}

lineprof( a <- check_mem(dist_parDist) )
lineprof( b <- check_mem(dist_fields) )
