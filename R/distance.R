#' @importFrom geosphere distm distHaversine
distance<-function(lon1, lat1, lon2, lat2){
  geosphere::distm(c(lon1, lat1), c(lon2, lat2), fun = geosphere::distHaversine)  
}

