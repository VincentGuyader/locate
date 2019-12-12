#' @importFrom geosphere distm distHaversine
distance <- function(lon1, lat1, lon2, lat2) {
  d <-  geosphere::distm(c(lon1, lat1), c(lon2, lat2), fun = geosphere::distHaversine)
  
  list(affichage = set_affichage(d),valeur = d)
}


set_affichage <- function(d) {
  class(d) <- "distance"
  if (d > 500) {
    attr(d, "affichage") <- paste(round(d / 1000, 1), "km")
  } else {
    attr(d, "affichage") <- paste(round(d, 1), "m")
  }
  d
}

`+.distance` <- function(x, y) {
  set_affichage(unclass(x) + unclass(y))
}

print.distance <- function(x, ...) {
  print(attributes(x)$affichage)
}
