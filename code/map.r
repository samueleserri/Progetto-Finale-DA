
library(maps)




oliveGPS <- oliveoil[,1:2]
oliveGPS$lat <- NA
oliveGPS$long <- NA

region_coords <- list(
  "Apulia.north" = c(lat = 41.4, long = 15.5),
  "Calabria" = c(lat = 39.0, long = 16.5),
  "Apulia.south" = c(lat = 40.0, long = 18.0),
  "Sicily" = c(lat = 37.6, long = 14.1),
  "Sardinia.inland" = c(lat = 40.0, long = 9.0),
  "Sardinia.coast" = c(lat = 40.0, long = 9.5),
  "Liguria.east" = c(lat = 44.3, long = 9.5),
  "Liguria.west" = c(lat = 44.0, long = 8.0),
  "Umbria" = c(lat = 42.9, long = 12.6)
)

for (i in 1:nrow(oliveGPS)) {
  region <- oliveGPS$region[i]
  if (region %in% names(region_coords)) {
    oliveGPS$lat[i] <- region_coords[[region]]["lat"]
    oliveGPS$long[i] <- region_coords[[region]]["long"]
  }
}

for (i in 1:nrow(oliveGPS)) {
  oliveGPS$lat[i] <- oliveGPS$lat[i] + runif(1, min = -0.4, max = 0.4)
  oliveGPS$long[i] <- oliveGPS$long[i] + runif(1, min = -0.4, max = 0.4)
}



map("italy", col="white", fill=TRUE, lty=1, lwd=1, border="black")
points(oliveGPS$long, oliveGPS$lat, col=km.out$cluster+1, pch=19, cex=0.3)

