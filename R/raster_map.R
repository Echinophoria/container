# mapping
LAT1 =  min(lines$lat)*0.9999 ; LAT2 = max(lines$lat)*1.0001
LON1 = min(lines$lon)*0.9999 ; LON2 = max(lines$lon)*1.0001

map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = 14,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[1],
               mergeTiles = TRUE)
plot(map)

map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

library(raster)
r <- raster(map.latlon)

map <- as.data.frame(r, xy=TRUE)
map$RGB <- rgb(map$layer.1, map$layer.2, map$layer.3, maxColorValue=255)