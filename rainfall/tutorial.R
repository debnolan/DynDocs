library(ggmap)
setwd("~/GitHub/DynDocs/rainfall")

##  plot1: show rain station map 
myMap <- get_googlemap(center = c(lon = -104.9477, lat = 39.29464), zoom = 7,
                       size = c(640,640), scale = 2, maptype = "terrain")
ggmap(myMap, extent = "device") +
  geom_point(aes(x = lon, y = lat), data = FR[["info"]][1:56,],
             alpha = 0.6, color="black", size = 4, shape = 21, fill = "white") +
  geom_point(aes(x = lon, y = lat), data = FR[["info"]][37,],
             alpha = 0.8, color="black", size = 6, shape = 23, fill = "red")  +
  coord_fixed(ylim = c(36.52, 41)) 

##  plot2: the data



