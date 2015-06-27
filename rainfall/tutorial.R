library(ggmap)
library(scales)
setwd("~/GitHub/DynDocs/rainfall")
##  save the ggmap
plot_map <- function(x){
  dat = cbind(FR[["info"]][,1:2], var = c(rep("Other stations",times = x-1),"Currently selected", rep("Other stations", times = 56-x)))
  myMap <- get_googlemap(center = c(lon = -104.9477, lat = 39.29464), zoom = 7,
                         size = c(640,640), scale = 2, maptype = "terrain")
  map = ggmap(myMap, extent = "device") +
    geom_point(aes(x = lon, y = lat, size = factor(var), colour = factor(var), 
                   alpha = factor(var), fill = factor(var), shape = factor(var)), data = dat)+
    scale_colour_manual(values = c("black","black"))+
    scale_size_manual(values = c(6,4))+
    scale_alpha_manual(values = c(0.8,0.6))+
    scale_fill_manual(values = c("red", "white"))+
    scale_shape_manual(values = c(23, 21))+
    theme(legend.position = c(.85,.9),
          legend.title = element_blank(),
          legend.text=element_text(size=12),
          legend.background = element_rect(fill=alpha('grey', 0.7)))
ggsave (paste("rain/www/images/",x,".png",sep = ""), dpi = 90, width = 7.53)
}

for (i in 1:56)
  plot_map(i)



###  Threshold plot

thresh = quantile(rainfall[[2]]$precip, 95/100)
which(rainfall[[2]]$precip>thresh)
par(mfrow = c(1,1),bg = '#FAFAD2', mai = c(0.5,0.5,0.3,0.1))
plot(rainfall[[2]]$precip[which(rainfall[[2]]$precip>thresh)]~rainfall[[2]]$time[which(rainfall[[2]]$precip>thresh)],
     ylim = range(rainfall[[2]]$precip),
     xlab = "", ylab = "Precipitation (mm)", main = paste("Daily precipitation of weather station", 2), col = 'darkblue')
points(rainfall[[2]]$precip[which(rainfall[[2]]$precip<=thresh)]~rainfall[[2]]$time[which(rainfall[[2]]$precip<=thresh)],
       col = 'black')
abline(h = quantile(rainfall[[2]]$precip, 95/100), col = 'red', lwd = 2)


## Four diagnostic plots

fit = fevd(precip, rainfall[[2]], threshold = quantile(rainfall[[2]]$precip, 95/100), type="GP", units = "mm")
par(mfrow = c(2,2), bg = '#FAFAD2',mai = c(0.45,0.45,0.3,0.1))
plot(fit, "probprob", main ="Probability Plot")
plot(fit, "qq", main ="Quantile Plot")
plot(fit, type = "rl", rperiods= c(2,5,10,20,50,80,100,120,200), main ="Return Level Plot")
plot(fit, type = "density", main = "Density Plot")










