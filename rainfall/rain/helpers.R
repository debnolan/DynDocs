

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]]) 
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_map <- function(x){
  myMap <- get_googlemap(center = c(lon = -104.9477, lat = 39.29464), zoom = 7,
                         size = c(640,640), scale = 2, maptype = "terrain")
  ggmap(myMap, extent = "device") +
    geom_point(aes(x = lon, y = lat), data = FR[["info"]][(1:56)[-x],],
               alpha = 0.6, color="black", size = 4, shape = 21, fill = "white") +
    geom_point(aes(x = lon, y = lat), data = FR[["info"]][x,],
               alpha = 0.8, color="black", size = 6, shape = 23, fill = "red")  +
    coord_fixed(ylim = c(36.52, 41)) 
    lengend()
}

plot_map(4)




