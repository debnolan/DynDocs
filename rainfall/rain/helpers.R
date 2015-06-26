load("data/FrontRange.rda")
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
myprint <- function (x, ...) 
{
  tmp <- attributes(x)
  cat("\n")
  print(tmp$method)
  if (!is.null(tmp$R)) 
    cat(tmp$R, " iterations\n")
  cat("\n")
  if (!is.matrix(x)) {
    print(paste(names(c(x))[2], ": ", round(x[2], digits = 2), 
                sep = ""))
    cat("\n")
    print(paste(tmp$conf.level, "% Confidence Interval: (", 
                round(x[1], digits = 2), ", ", round(x[3], digits = 2), 
                ")", sep = ""))
  }
  else {
    y <- x
    attributes(y) <- NULL
    y <- matrix(y, tmp$dim[1], tmp$dim[2])
    colnames(y) <- tmp$dimnames[[2]]
    rownames(y) <- tmp$dimnames[[1]]
    print(y)
  }
  cat("\n")
  invisible()
}







