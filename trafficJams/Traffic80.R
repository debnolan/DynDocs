setwd("~/Documents/Projects/DynDocs/trafficJams")

traf <- read.csv("http://www.stat.berkeley.edu/~rice/UCLA/flow-occ-table.txt")
traf2 <- read.csv("http://www.stat.berkeley.edu/users/nolan/stat133/data/flow-occ-table.txt")
> identical(traf, traf2)
[1] TRUE

nearPointIndex <- function(data, point, threshold = 0.1) {
	# finds the index (of a data row) of a nearest point
	# data: matrix with 2 cols or a data frame
	# point: coordinates of a point 
	# threshold: proportion of the range to be included
	
	scaled <- scale(rbind(point, data))
	data   <- scaled[-1, ]
	point  <- scaled[1, ]
	rangeX <- point[1] + c(-1, 1)*diff(range(data[, 1]))*threshold
	rangeY <- point[2] + c(-1, 1)*diff(range(data[, 2]))*threshold
	close <- which(rangeX[1] < data[, 1] & data[, 1] < rangeX[2] &
	               rangeY[1] < data[, 2] & data[, 2] < rangeY[2])
    distclose <- sqrt((data[close, 1] - point[1])^2 + (data[close, 2] - point[2])^2)               
    return(close[which.min(distclose)])
}

PointsToLine <- function(x, y) {
  # x, y: coordinates of two points (vectors of length 2)
  slope <- (y[2] - y[1])/(x[2] - x[1])
  icept <- y[1] - x[1]*slope
  return(c(icept = icept, slope = slope))
}

#------------------------- choosing colors ------------------------------#
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(9, "Blues")
display.brewer.pal(9, "GnBu")
display.brewer.pal(9, "YlGnBu")
display.brewer.pal(9, "Purples")
display.brewer.pal(9, "RdPu")
display.brewer.pal(9, "Greys")

blues <- c(brewer.pal(9, "Blues")[c(5, 8)], 
           brewer.pal(9, "GnBu")[c(5, 6, 8, 9)], 
           brewer.pal(9, "YlGnBu")[c(5, 8)])
purples <- c(brewer.pal(9, "Purples")[c(6, 8, 9)],   
             brewer.pal(9, "RdPu")[c(8, 9)])
greys <- brewer.pal(9, "Greys")[c(5, 6, 8)]     
c1 <- c(blues, purples, greys)   
c2 <- c("orchid1", "mediumorchid1", "purple3", "slateblue", "midnightblue",
        "royalblue3", "blue", "lightskyblue", "gray24", "mistyrose4", 
        "navajowhite3","lightyellow4", "lightyellow3", "lemonchiffon4",
        "lemonchiffon3")
c3 <- c(c2[10:11], c1[5:4], c2[3:2])
c4 <- c(c2[9], c1[14], c1[5:4], c2[3:2])
c5 <- c(c2[9], c2[15], c1[5:4], c2[3:2])
n1 <- length(c1)
n2 <- length(c2)  
n3 <- length(c3)      
plot(1:n1, rep(4, n1), col = c1, pch = 16, cex = 5, ylim = c(-1, 5), xlim = c(0, max(n1, n2)))             
points(1:n2, rep(3, n2), col = c2, pch = 16, cex = 5)
points(1:n3, rep(2, n3), col = c3, pch = 16, cex = 5)
points(1:n3, rep(1, n3), col = c4, pch = 16, cex = 5) # favorite
points(1:n3, rep(0, n3), col = c5, pch = 16, cex = 5)
points(15, 1, col = "grey", pch = 16, cex = 5)

cols.rgb <- col2rgb(cols) #(c4)
cols.hex <- as.character(as.hexmode(cols.rgb))
cols <- apply(cols.hex, 2, function(x) paste("#", paste(x, collapse = ""), sep = ""))
plot(1:12, col = c(cols1, cols), pch = 19, cex = 5)
col.light <- c("turquoise", "tan", "khaki3", "palevioletred", "violetred1", 
               "maroon2", "sandybrown", "peachpuff3")
cols <- matrix(c("#3d3d3d", "#0868ac", "#7d26cd",  # or "black" first
                 "bisque4", "darkturquoise", "violetred2"), nrow = 3)   

#-----------------------------------------------------------------------------#
#                                	PREP                                      #
#-----------------------------------------------------------------------------#

library(shiny)

#traf <- read.csv("http://www.stat.berkeley.edu/~rice/UCLA/flow-occ-table.txt")
traf <- read.csv("flow-occ-table.txt")
nint <- nrow(traf) # number of 5-min intervals, 1740

### Suppose starts on March 14th, 2003, Friday at midnight (we don't know the time)
### then ends at 1am on Thur, March 20th
day <- rep(c("Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thur"), 
                each = 24*12, length.out = nint)
time <- paste(rep(0:23, each = 12, length.out = nint), 
                   rep(c("00", "05", seq(10, 55, by = 5)), length.out =    nint), 
              sep = ":") 

# reshape data
traf <- data.frame(Occ  = with(traf, c(Occ1, Occ2, Occ3)),
                   Flow = with(traf, c(Flow1, Flow2, Flow3)),
                   lane = rep(1:3, each = nint),
                   day  = rep(day, 3), 
                   time = rep(time, 3))
traf$Speed <- with(traf, Flow/Occ) 

cols1 <- matrix(c("#3d3d3d", "#969696", "#0868ac", "#4eb3d3", "#7d26cd", "#e066ff"), 
               nrow = 3, byrow = TRUE)             # similar colors

cols <- matrix(c("#000000", "#0868ac", "#7d26cd",  # more contrasting colors
                 "#8b7d6b", "#00ced1", "#ee3a8c"), nrow = 3)                             

spRange <- range(traf$Speed, na.rm = TRUE)
ocRange <- range(traf$Occ)
flRange <- range(traf$Flow)

#-------------------------------- plot 1 -------------------------------------#

    par(mfrow = c(2, 1), mar = c(2, 4, 35, 2) + 0.1)
    plot(traf$Flow[traf$lane == 1], type = "l", xaxt = "n", mar = c(5, 4, 4, 2) + 0.1,
         xlab = "time of the day", ylab = "Flow", col = cols[1, 1])
    lines(traf$Flow[traf$lane == 2], col = cols[2, 1])
    lines(traf$Flow[traf$lane == 3], col = cols[3, 1])
#    if (input$time) 
#      axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
#          length.out = nint/(12*3) + 1))
#    if (input$day)
      axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
           labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
  
    par(mar = c(5, 4, 0, 2) + 0.1)
    plot(traf$Occ[traf$lane == 1], type = "l", xaxt = "n", mar = c(3, 3, 2, 3) + 0.1,
         xlab = "time of the day", ylab = "Occupancy", col = cols[1, 1])
    lines(traf$Occ[traf$lane == 2], col = cols[2, 1])
    lines(traf$Occ[traf$lane == 3], col = cols[3, 1])
    legend("topleft", col = cols[, 1], legend = c("left", "middle", "right"), lwd = 2, cex = 0.8)
#    if (input$time) 
      axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
                                                         length.out = nint/(12*3) + 1))
#    if (input$day)
#      axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
#           labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))  
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

#-------------------------------- plot 2 -------------------------------------#

    par(mfrow = c(1, 1))
    plot(Flow   ~ Occ, data = traf[traf$lane == 1, ], cex = 0.7, col = cols[1, 2], 
         xlim = ocRange, ylim = flRange, xlab = "Occupancy", ylab = "Flow",
         main = "Flow vs Occupancy")
    points(Flow ~ Occ, data = traf[traf$lane == 2, ], cex = 0.7, col = cols[2, 2])
    points(Flow ~ Occ, data = traf[traf$lane == 3, ], cex = 0.7, col = cols[3, 2])
    legend("topright", fill = cols[, 2], legend = c("left", "middle", "right"), bty = "n") 
    
    if (input$loess) {
      lines(sort(traf$Occ[traf$lane == 1]), 
            loess(Flow[order(Occ)] ~ sort(Occ), data = traf[traf$lane == 1, ],
                  span = 0.2)$fitted, col = cols[1, 1]) 
      lines(sort(traf$Occ[traf$lane == 2]), 
            loess(Flow[order(Occ)] ~ sort(Occ), data = traf[traf$lane == 2, ],
                  span = 0.2)$fitted, col = cols[2, 1]) 
      lines(sort(traf$Occ[traf$lane == 3]), 
            loess(Flow[order(Occ)] ~ sort(Occ), data = traf[traf$lane == 3, ],
                  span = 0.2)$fitted, col = cols[3, 1]) 
    }
    
#-------------------------------- plot 3 -------------------------------------#
    
myPng <- function(..., width = 7, height = 7, res = 300) {
	png(..., width = width*res, height = height*res, res = res)
}

png("/Users/innars/Documents/Projects/DynDocs/trafficJams/traffic/www/figure/speedHist.png", 
     res = 220, width = 9*220, height = 6*220)
par(mfrow = c(2, 2))
hist(traf$Speed[traf$lane == 1], breaks = 50, xlim = spRange, xlab = "", col = "lightyellow",
     main = "Histogram of V, left lane")
hist(traf$Speed[traf$lane == 2], breaks = 50, xlim = spRange, xlab = "", col = "lightyellow",
     main = "Histogram of V, middle lane")
hist(traf$Speed[traf$lane == 3], breaks = 50, xlim = spRange, xlab = "", col = "lightyellow",
     main = "Histogram of V, right lane")
boxplot(split(traf$Speed, traf$lane), col = cols[, 2], yaxt = "n", xaxt = "n",
        main = "Boxplots of speed in three lanes")
axis(1, at = 1:3, labels = c("left", "middle", "right")) 
dev.off() 
                   
#-------------------------------- plot 4 -------------------------------------#

    plot(Occ   ~ Speed, data = traf[traf$lane == 3, ], xlim = spRange, ylim = ocRange, 
         cex = 0.6, col = cols[3, ][rvals$cong[traf$lane == 3] + 1], 
         xlab = "speed", ylab = "Occupancy", main = "Occupancy vs Speed", xaxt = "n") 
    points(Occ ~ Speed, data = traf[traf$lane == 2, ], xlim = spRange, ylim = ocRange, 
           cex = 0.6, col = cols[2, ][rvals$cong[traf$lane == 2] + 1]) 
    points(Occ ~ Speed, data = traf[traf$lane == 1, ], xlim = spRange, ylim = ocRange,
           cex = 0.6, col = cols[1, ][rvals$cong[traf$lane == 1] + 1])

    points(traf[rvals$clickInd, c("Speed", "Occ")], pch = 16, col = "green")  
    points(rvals$dblclicks, pch = 4, col = "red")
  
    if (length(rvals$dblclicks) >= 4) {
      line <- PointsToLine(rvals$dblclicks[1:2, 1], rvals$dblclicks[1:2, 2])
      abline(a = line[1], b = line[2], col = "red", lty = 2)
    }
    legend("topright", col = t(cols), bty = "o", pch = 16, box.col = "white",
           legend = c("left non-congestion", "left congestion",
                      "middle non-congestion", "middle congestion",
                      "right non-congestion", "right congestion")) 
                      
#-------------------------------- plotZoom -------------------------------------#  

    plot(1:nint, rep(1, nint), type = "n", xaxt = "n", yaxt = "n", ylab = "", 
         xlab = "time of day", main = "Zoom: brush to select time span", 
         col.main = "darkblue")
    mtext("time of day", side = 1, line = 0.6, cex = 0.7)
    suppressWarnings(axis(1, at = seq(0, nint, by = 12*3), tck = 0.06,
                          labels = rep(seq(0, 21, by = 3), length.out = nint/(12*3) + 1),
                          mgp = c(3, -1.7, 0), cex.axis = 0.7))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -2.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
                      
#-------------------------------- plot 5 -------------------------------------#   
x1 <- 450;  x2 <- 1280
y1 <- 0.036; y2 <- 0.15
slope <- (y2 - y1)/(x2 - x1)
icept <- y1 - x1*slope
cong <- traf$Occ >= slope*traf$Speed + icept
rvals <- list(cong = cong)

    par(mfrow = c(3, 1))
    plot(traf$Flow[traf$lane == 1],  pch = c(1, 19)[rvals$cong[traf$lane == 1] + 1], 
         cex = c(0.5, 0.7)[rvals$cong[traf$lane == 1] + 1],
         col = cols[1, ][rvals$cong[traf$lane == 1] + 1],
         xaxt = "n", xlab = "time of day", ylab = "Flow", xlim = rvals$brush)
    points(traf$Flow[traf$lane == 2], pch = c(1, 19)[rvals$cong[traf$lane == 2] + 1], 
           cex = c(0.5, 0.7)[rvals$cong[traf$lane == 2] + 1],
           col = cols[2, ][rvals$cong[traf$lane == 2] + 1])
    points(traf$Flow[traf$lane == 3], pch = c(1, 19)[rvals$cong[traf$lane == 3] + 1], 
           cex = c(0.5, 0.7)[rvals$cong[traf$lane == 3] + 1],
           col = cols[3, ][rvals$cong[traf$lane == 3] + 1])
    points(ifelse(rvals$clickInd %% nint != 0, rvals$clickInd %% nint, nint),
           traf[rvals$clickInd, "Flow"], pch = 21, cex = 1.5, bg = "green", col = 1)  
    axis(1, at = seq(0, nint, by = 12), labels = rep(seq(0, 23, by = 1), 
                                                     length.out = nint/12 + 1))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
    
    plot(traf$Occ[traf$lane == 1],  pch = 19, cex = 0.5, xlim = rvals$brush,
         col = cols[1, ][rvals$cong[traf$lane == 1] + 1],
         xaxt = "n", xlab = "time of day", ylab = "Occupancy")
    points(traf$Occ[traf$lane == 2], pch = 19, cex = 0.5, 
           col = cols[2, ][rvals$cong[traf$lane == 2] + 1])
    points(traf$Occ[traf$lane == 3], pch = 19, cex = 0.5, 
           col = cols[3, ][rvals$cong[traf$lane == 3] + 1])
    points(ifelse(rvals$clickInd %% nint != 0, rvals$clickInd %% nint, nint),
           traf[rvals$clickInd, "Occ"], pch = 23, cex = 1.5, bg = "green")  
    axis(1, at = seq(0, nint, by = 12), labels = rep(seq(0, 23, by = 1), 
                                                     length.out = nint/12 + 1))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
    legend("topleft", col = cols[, 1], legend = c("left", "middle", "right"), 
           lwd = 2)
    
    plot(traf$Speed[traf$lane == 1],  pch = 1, cex = 0.5, xlim = rvals$brush,
         col = cols[1, ][rvals$cong[traf$lane == 1] + 1],
         xaxt = "n", xlab = "time of day", ylab = "Speed", yaxt = "n")
    points(traf$Speed[traf$lane == 2], pch = 1, cex = 0.5, 
           col = cols[2, ][rvals$cong[traf$lane == 2] + 1])
    points(traf$Speed[traf$lane == 3], pch = 1, cex = 0.5, 
           col = cols[3, ][rvals$cong[traf$lane == 3] + 1])
    points(traf$Speed[traf$lane == 1], pch = 1, cex = 0.5, 
           col = cols[1, ][rvals$cong[traf$lane == 1] + 1])
    points(ifelse(rvals$clickInd %% nint != 0, rvals$clickInd %% nint, nint),
           traf[rvals$clickInd, "Speed"], pch = 24, cex = 1.5, bg = "green")  
    axis(1, at = seq(0, nint, by = 12), labels = rep(seq(0, 23, by = 1), 
                                                     length.out = nint/12 + 1))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
                            
#-----------------------------------------------------------------------------#  
#-----------------------------------------------------------------------------#                    

#-----------------------------------------------------------------------------#
#                try some supervized clustering procedure                     #
#-----------------------------------------------------------------------------#

Categorize <- function(x) {
	sum(x[!is.na(x)] < cutoff) <= k
}

flow  <- unlist(traf[, c(2, 4, 6)])
occ   <- unlist(traf[, c(1, 3, 5)])
speed <- c(speed1, speed2, speed3)

flnorm <- (flow  - mean(flow))/sd(flow)
ocnorm <- (occ   - mean(occ))/sd(occ)
spnorm <- (speed - mean(speed, na.rm = TRUE))/sd(speed, na.rm = TRUE)
names(flnorm) <- names(ocnorm) <- names(spnorm) <- as.character(1:nint)

## distance between points
d <- as.matrix(dist(data.frame(spnorm, ocnorm)))
diag(d) <- NA
d[is.na(speed), ] <- d[, is.na(speed)] <- NA
dmin <- apply(d, 1, function(x) {
	          if (all(is.na(x))) return(NA)
	          min(x[x != 0], na.rm = TRUE)
	          })
cutoff <- quantile(dmin, 0.9999, na.rm = TRUE)
cutoff <- max(dmin, na.rm = TRUE)
cutoff <- (quantile(dmin, 0.95, na.rm = TRUE) + quantile(dmin, 0.99, na.rm = TRUE))/2
k <- nint/100
res <- apply(d, 1, Categorize)
res[ocnorm < 0.5 | spnorm > 0]  <- FALSE
res[ocnorm > 1 & spnorm < -1] <- TRUE
plot(spnorm, ocnorm, cex = 0.6, col = c(1, "grey")[res + 1])
cong <- as.data.frame(matrix(res, ncol = 3))
names(cong) <- paste("cong", 1:3, sep = "")


# based on left lane
par(mfrow = c(3, 1))
plot(traf$Flow1,   pch = 16, cex = 0.5, col = c(1, "grey")[cong1 + 1])
points(traf$Flow2, pch = 16, cex = 0.5, col = c("blue", "light blue")[cong1 + 1])
points(traf$Flow3, pch = 16, cex = 0.5, col = c("purple3", "orchid1")[cong1 + 1])

plot(traf$Occ1,   pch = 16, cex = 0.5, col = c(1, "grey")[cong1 + 1])
points(traf$Occ2, pch = 16, cex = 0.5, col = c("blue", "light blue")[cong1 + 1])
points(traf$Occ3, pch = 16, cex = 0.5, col = c("purple3", "orchid1")[cong1 + 1])
legend("topleft", col = c(1, "blue", "purple"), legend = c("left", "middle", "right"), 
       lwd = 2)

plot(speed1,   pch = 16, cex = 0.5, col = c(1, "grey")[cong1 + 1])
points(speed2, pch = 16, cex = 0.5, col = c("blue", "light blue")[cong1 + 1])
points(speed3, pch = 16, cex = 0.5, col = c("purple3", "orchid1")[cong1 + 1])

# based on right lane
par(mfrow = c(3, 1))
plot(traf$Flow1,   pch = 16, cex = 0.5, col = c(1, "grey")[cong3 + 1])
points(traf$Flow2, pch = 16, cex = 0.5, col = c("blue", "light blue")[cong3 + 1])
points(traf$Flow3, pch = 16, cex = 0.5, col = c("purple3", "orchid1")[cong3 + 1])

plot(traf$Occ1,   pch = 16, cex = 0.5, col = c(1, "grey")[cong3 + 1])
points(traf$Occ2, pch = 16, cex = 0.5, col = c("blue", "light blue")[cong3 + 1])
points(traf$Occ3, pch = 16, cex = 0.5, col = c("purple3", "orchid1")[cong3 + 1])
legend("topleft", col = c(1, "blue", "purple"), legend = c("left", "middle", "right"), 
       lwd = 2)

plot(speed1,   pch = 16, cex = 0.5, col = c(1, "grey")[cong3 + 1])
points(speed2, pch = 16, cex = 0.5, col = c("blue", "light blue")[cong3 + 1])
points(speed3, pch = 16, cex = 0.5, col = c("purple3", "orchid1")[cong3 + 1])



#----------------- For Tutorial ---------------------#

N <- 2000n <- 200ls <- sort(sample(1:N,n,replace=FALSE))psi <- function(W) 5*W^3 + 2*sin(4*pi*W^2)W <- sort(runif(N,-1,1))Y <- psi(W) + (rchisq(N, 6) - 4)/sqrt(8)
plot(W, Y, pch = ".")write.table(cbind(W,Y)[ls,],"Q2LS.txt")write.table(cbind(W,Y)[-ls,],"Q2TS.txt")

#----------------- UI ---------------------#

  fluidRow(
    column(width = 6,
           plotOutput("plot1", height = 350,
                      click = "plot1_click",
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        direction = "x"
                      )
           ),
           actionButton("clear", "Clear points"),
           actionButton("exclude_reset", "Reset")
    )
  )
 
fluidPage(  
  fluidRow(
     column(width = 7,
           plotOutput("plot4", height = 450,
                      click = "plot4_click",
                      dblclick = "plot4_dblclick"

           ),
           actionButton("clearP", "Clear points"),
           actionButton("clearL", "Clear line"),
           offset = 2.5
    )
  )
)  
     column(width = 7, 
           plotOutput("plot4", height = 450,
                      click = "plot4_click",
                      dblclick = "plot4_dblclick"

           ),
           radioButtons("clear", "Clear points", choices = "only choice"),
           offset = 2.5, align = "center"
    )
    
#-------------------------------------------------------------------------#

