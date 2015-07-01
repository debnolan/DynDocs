library(shiny)

traf <- read.csv("http://www.stat.berkeley.edu/~rice/UCLA/flow-occ-table.txt")
nint <- nrow(traf) # number of 5-min intervals, 1740
traf$day <- rep(c("Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thur"), 
                each = 24*12, length.out = nint)
traf$time <- paste(rep(0:23, each = 12, length.out = nint), 
                   rep(c("00", "05", seq(10, 55, by = 5)), length.out =    nint), sep = ":") 
cols <- matrix(c("#3d3d3d", "#969696", "#0868ac", "#4eb3d3", "#7d26cd", "#e066ff"), nrow = 3, byrow = TRUE)

speed1 <- with(traf, Flow1/Occ1) 
speed2 <- with(traf, Flow2/Occ2) 
speed3 <- with(traf, Flow3/Occ3)  

spRange <- range(c(speed1, speed2, speed3), na.rm = TRUE)
ocRange <- range(traf[, c(1, 3, 5)])
flRange <- range(traf[, c(2, 4, 6)])

#--------------------------------------------------------------------------#
#   This should go into a separate .R file to source (if at all)           #
#--------------------------------------------------------------------------#

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

d <- as.matrix(dist(data.frame(spnorm, ocnorm)))
diag(d) <- NA
d[is.na(speed), ] <- d[, is.na(speed)] <- NA
dmin <- apply(d, 1, function(x) {
  if (all(is.na(x))) return(NA)
  min(x[x != 0], na.rm = TRUE)
})
cutoff <- quantile(dmin, 0.9999, na.rm = TRUE)
k <- nint/100
res <- apply(d, 1, Categorize)
res[ocnorm < 0.5 | spnorm > 0]  <- FALSE
res[ocnorm > 1 & spnorm < -1] <- TRUE
cong <- as.data.frame(matrix(res, ncol = 3))

#--------------------------------------------------------------------------#

shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    par(mfrow = c(2, 1))
    plot(traf$Flow1, type = "l", xaxt = "n", xlab = "time of the day", ylab = "Flow", col = cols[1, 1])
    lines(traf$Flow2, col = cols[2, 1])
    lines(traf$Flow3, col = cols[3, 1])
    if (input$time) 
      axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
          length.out = nint/(12*3) + 1))
    if (input$day)
      axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
           labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
  
    plot(traf$Occ1, type = "l", xaxt = "n", xlab = "time of the day", ylab = "Occupancy", col = cols[1, 1])
    lines(traf$Occ2, col = cols[2, 1])
    lines(traf$Occ3, col = cols[3, 1])
    legend("topleft", col = cols[, 1], legend = c("left", "middle", "right"), lwd = 2, cex = 0.8)
    if (input$time) 
      axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
                                                         length.out = nint/(12*3) + 1))
    if (input$day)
      axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
           labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))    
  })
  
  output$plot2 <- renderPlot({
    par(mfrow = c(1, 1))
    plot(traf$Occ1, traf$Flow1, cex = 0.7, col = cols[1, 2], 
         xlim = ocRange, ylim = flRange, xlab = "Occupancy", ylab = "Flow",
         main = "Flow vs Occupancy")
    points(traf$Occ2, traf$Flow2, cex = 0.7, col = cols[2, 2])
    points(traf$Occ3, traf$Flow3, cex = 0.7, col = cols[3, 2])
    legend("topright", fill = cols[, 2], legend = c("left", "middle", "right"), bty = "n") 
    
    if (input$loess) {
      lines(sort(traf$Occ1), loess(Flow1 ~ Occ1, data = traf[order(traf$Occ1), ],
                                 span = 0.2)$fitted, col = cols[1, 1]) 
      lines(sort(traf$Occ2), loess(Flow2 ~ Occ2, data = traf[order(traf$Occ2), ],
                                 span = 0.2)$fitted, col = cols[2, 1])
      lines(sort(traf$Occ3), loess(Flow3 ~ Occ3, data = traf[order(traf$Occ3), ], 
                                 span = 0.2)$fitted, col = cols[3, 1])
    }
  })    
  
  output$plot5 <- renderPlot({
    par(mfrow = c(3, 1))
    whatever <- input$drange
    plot(traf$Flow1,   pch = 16, cex = 0.5, col = cols[1, ][cong[, 1] + 1], 
         xaxt = "n", xlab = "time of day", ylab = "Flow")
    points(traf$Flow2, pch = 16, cex = 0.5, col = cols[2, ][cong[, 2] + 1])
    points(traf$Flow3, pch = 16, cex = 0.5, col = cols[3, ][cong[, 3] + 1])
    axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
                                                       length.out = nint/(12*3) + 1))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
    
    plot(traf$Occ1,   pch = 16, cex = 0.5, col = cols[1, ][cong[, 1] + 1],
         xaxt = "n", xlab = "time of day", ylab = "Occupancy")
    points(traf$Occ2, pch = 16, cex = 0.5, col = cols[2, ][cong[, 2] + 1])
    points(traf$Occ3, pch = 16, cex = 0.5, col = cols[3, ][cong[, 3] + 1])
    legend("topleft", col = cols[, 1], legend = c("left", "middle", "right"), 
           lwd = 2)
    axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
                                                       length.out = nint/(12*3) + 1))       
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))
    
    plot(speed1,   pch = 16, cex = 0.5, col = cols[1, ][cong[, 1] + 1],
         xaxt = "n", xlab = "time of day", ylab = "Speed", yaxt = "n")
    points(speed2, pch = 16, cex = 0.5, col = cols[2, ][cong[, 2] + 1])
    points(speed3, pch = 16, cex = 0.5, col = cols[3, ][cong[, 3] + 1])
    points(speed1,   pch = 16, cex = 0.5, col = cols[1, ][cong[, 1] + 1])
    axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
                                                       length.out = nint/(12*3) + 1))  
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
         labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"))       
  })
    
      
  output$summary <- renderPrint({
    summary(data())
  })
  
})
