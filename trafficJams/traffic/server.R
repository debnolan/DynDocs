library(shiny)
source("helper.R")

#load("../data/flow_occ.RData")
data(flow_occ)

nint <- nrow(traf)/max(traf$lane)  # number of 5-min intervals, 1740
spRange <- range(traf$Speed, na.rm = TRUE)
ocRange <- range(traf$Occ)
flRange <- range(traf$Flow)

cols <- matrix(c("#000000", "#0868ac", "#7d26cd", 
                 "#8b7d6b", "#00ced1", "#ee3a8c"), nrow = 3)   
col.pt <- "lawngreen"

#-----------------------------------------------------------------------------#

server <- function(input, output) {

  rvals <- reactiveValues(
    clickInd  = NULL,  
    dblclicks = cbind(spRange, ocRange),  #NULL,
    brush     = c(0, nint),
    cong      = PointsToCongestion(spRange, ocRange, traf$Occ, traf$Speed)
  )    
  
  output$plot1 <- renderPlot({
    par(mfrow = c(2, 1), mar = c(2, 4, 3, 2) + 0.1)
    plot(traf$Flow[traf$lane == 1], type = "l", xaxt = "n", 
         xlab = "", ylab = "Flow", col = cols[1, 1])
    lines(traf$Flow[traf$lane == 2], col = cols[2, 1])
    lines(traf$Flow[traf$lane == 3], col = cols[3, 1])
    if (input$time) 
      axis(1, at = seq(0, nint, by = 12*3), labels = rep("", nint/(12*3) + 1), tck = -0.03) 
    if (input$day)
      axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.5,
           labels = unique(traf$day)[1:6])
    
    par(mar = c(5, 4, 0, 2) + 0.1)
    plot(traf$Occ[traf$lane == 1], type = "l", xaxt = "n", 
         xlab = "time of the day", ylab = "Occupancy", col = cols[1, 1])
    lines(traf$Occ[traf$lane == 2], col = cols[2, 1])
    lines(traf$Occ[traf$lane == 3], col = cols[3, 1])
    legend("topleft", col = cols[, 1], legend = c("left", "middle", "right"), 
           lwd = 2, cex = 0.8)
    if (input$time) 
      axis(1, at = seq(0, nint, by = 12*3), labels = rep(seq(0, 21, by = 3), 
                                                         length.out = nint/(12*3) + 1))
  })
  
  output$plot2 <- renderPlot({
    par(mfrow = c(1, 1))
    plot(Flow   ~ Occ, data = traf[traf$lane == 3, ], cex = 0.7, col = cols[3, 1], 
         xlim = ocRange, ylim = flRange, xlab = "Occupancy", ylab = "Flow",
         main = "Flow vs Occupancy")
    points(Flow ~ Occ, data = traf[traf$lane == 2, ], cex = 0.7, col = cols[2, 1])
    points(Flow ~ Occ, data = traf[traf$lane == 1, ], cex = 0.7, col = cols[1, 1])
    legend("topright", fill = cols[, 1], legend = c("left", "middle", "right"), bty = "n") 
    
    if (input$loess) {
      lines(sort(traf$Occ[traf$lane == 3]), 
            loess(Flow[order(Occ)] ~ sort(Occ), data = traf[traf$lane == 3, ],
                  span = 0.2)$fitted, col = cols[3, 2]) 
      lines(sort(traf$Occ[traf$lane == 2]), 
            loess(Flow[order(Occ)] ~ sort(Occ), data = traf[traf$lane == 2, ],
                  span = 0.2)$fitted, col = cols[2, 2]) 
      lines(sort(traf$Occ[traf$lane == 1]), 
            loess(Flow[order(Occ)] ~ sort(Occ), data = traf[traf$lane == 1, ],
                  span = 0.2)$fitted, col = cols[1, 2]) 
    }
  })    
  
  output$plot4 <- renderPlot({
    plot(Occ   ~ Speed, data = traf[traf$lane == 3, ], xlim = spRange, ylim = ocRange, 
         cex = 0.6, col = cols[3, ][rvals$cong[traf$lane == 3] + 1], 
         xlab = "speed", ylab = "Occupancy", main = "Occupancy vs Speed", xaxt = "n") 
    points(Occ ~ Speed, data = traf[traf$lane == 2, ], xlim = spRange, ylim = ocRange, 
           cex = 0.6, col = cols[2, ][rvals$cong[traf$lane == 2] + 1]) 
    points(Occ ~ Speed, data = traf[traf$lane == 1, ], xlim = spRange, ylim = ocRange,
           cex = 0.6, col = cols[1, ][rvals$cong[traf$lane == 1] + 1])

    points(traf[rvals$clickInd, c("Speed", "Occ")], pch = 21, cex = 0.9, bg = col.pt)  
    points(rvals$dblclicks, pch = 4, col = "red")
  
    if (length(rvals$dblclicks) >= 4) {
      line <- PointsToLine(rvals$dblclicks[1:2, 1], rvals$dblclicks[1:2, 2])
      abline(a = line[1], b = line[2], col = "red", lty = 2)
    }
    legend("topright", col = t(cols), bty = "o", pch = 19, box.col = "white",
           legend = c("left non-congestion", "left congestion",
                      "middle non-congestion", "middle congestion",
                      "right non-congestion", "right congestion")) 
  })
    
  output$plotZoom <- renderPlot({
    par(mar = c(3, 2.5, 3, 1.35) + 0.1)
    plot(1:nint, rep(1, nint), type = "n", xaxt = "n", yaxt = "n", ylab = "", 
         xlab = "", main = "Zoom: brush to select time span", col.main = "darkblue")
    mtext("time of day", side = 1, line = 0.6, cex = 1)
    suppressWarnings(axis(1, at = seq(0, nint, by = 12*3), tck = 0.06,
                          labels = rep(seq(0, 21, by = 3), length.out = nint/(12*3) + 1),
                          mgp = c(3, -1.7, 0), cex.axis = 0.7))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -2.5, cex.axis = 1.2,
         labels = unique(traf$day)[1:6])
  })

  output$plot5 <- renderPlot({
    par(mfrow = c(3, 1), mar = c(1, 4, 3, 2) + 0.1)
    plot(traf$Flow[traf$lane == 3],  pch = c(1, 19)[rvals$cong[traf$lane == 3] + 1], 
         cex = c(0.6, 0.7)[rvals$cong[traf$lane == 3] + 1],
         col = cols[3, ][rvals$cong[traf$lane == 3] + 1],
         xaxt = "n", xlab = "", ylab = "Flow", ylim = flRange, xlim = rvals$brush)
    points(traf$Flow[traf$lane == 2], pch = c(1, 19)[rvals$cong[traf$lane == 2] + 1], 
           cex = c(0.6, 0.7)[rvals$cong[traf$lane == 2] + 1],
           col = cols[2, ][rvals$cong[traf$lane == 2] + 1])
    points(traf$Flow[traf$lane == 1], pch = c(1, 19)[rvals$cong[traf$lane == 1] + 1], 
           cex = c(0.6, 0.7)[rvals$cong[traf$lane == 1] + 1],
           col = cols[1, ][rvals$cong[traf$lane == 1] + 1])
    points(ifelse(rvals$clickInd %% nint != 0, rvals$clickInd %% nint, nint),
           traf[rvals$clickInd, "Flow"], pch = 21, cex = 1.5, bg = col.pt, col = 1)  
    axis(1, at = seq(0, nint, by = 12), labels = rep(seq(0, 23, by = 1), 
                                                     length.out = nint/12 + 1))
    axis(3, at = seq(24*12/2, nint, by = 24*12), tick = FALSE, line = -0.3, cex.axis = 1.8,
         labels = unique(traf$day)[1:6])
    
    par(mar = c(2.5, 4, 1.5, 2) + 0.1)
    plot(traf$Occ[traf$lane == 3],  pch = c(1, 19)[rvals$cong[traf$lane == 3] + 1], 
         cex = c(0.6, 0.7)[rvals$cong[traf$lane == 3] + 1],
         col = cols[3, ][rvals$cong[traf$lane == 3] + 1],
         xaxt = "n", xlab = "", ylab = "Occupancy", ylim = ocRange, xlim = rvals$brush)
    points(traf$Occ[traf$lane == 2], pch = c(1, 19)[rvals$cong[traf$lane == 2] + 1], 
           cex = c(0.6, 0.7)[rvals$cong[traf$lane == 2] + 1],
           col = cols[2, ][rvals$cong[traf$lane == 2] + 1])
    points(traf$Occ[traf$lane == 1], pch = c(1, 19)[rvals$cong[traf$lane == 1] + 1], 
           cex = c(0.6, 0.7)[rvals$cong[traf$lane == 1] + 1],
           col = cols[1, ][rvals$cong[traf$lane == 1] + 1])
    points(ifelse(rvals$clickInd %% nint != 0, rvals$clickInd %% nint, nint),
           traf[rvals$clickInd, "Occ"], pch = 21, cex = 1.5, bg = col.pt)  
    axis(1, at = seq(0, nint, by = 12), labels = rep(seq(0, 23, by = 1), 
                                                     length.out = nint/12 + 1))
    legend("topleft", col = t(cols), pch = rep(c(1, 19), 3), 
           legend = c("left non-cong.", "left cong.",
                      "middle non-cong.", "middle cong.",
                      "right non-cong.", "right cong.")) 
   
    par(mar = c(4, 4, 0, 2) + 0.1)
    plot(traf$Speed[traf$lane == 2],  pch = c(1, 19)[rvals$cong[traf$lane == 2] + 1], 
         cex = c(0.6, 0.7)[rvals$cong[traf$lane == 2] + 1],
         col = cols[2, ][rvals$cong[traf$lane == 2] + 1],
         xaxt = "n", xlab = "", ylab = "Speed", ylim = spRange, xlim = rvals$brush)
    points(traf$Speed[traf$lane == 3], pch = c(1, 19)[rvals$cong[traf$lane == 3] + 1], 
           cex = c(0.6, 0.7)[rvals$cong[traf$lane == 3] + 1],
           col = cols[3, ][rvals$cong[traf$lane == 3] + 1])
    points(traf$Speed[traf$lane == 1], pch = c(1, 19)[rvals$cong[traf$lane == 1] + 1], 
           cex = c(0.6, 0.7)[rvals$cong[traf$lane == 1] + 1],
           col = cols[1, ][rvals$cong[traf$lane == 1] + 1])
    points(ifelse(rvals$clickInd %% nint != 0, rvals$clickInd %% nint, nint),
           traf[rvals$clickInd, "Speed"], pch = 21, cex = 1.5, bg = col.pt)  
    axis(1, at = seq(0, nint, by = 12), labels = rep(seq(0, 23, by = 1), 
                                                     length.out = nint/12 + 1))
  })
  
  observeEvent(input$plot4_click, {
    newClick <- unlist(input$plot4_click[c("x", "y")])
    newInd   <- nearPointIndex(traf[c("Speed", "Occ")], newClick)
    rvals$clickInd <- c(rvals$clickInd, newInd)  
  })
  
  observeEvent(input$plot4_dblclick, {
    newDblclick <- unlist(input$plot4_dblclick[c("x", "y")])
    if (length(rvals$dblclicks) == 6) 
      rvals$dblclicks <- rvals$dblclicks[3, ]
    rvals$dblclicks <- rbind(rvals$dblclicks, newDblclick)  
    if (length(rvals$dblclicks) >= 4) 
      rvals$cong <- PointsToCongestion(rvals$dblclicks[1:2, 1], rvals$dblclicks[1:2, 2],
                                       traf$Occ, traf$Speed)  
  })

  observe({
    brush <- input$plotZoom_brush
    if (is.null(brush)) 
      rvals$brush <- c(0, nint)
    else 
      rvals$brush <- c(brush$xmin, brush$xmax)
  })
  
  observeEvent(input$clearP, {
    rvals$clickInd <- NULL
  })
  
  observeEvent(input$clearL, {
    rvals$dblclicks <- NULL
    rvals$cong <- rep(FALSE, nrow(traf))
  })
  
}
