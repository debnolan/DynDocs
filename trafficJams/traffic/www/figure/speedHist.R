#-------------------------------- plot 3 -------------------------------------#
    
png("speedHist.png", res = 220, width = 9*220, height = 6*220)
par(mfrow = c(2, 2))
hist(traf$Speed[traf$lane == 1], breaks = 50, xlim = spRange, xlab = "", 
     col = "lightyellow", main = "Histogram of V, left lane")
hist(traf$Speed[traf$lane == 2], breaks = 50, xlim = spRange, xlab = "", 
     col = "lightyellow", main = "Histogram of V, middle lane")
hist(traf$Speed[traf$lane == 3], breaks = 50, xlim = spRange, xlab = "", 
     col = "lightyellow", main = "Histogram of V, right lane")
boxplot(split(traf$Speed, traf$lane), col = cols[, 2], yaxt = "n", xaxt = "n",
        main = "Boxplots of speed in three lanes")
axis(1, at = 1:3, labels = c("left", "middle", "right")) 
dev.off() 
