
nearPointIndex <- function(data, point, threshold = 0.1) { 
  # version of nearPoints() function (to reduce time)
  # finds the index (of a data row) of a nearest point
  # data: matrix with 2 cols or a data frame
  # point: coordinates of a point 
  # threshold: proportion of the range to be included
  
  scaled <- scale(rbind(point, data))
  data   <- scaled[-1, ]
  point  <- scaled[1, ]
  rangeX <- point[1] + c(-1, 1)*diff(range(data[, 1], na.rm = TRUE))*threshold
  rangeY <- point[2] + c(-1, 1)*diff(range(data[, 2], na.rm = TRUE))*threshold
  close <- which(rangeX[1] < data[, 1] & data[, 1] < rangeX[2] &
                   rangeY[1] < data[, 2] & data[, 2] < rangeY[2])
  distclose <- sqrt((data[close, 1] - point[1])^2 + (data[close, 2] - point[2])^2)               
  return(close[which.min(distclose)])
}

PointsToLine <- function(x, y) {
  # x, y: coordinates of two points (vectors of length 2)
  if (x[1] == x[2]) {                   # can happen - "click" resolution
    x[1] <- x[1] + x[1]*1e-6
  }
  slope <- (y[2] - y[1])/(x[2] - x[1])
  icept <- y[1] - x[1]*slope
  return(c(icept = icept, slope = slope))
}

PointsToCongestion <- function(x, y, occ, speed) {
  # x, y: coordinates of two points (vectors of length 2)
  line <- PointsToLine(x, y)
  cong <- occ >= line[2]*speed + line[1] # glitches if subset by name
  return(cong)
}
