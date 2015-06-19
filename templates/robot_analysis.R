setwd("~/DynDocs/RobotExpr")
load("logs.rda")

# Get a specific  measurement made during the robot’s data collection,
# which we will call a “look.”
look <- logs[[1]][nrow(logs[[1]]), ]

#Get a contiguous segment from the look.
#segs is a list with two vectors, and each vector gives the 
#indices of the elements of the range values for that segment/arc.
segs <- getWrappedSegments(as.numeric(look[1, -(1:3)]))

#Using segs, we can comput xi and yi values for the segment.
i = segs[[2]]
range = as.numeric(look[, -(1:3)])[i]
theta = seq(0, 2*pi, length = 360) - pi/2
xi = look$x + range * cos(theta[i])
yi = look$y + range * sin(theta[i])

plotLook <-
  function(row, ...)
  {
    x = row[1, "x"]
    y = row[1, "y"]
    
    theta = seq(0, 2*pi, length = 360) - pi/2
    r = as.numeric(row[1, -c(1:3, 365)])
    x1 = x + r*cos(theta)
    y1 = y + r*sin(theta)
    par(pty = 's')
    plot(x + 2*cos(theta), y + 2*sin(theta), 
         col = "dark green", type = "l", lty = 2, 
         xlab = "x", ylab = "y", ...)    # 2 meter circle 
    points(x1, y1, type = "l", col = "purple")           # what the robot sees
}

plotLook(look)


#Sample Final Look
plotLook(look)
#This is the path/shape seen by the robot in the final look 
#of the first log file, JRSPdata_2010_03_10_12_12_31. 
#The robot is in the center of the circle. 
#At the top right of the circle, we see a circular-like object 
#that might be the target. A straight edge corresponding to a rectangular 
#obstacle appears at the bottom of the circle.

#To find the target, we are looking for a reasonably short sub-arc/segment 
#of the 360 degree view, not the entire 360 degrees.
getSegments <-
  # return a list with elements being integer vectors
  # giving the indices of each contiguous segment
  # with values less than the threshold.
  #
  # We discard the 361st element.
  function(range, threshold = 2)
  {
    if(is.data.frame(range)) 
      range = as.numeric(range[1, -(1:3)])
    
    if(length(range) == 361)
      range = range[-361]
    
    rl = rle(range < threshold)
    
    cur = 1L
    ans = list()
    for(i in seq(along = rl$lengths)) {
      if(!rl$values[i]) {
        cur = cur + rl$lengths[i]
        next
      }
      ans[[length(ans) + 1L]] = seq(cur, length = rl$lengths[i])
      cur = cur + rl$lengths[i]
    }
    
    ans
  }

#To overcome this issue of a segment “wrapping” around from 360 degrees to 0 degrees, 
#the pieces directly behind the robot need to be combined by being laid end-to-end. 
getWrappedSegments =
  function(range, threshold = 2,
           segments = getSegments(range, threshold),
           byDist = FALSE, ...)  # was byDist = TRUE
  {
    force(byDist)
    if(is.data.frame(range)) {
      look = range
      range = as.numeric(range[ - c(1:3, 364)])
    } 
    
    if(length(segments) > 1) {
      s1 = segments[[1]]
      s2 = segments[[length(segments)]]
      if(s1 == 1L && s2[length(s2)] == 360) {
        segments[[1]] = c(s2, s1)
        segments = segments[-length(segments)]
      }
    }
    
    if(byDist)
      separateSegmentByDist(look, segs = segments, ...)
    else
      segments
  }

################################### Objective ################################
#We can compare the shape defined by each segment to an arc/part of a circle.
#Essentially, we want to allow x0,y0, and r to vary    
#so that we find the best fit for our points as a circle. 
############################################################################

#Implement the least squares function that get minimized
circle.fit.nlm.funk <-
  function (p, x, y) 
  {
    x0 <- p[1]
    y0 <- p[2]
    r <- p[3]
    actual.r <- sqrt((x - x0)^2 + (y - y0)^2)
    sum((r - actual.r)^2)
  }

#We’ll use the very simple approach of using the average of the xi values 
#and the yi values in the arc as our initial guess.
x0 = mean(xi)
y0 = mean(yi)

nlm(circle.fit.nlm.funk, c(x0 = mean(xi), y0 = mean(yi), r = .5),
    x = xi, y = yi)

#$minimum
#[1] 0.003253126

#$estimate
#[1] 13.8686765 -5.9110040  0.5387053

#$iterations
#[1] 17

circle.fit <-
  function (x, y, initGuess = c(mean(x), mean(y), .5), ...) 
    nlm(circle.fit.nlm.funk, initGuess, x = x, y = y, ...)

#To process the entire look
robot.evaluation <-
  function(look, min.length = 3, max.ss.ratio = 0.01, 
           min.radius = .5, max.radius = 2,
           range.threshold = 2,
           segs = getWrappedSegments(range, range.threshold),
           ...)
  {
    x = look$x
    y = look$y
    range = as.numeric(look[, -(1:3)])
    theta = seq(0, 2*pi, length = 360)
    
    for(s in segs) {
      if(length(s) < min.length)
        next  
      
      xi = x + cos(theta[s]) * range[s]
      yi = y + sin(theta[s]) * range[s]
      out = circle.fit(xi, yi, ...)
      
      if(out$code > 3)  
        next
      
      if((out$minimum/length(s)) > max.ss.ratio)
        next
      
      if(abs(out$estimate[3]) < min.radius  
         || abs(out$estimate[3]) > max.radius) 
        next
      
      return(list(x = xi, y = yi, range = range[s], 
                  robot = c(x, y), fit = out))
    }
  }

#Examine robot.evaluation() performs on the last look of each of the 100 log files:
finalLooks = lapply(logs, function(x) x[nrow(x),])
circs = lapply(finalLooks, robot.evaluation)

unname(which(sapply(circs, length) > 0))

par(mfrow = c(7, 6), mar = rep(0, 4), pty = 's')
invisible(lapply(finalLooks[sapply(circs, length) > 0],
                 plotLook, axes = FALSE))
