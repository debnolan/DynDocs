ff <- list.files("logs", full.names = TRUE, 
                 pattern = "JRSPdata.*\\.log")
readLog <- 
  function(filename = "logs/JRSPdata_2010_03_10_12_12_31.log",
           lines = readLines(filename))
  {
    lines = grep("^#", lines, invert = TRUE, value = TRUE)
    els = strsplit(lines, "[[:space:]]+")
    
    # Get the interface and type so we can subset.
    iface = sapply(els, `[`, 4)
    type = sapply(els, `[`, 6)
    
    # find the indices corresponding to a position2d 
    # with a laser immediately after.
    i = which(iface == "position2d" & type == "001")
    i = i[ iface[i+1] == "laser" & type[i+1] == "001"]
    
    # Get the time, x, y, and then the range values 
    # from the laser below.
    locations = t(sapply(els[i], `[`, c(1, 8, 9)))
    ranges = t(sapply(els[i + 1], `[`, 
                      seq(14, by = 2, length = 361) ))
    
    # now combine these into a data frame
    locations = as.data.frame(lapply(1:ncol(locations),
                                     function(i) 
                                       as.numeric(locations[, i])))
    ranges = as.data.frame(lapply(1:ncol(ranges), 
                                  function(i) 
                                    as.numeric(ranges[, i]))) 
    
    ans = cbind(locations, ranges)
    names(ans) = c("time", "x", "y", 
                   sprintf("range%d", 1:ncol(ranges)))    
    
    invisible(ans)
  }

system.time(logs <- lapply(ff, readLog))
names(logs) <- ff

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

circle.fit.nlm.funk <-
  function (p, x, y) 
  {
    x0 <- p[1]
    y0 <- p[2]
    r <- p[3]
    actual.r <- sqrt((x - x0)^2 + (y - y0)^2)
    sum((r - actual.r)^2)
  }

look <- logs[[1]][nrow(logs[[1]]), ]

segs <- getWrappedSegments(as.numeric(look[1, -(1:3)]))

i = segs[[2]]
range = as.numeric(look[, -(1:3)])[i]
theta = seq(0, 2*pi, length = 360) - pi/2
xi = look$x + range * cos(theta[i])
yi = look$y + range * sin(theta[i])

plot(xi, yi, type = "l")

nlm(circle.fit.nlm.funk, c(x0 = mean(xi), y0 = mean(yi), r = .5),
    x = xi, y = yi)

circle.fit <-
  function (x, y, initGuess = c(mean(x), mean(y), .5), ...) 
    nlm(circle.fit.nlm.funk, initGuess, x = x, y = y, ...)
