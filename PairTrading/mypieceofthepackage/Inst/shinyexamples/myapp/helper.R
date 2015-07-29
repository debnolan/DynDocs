readData =
  function(fileName, dateFormat = c("%Y-%m-%d", "%Y/%m/%d"), ...)
  {
    data = read.csv(fileName, header = TRUE, 
                    stringsAsFactors = FALSE, ...)
    for(fmt in dateFormat) {
      tmp = as.Date(data$Date, fmt)
      if(all(!is.na(tmp))) {
        data$Date = tmp
        break
      }
    }
    
    data[ order(data$Date), ]
  }

combine2Stocks = 
  function(a, b, stockNames = c(deparse(substitute(a)), 
                                deparse(substitute(b))))
  {
    rr = range(intersect(a$Date, b$Date))
    a.sub = a[ a$Date >= rr[1] & a$Date <= rr[2],]
    b.sub = b[ b$Date >= rr[1] & b$Date <= rr[2],]
    structure(data.frame(a.sub$Date, 
                         a.sub$Adj.Close, 
                         b.sub$Adj.Close),
              names = c("Date", stockNames))
  }

plotRatio =
  function(r, k = 1, date = seq(along = r), ...)
  {
    plot(date, r, type = "l", ...)
    abline(h = c(mean(r), 
                 mean(r) + k * sd(r), 
                 mean(r) - k * sd(r)), 
           col = c("darkgreen", rep("red", 2*length(k))), 
           lty = "dashed")
  }

findNextPosition =
  function(ratio, startDay = 1, k = 1, 
           m = mean(ratio), s = sd(ratio))
  {
    up = m + k *s
    down = m - k *s
    
    if(startDay > 1)
      ratio = ratio[ - (1:(startDay-1)) ]
    
    isExtreme = ratio >= up | ratio <= down
    
    if(!any(isExtreme))
      return(integer())
    
    start = which(isExtreme)[1]
    backToNormal = if(ratio[start] > up)
      ratio[ - (1:start) ] <= m
    else
      ratio[ - (1:start) ] >= m
    
    end = if(any(backToNormal))
      which(backToNormal)[1] + start
    else
      length(ratio)
    
    c(start, end) + startDay - 1 
  }

showPosition = 
  function(days, ratios, radius = 100)
  {
    symbols(days, ratios, circles = rep(radius, 2), 
            fg = c("darkgreen", "red"), add = TRUE, inches = FALSE)
  }

getPositions =
  function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
  {
    when = list()
    cur = 1
    
    while(cur < length(ratio)) {
      tmp = findNextPosition(ratio, cur, k, m, s)
      if(length(tmp) == 0)  # done
        break
      when[[length(when) + 1]] = tmp
      if(is.na(tmp[2]) || tmp[2] == length(ratio))
        break
      cur = tmp[2]
    }
    
    when
  }

showPosition = 
  function(days, ratio, radius = 70)
  {
    if(is.list(days))
      days = unlist(days)
    
    points(days, ratio[days], 
            pch=19, cex = 2,
            col = c("darkgreen", "red"))
  }

positionProfit =
  function(pos, stockPriceA, stockPriceB, 
           ratioMean = mean(stockPriceA/stockPriceB), 
           p = .001, byStock = FALSE)
  {
    if(is.list(pos)) {
      ans = sapply(pos, positionProfit, 
                   stockPriceA, stockPriceB, ratioMean, p, byStock)
      if(byStock)
        rownames(ans) = c("A", "B", "commission")
      return(ans)
    }
    priceA = stockPriceA[pos]
    priceB = stockPriceB[pos]
    
    unitsOfA = 1/priceA[1]
    unitsOfB = 1/priceB[1]
    
    amt = c(unitsOfA * priceA[2], unitsOfB * priceB[2])
    
    sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"
    
    profit = if(sellWhat == "A") 
      c((1 - amt[1]),  (amt[2] - 1), - p * sum(amt))
    else 
      c( (1 - amt[2]),  (amt[1] - 1),  - p * sum(amt))
    
    if(byStock)
      profit
    else
      sum(profit)
  }

getProfit.K =
  function(k, x, y, m = mean(x/y), s = sd(x/y))
  {
    pos = getPositions(x/y, k, m = m, s = s)
    if(length(pos) == 0)  
      0
    else
      sum(positionProfit(pos, x, y, m))
  }

getBestK = 
  function(x, y, ks = seq(0.1, max.k, length = N), N = 100, 
           max.k = NA, m = mean(x/y), s = sd(x/y))
  {
    if(is.na(max.k)) {
      r = x/y
      max.k = max(r/sd(r))
    }
    
    pr.k = sapply(ks, getProfit.K, x, y, m = m, s = s)
    median(ks[ pr.k == max(pr.k) ])
  }


att = readData("../Data/ATT.csv")
verizon = readData("../Data/VERIZON.csv")
southwest = readData("../Data/southwest.csv")
united = readData("../Data/united.csv")
hilton = readData("../Data/hilton.csv")
hyatt = readData("../Data/hyatt.csv")
hershey = readData( "../Data/hershey.csv")
kellog = readData("../Data/kellog.csv")
toyota = readData( "../Data/toyota.csv")
inc = readData( "../Data/inc.csv")
ibm = readData( "../Data/ibm.csv")
gm = readData( "../Data/gm.csv")