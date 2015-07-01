
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

att = readData("ATT.CSV")         # ATT symbol
verizon = readData("VERIZON.CSV") # VZ symbol



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


overlap = combine2Stocks(att, verizon)


r = overlap$att/overlap$verizon

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

#plotRatio(r)


findNextPosition =
  # e.g.,  findNextPosition(r)
  #        findNextPosition(r, 1174)
  # Check they are increasing and correctly offset
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
#findNextPosition( r, 1)




showPosition = 
  function(days, ratios, radius = 100)
  {
    symbols(days, ratios, circles = rep(radius, 2), 
            fg = c("darkgreen", "red"), add = TRUE, inches = FALSE)
  }

#showPosition( day = c(1,1927), ratio=c(0.626, 0.811))
#Plot all open and close pts:
#  pos = getPositions( r, k)
#  k=.85
#  plotRatio( r, k ,col = "lightgray", ylab = "ratio")
#  showPosition(pos,r)

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
    
    symbols(days, ratio[days], 
            circles = rep(radius, length(days)), 
            fg = c("darkgreen", "red"),
            add = TRUE, inches = FALSE)
  }

#getPositions( r )


positionProfit =
  #  r = overlap$att/overlap$verizon
  #  k = 1.7
  #  pos = getPositions(r, k)
  #  positionProfit(pos[[1]], overlap$att, overlap$verizon)
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

#prof = positionProfit(pos, overlap$att, overlap$verizon, mean(r))



i = 1:floor(nrow(overlap)/2)
train = overlap[i, ]
test = overlap[ - i, ]

r.train = train$att/train$verizon
r.test = test$att/test$verizon

k.max = max((r.train - mean(r.train))/sd(r.train))

k.min = min((abs(r.train - mean(r.train))/sd(r.train)))

ks = seq(k.min, k.max, length = 1000)
m  = mean(r.train)

#profits =
#  sapply(ks,
#         function(k) {
#           pos = getPositions(r.train, k)
#           sum(positionProfit(pos, train$att, train$verizon, 
#                              mean(r.train)))
#         })
#
#ks[  profits == max(profits) ]  
#
#tmp.k = ks[  profits == max(profits) ]  
#pos = getPositions(r.train, tmp.k[1])
#all(sapply(tmp.k[-1],
#           function(k) 
#             identical(pos, getPositions(r.train, k))))
#
#k.star = mean(ks[  profits == max(profits) ]  )
#
#pos = getPositions(r.test, k.star, mean(r.train), sd(r.train))
#testProfit = sum(positionProfit(pos, test$att, test$verizon))   



getProfit.K =
  function(k, x, y, m = mean(x/y), s = sd(x/y))
  {
    pos = getPositions(x/y, k, m = m, s = s)
    if(length(pos) == 0)  
      0
    else
      sum(positionProfit(pos, x, y, m))
  }
#getProfit.K( k = 0.28392, x= overlap$att, y=overlap$verizon) = 3.142893



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
#getBestK( x=overlap$att, y=overlap$verizon)




#Another example::

#All stock selections:
southwest = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/southwest.csv")
united = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/united.csv")
hilton = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/hilton.csv")
hyatt = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/hyatt.csv")
kellog = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/kellog.csv")
hershey = readData("http://real-chart.finance.yahoo.com/table.csv?s=HSY&d=2&e=13&f=2015&g=d&a=6&b=1&c=1985&ignore=.csv")
toyota = readData( "http://real-chart.finance.yahoo.com/table.csv?s=TM&d=2&e=13&f=2015&g=d&a=7&b=18&c=1976&ignore=.cs")
inc = readData( "http://real-chart.finance.yahoo.com/table.csv?s=INTC&amp;d=5&amp;e=6&amp;f=2015&amp;g=d&amp;a=2&amp;b=17&amp;c=1980&amp;ignore=.csv")
ibm = readData( "http://real-chart.finance.yahoo.com/table.csv?s=IBM&amp;d=5&amp;e=9&amp;f=2015&amp;g=d&amp;a=0&amp;b=2&amp;c=1962&amp;ignore=.csv")
gm = readData( "http://real-chart.finance.yahoo.com/table.csv?s=GM&amp;d=5&amp;e=9&amp;f=2015&amp;g=d&amp;a=10&amp;b=18&amp;c=2010&amp;ignore=.csv")

sample = combine2Stocks(toyota, southwest)

q = sample$toyota/sample$southwest
plotRatio(q)
q[1]
q[556]
showPosition( days= c(1,556), ratio = c(86.0655,6.982537))
# to plot all positions:
pos = getPositions(q)
k = 1
plotRatio( q, k ,col = "lightgray", ylab = "ratio")
showPosition(pos,q)


i = 1:floor(nrow(sample)/2)
train = sample[i, ]
test = sample[ - i, ]
q.train = train$toyota/train$southwest
q.test = test$toyota/test$southwest

k.max = max((q.train - mean(q.train))/sd(q.train))

k.min = min((abs(q.train - mean(q.train))/sd(q.train)))

ks = seq(k.min, k.max, length = 1000)
m  = mean(q.train)

getBestK( x=sample$toyota, y=sample$southwest)

getProfit.K( k = 0.5403035, x = sample$toyota, y = sample$southwest)
#profit with best K= 6.372544

getProfit.K( k = 0.28392, x = sample$toyota, y = sample$southwest)
#this profit with att/verizon best K= 3.490972