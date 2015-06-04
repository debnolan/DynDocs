#
# simulate changes to a page based on a given  rate
# then for a given crawl rate, calculate when the
#
# This is written in a non-vectorized manner in an attempt
# to make it clear.
#
#
#   ss = staleSim(.2, 5,10)
#
staleSim =
function(rate, crawlInterval = 2, numIntervals = 200, n = 1000,
          inter.times = rexp(n, rate))
{
     # these are the times we actually perform the crawls to retrieve any updates
   crawlTimes = (1:numIntervals)*crawlInterval

     # we generate a lot of inter-arrival times and then
     # add them together to get the actual times when the
     # changes to the pages are done. Then we throw away
     # the events that occur after our last crawl time.

   changeTimes = cumsum(inter.times)
   changeTimes = changeTimes[changeTimes < max(crawlTimes)]
   inter.times = inter.times[seq(length = length(changeTimes))]

   val = structure(list(changeTimes = changeTimes,
                        crawlTimes = crawlTimes,
                        inter.times = inter.times,
                        rate = rate),
                   class = "StalePageSim")

#   if(!doStale)
#      return(val)
  
   val$staleDurations = computeStaleTimes(crawlTimes, changeTimes)

   val

}

computeStaleTimes =
function(crawlTimes, changeTimes)
{
    # now we are ready to count the amount of time we are stale.
    # We will have an amount of time for each crawl interval.
    # We may have 0 if there is no event in the period
    # and if there is one or more events, then we are stal
    # from when the earliest of these events occurs.
   ans = numeric(length(crawlTimes))

   tmp = c(0, crawlTimes)  
   for(i in 1:length(crawlTimes) + 1) {
         # get the events in this interval
       w = changeTimes < tmp[i] & changeTimes > tmp[i-1]
         # the stale time is the end of the interval minus the earliest event
       ans[i-1] = if(any(w)) tmp[i] - min(changeTimes[w]) else 0
    }
   
   ans
}

plot.StalePageSim =
  # Draw the 
function(x, ..., xlim = c(0, max(x$crawlTimes)))
{
  plot(x$changeTimes, rep(2, length(x$changeTimes)), pch = 5, col = "blue",
        ylim = c(-1,4), xlim = xlim, axes = FALSE,
        xlab = "time", ylab = "events")
  box()
  axis(1)
  axis(2, at = c(1, 2), labels = c("changes", "crawls"))
  points(x$crawlTimes, rep(1, length(x$crawlTimes)))
  abline(h = c(1,2), col = "grey")
  abline(v = x$crawlTimes, col = "red", lty = 3)
}

summary.StalePageSim =
function(object, ...)
{
  cat("change rate: ", object$rate, "\n",
      "number of page changes: ", length(object$changeTimes), "\n",
      "mean stale time: ", mean(object$staleDurations), "\n",
      "number of intervals stale/not-stale: ", sum(object$staleDurations > 0), ", ", sum(object$staleDurations == 0), "\n",
      sep = "")
}
