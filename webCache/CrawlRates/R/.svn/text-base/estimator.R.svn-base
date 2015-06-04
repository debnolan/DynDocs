intervalIndicesToLogical =
function(changeEvents, totalIntervals = max(changeEvents))
{  
    tmp = rep(FALSE, totalIntervals)
    tmp[changeEvents] = TRUE
    tmp
}


mle =
  #
  # Assumes events is a logical vector indicating change detected at that interval or no.
  # Or you can give it a vector of interval lengths via the second argument

  #
  # 1/mle(c(1, 1, 1, 1, 0),  rep(3, 5))$root
  #
  #
function(changeEvents, interval = 12, totalIntervals = length(changeEvents))
{
   if(!all(changeEvents %in% c(TRUE, FALSE)) || !all(changeEvents %in% c(0, 1))) {
       # turn the collection of interval numbers corresponding to an event
       # into  a logical vector.
      if(missing(totalIntervals))
        totalIntervals = max(changeEvents)
      changeEvents = intervalIndicesToLogical(changeEvents, totalIntervals)
   }

   interval = rep(interval, length = length(changeEvents))
   
   if(all(changeEvents == 0))
       return(sum(interval))

   if(all(changeEvents > 0))
       return(min(interval))
   
   noChange = changeEvents == 0

   rhs =  sum(interval[noChange])
# Shouldn't the following work? sum(interval[noChange]) 

   changes = changeEvents[!noChange]

   fun = function(lambda) {
      sum( changes * interval[!noChange] / (exp(lambda * interval[!noChange]) - 1) ) - rhs
   }

   uniroot(fun, lower = 0, upper = 10)$root
}



mle1 =
  #
  # Assumes events is a logical vector indicating change detected at that interval or no.
  #
function(changeEvents, interval = 2)
{
   if(all(changeEvents == 0) || all(changeEvents > 0))
       stop("Cannot compute estimate when homogeneous events")
   
   noChange = changeEvents == 0
   rhs =  sum(noChange * interval)
   changes = changeEvents[!noChange]
   fun = function(lambda) 
        sum( changes * interval / (exp(lambda * changes * interval) - 1) ) - rhs

   uniroot(fun, lower = 0, upper = 10)
}
