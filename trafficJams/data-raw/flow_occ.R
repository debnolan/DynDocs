traf <- read.csv("flow_occ.txt")
nint <- nrow(traf) # number of 5-min intervals, 1740

## Suppose starts on March 14th, 2003, Friday at midnight (we don't know the time)
## then ends at 1am on Thur, March 20th
day <- rep(c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", 
             "Wednesday", "Thursday"), 
           each = 24*12, length.out = nint)
time <- paste(rep(0:23, each = 12, length.out = nint), 
              rep(c("00", "05", seq(10, 55, by = 5)), length.out =    nint), 
              sep = ":") 

## reshape data
traf <- data.frame(Occ  = with(traf, c(Occ1, Occ2, Occ3)),
                   Flow = with(traf, c(Flow1, Flow2, Flow3)),
                   lane = rep(1:3, each = nint),
                   day  = rep(day, 3), 
                   time = rep(time, 3))
traf$Speed <- with(traf, Flow/Occ) 
save(traf, file = "flow_occ.RData")
