#' Traffic measurements by a loop detector in a particluar location on I-80 near Sacramento
#' March 14 - 20, 2003.
#' 
#' A dataset containing Flow and Occupancy in three lanes of the freeway recorded in 
#' 1740 successive five minutes intervals.
#' 
#' @format A data frame named "traf" with 5220 rows and 6 variables:
#' \describe{
#'  \item{Occ}{occupancy - percentage of time there was a car over the loop}
#'  \item{Flow}{number of cars passing over a detector in a time interval}
#'  \item{lane}{1: leftmost, 2: middle, 3: rightmost}
#'  \item{day}{day of the week}
#'  \item{time}{time of the day, in hours: 0 to 23}
#'  \item{Speed}{ratio of Flow to Occupancy}
#' }
#' @source \url{http://www.stat.berkeley.edu/~rice/UCLA/flow-occ-table.txt}
"flow_occ"