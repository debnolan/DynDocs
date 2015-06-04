\name{staleSim}
\alias{staleSim}
\alias{plot.StalePageSim}
\alias{summary.StalePageSim}
\title{Simulate changes to web pages and calculate statistics about
  stale pages}

\description{

  This function can be used to simulate changes to Web pages using a
  Poisson model and then to examine the distribution of the amount of
  time a page is stale in a search engine for different crawl rates.
  The function generates the change times for the page and
  then compares these times to the start of the next interval
  to determine the amount of time the page is stale, i.e.
  updated but not yet retrieved by the crawler.

  One can plot and compute summaries of the simulation using
  \code{plot} and \code{summary}.
}
\usage{
staleSim(rate, crawlInterval = 2, numIntervals = 200, n = 1000, inter.times = rexp(n, rate))
}

\arguments{
  \item{rate}{this is the value of lambda in the Poisson model that
    gives the rate at which pages change. 1/lambda is the average amount
   of time between page updates}
  \item{crawlInterval}{the length of time between crawls. This can be a
  single number in which case all of the intervals are of the same
  length, or this can be a vector of different interval lengths.}
  \item{numIntervals}{the number of intervals to simulate the crawler}
  \item{n}{the number of inter-arrival times to randomly generate. This
  should be a large number so that the cumulative time exceeds
  \code{numIntervals * crawlInterval} }
\item{inter.times}{a vector of times between page changes.
  This allows us to use the same values in different calls,
  e.g. for different \code{crawlInterval} values.}
}

\value{
  A list with the following elements:
  \item{changeTimes}{the times when the changes to the page occurred}
  \item{crawlTimes}{the different times the crawler retrieved any
    updates to the page}
  \item{inter.times}{the times between changes (which can also be computed
  from \code{changeTimes}.}
  \item{rate}{the value of the change rate for the Poisson.}
  \item{staleDurations}{a vector of values that give the amount of time
    the page was stale in each of the crawl intervals}
}
\references{

}
\author{
Duncan Temple Lang
}

\seealso{

}
\examples{

  sim = staleSim(.5, 1, 10)
  names(sim)
  summary(sim)
  plot(sim)

  sim = staleSim(.01, 50,10)
  summary(sim)
  plot(sim)

  plot(density(sim$staleDurations))

  times = rexp(1000, .5)
  sim12 = staleSim(.5, 12, 10, inter.times = times)
  sim24 = staleSim(.5, 24, 10, inter.times = times)

  par(mfrow = c(2, 1))
  plot(sim12, xlim = c(1, 240))
  plot(sim24, xlim = c(1, 240))

}


