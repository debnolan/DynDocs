\name{CrawlRates}
\alias{CrawlRates}
\title{Data and functions for Estimating optimal crawl rates for 'bots}
\description{This package provides a collection
  of data sets and some functions for exploring
  how often Web crawling robots (bots) should
  return to a page to see if it has changed.
  The goal of the case study is to explore approaches
  to finding  ways to try to minimize the amount of
  time a bot has to query a page, but still have a good
  chance of having the most up to date version of that
  page, e.g. for Web searches.
}
\author{Duncan Temple Lang}
\examples{
  data(cr30DayShort)
  table(sapply(cr30DayShort$intervals, length))
  which(sapply(cr30DayShort$intervals, length) > 0)
  1/mle(cr30DayShort$intervals[[22]], 1)
}
