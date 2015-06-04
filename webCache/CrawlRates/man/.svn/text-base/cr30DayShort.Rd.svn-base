\name{cr30DayShort}
\alias{cr30DayShort}
\alias{smallChangeFast}
\alias{smallChangeSlow}
\alias{fast30day}
\alias{slow30day}
\alias{addLinkFast}
\alias{anyLinkFast}
\docType{data}
\title{Page change data collected by a Web crawler}
\description{

  These data sets contain information about various collections of URLs.
  The information is the URL itself and the occassions when the Web
  crawler determined that there was a change in the page from the
  previous retrieval.  The idea is that the Web crawler attempts to
  retrieve the page at regular intervals (every 12 hours).  It then
  compares the current page to the previously downloaded version and
  determines if there is a change. (This can be done in different ways.)
  If there is a change, the identity of the crawl interval is
  recorded. And so we end up with a series of events in which a page
  changed for a given URL and when each change was detected.

  \code{cr30DayShort} is a small data set with just 1000 URLs
  which change relatively slowly.
  
  \code{addLinkFast} is for URLs/pages that change
  frequently and in which the notion of change
  is new links added to the page.

  \code{anyLinkFast} is again for rapidly changing URLs
  but the notion of change is that any of the links to other pages
  (inter- or intra-nal site) changed.

  The two data sets \code{smallChangeFast} and \code{smallChangeSlow}
  are for sets of URLs that change rapidly and relatively infrequently
  respectively. The notion of change is related to the content
  for the reader rather than the links to other pages.
  
}
\usage{data(cr30DayShort)}
\format{
  These objects are lists with 4 elements:
  \describe{
  \item{urls}{a character vector giving the full URLs of the pages that
    were being crawled in this sample.}
  \item{numIntervals}{an integer vector with an element for each URL.
    The value for a URL is the total number of intervals for which a
    crawl was attempted.}
  \item{intervals}{a list with an entry for each URL. Each entry is a
    vector of integers that tell us the index of the intervals in which
    a crawl was made and a change detected.}
  \item{refreshEvents}{a data frame which has a row for each event
    in which the Web crawler detected a change from the  previous
    version of that page.
    Each row has the URL for the page being crawled, the index of the
    interval, and the total number of intervals.
    This contains the same information as the three earlier elements
    but in a slightly different format. This can make some operations
    easier, e.g. plotting the occurrences of the events for each URL.
  }
  }
}
\details{
  
}
\source{
  Carrie Grimes
}
\references{
Estimators for Rate of Change
}
\examples{
data(cr30DayShort)

with(cr30DayShort$refreshEvents,
      plot(as.integer(url) ~ intervals, pch = ".", cex = .5,
            ylab = "URL"))

plot(as.integer(url) ~ intervals,
      subset(cr30DayShort$refreshEvents,
                url \%in\%cr30DayShort$urls[sapply(cr30DayShort$intervals, length) > 20]),
       pch = ".", cex = .5, ylab = "URL")
}
\keyword{datasets}
