\name{mle}
\alias{mle}
\title{Compute the Cho and Garcia-Molina MLE for page change rates}
\description{
  
  This function allows the caller to compute the maximum likelihood
  estimate for rate of change of a Web page.  The function can be called
  with the data in different formats, but essentially we need to know in
  which intervals a change was detected and which intervals had no
  change to the page, and then how long each of these intervals was.  In
  many cases, the length of the interval is the same for all intervals
  so we can specify a single number for this.  To specify the change
  events, we can give a logical vector of \code{TRUE} and \code{FALSE}
  elements which correspond to the ordered intervals.  A \code{TRUE}
  value means there was a change in that interval and \code{FALSE}
  indicates no change(s).

  An alternative way to specify the information is to give a vector of
  integer values that indicate the index/identity of the intervals in
  which a change occurred. You also need to specify the total number of
  intervals the crawler attempted to fetch the page. This is because
  there may have been no changes from one point all the way to the end
  of the crawling and these need to be counted as no changes rather than
  assuming we don't have that information.
  
}
\usage{
mle(changeEvents, interval = 12, totalIntervals = length(changeEvents))
}
\arguments{
  \item{changeEvents}{
    either a vector of the form
    \code{c(TRUE, FALSE, FALSE, TRUE, FALSE)}
    giving all the intervals and whether a change was detected in each
    of them; or     \code{c(1, 4)}
    which identifies in which intervals a change was detected.
}
\item{interval}{the length of time between crawls.
  This can be a single number if all of the intervals have the same
  length. Alternatively, you can specify a vector whose
  values are the individual interval lengths, e.g.
   \code{c(12, 11, 12, 12, 12, 8, 23)}.
  }

  \item{totalIntervals}{
   the total number of intervals the Web crawler attempted to retrieve
   the page.}
}
\details{
  This solves equation  2 on page 8 of
  The Cost of Refresh Failures, Carrie Grimes, D. Ford and E. Tassone.
  It uses \code{uniroot} to do this.
  We construct a function that is a function of lambda and which
  has access to the interval lengths and which intervals had a change
  and which did not. 
}
\value{
  A single real value which is the estimate of lambda,
  the parameter in the Poisson distribution that
  is used to model the number of changes within a unit time interval.
}
\references{
  The Cost of Refresh Failures, Carrie Grimes, D. Ford and E. Tassone.
  Cho and Garcia-Molina
}
\author{
 Duncan Temple Lang
}

\seealso{
\code{\link{uniroot}}
}

\examples{
  events = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  lambda = mle(events, 3)
  1/lambda # expected time between changes

  lambda = mle(which(events), 3, 7)
  1/lambda #  same as before.

  data(cr30DayShort)
  1/mle(cr30DayShort$intervals[[22]], 1)
}


