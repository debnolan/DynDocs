#' Historical prices of various stocks.
#' 
#' A list containing the historical prices and information including but not limited to date, adjunsted prices of twelve differet stocks. 
#' 
#' @format A list with 12 elements; each element is a data frame containing historical price information of a specific stock.
#' \describe{
#' \element{ATT}{Verizon}{Southwest}{United}{Hilton}{Hyatt}{Hershey}{Kellog}{Totyota}{INC}{IBM}{GM}
#'  \item{Date}{date,in the form of MM/DD/YYYY}
#'  \item{Open}{open price of the stock, in USD}
#'  \item{High}{highest price throughout the day, in USD}
#'  \item{Low}{lowest price throughout the day, in USD}
#'  \item{Close}{close price of the stock, in USD}
#'  \item{Volume}{amount of stocks traded}
#'  \item{Adj Close}{Adjusted closing price, in USD}
#'  }
#'  @source \url{http://finance.yahoo.com/}
"stocks"