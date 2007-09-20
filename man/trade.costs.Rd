\name{trade.costs}
\alias{trade.costs}
\title{User-level function to perform basic trade cost analysis.}
\description{
  This function requires three data sets (dynamic descriptive, static
  descriptive, and trading data).  It also requires the names of
  important columns related to the trade cost analysis.
}
\usage{
trade.costs(trade, dynamic, static = NULL, decisions = NULL,
            benchmark.price = "vwap", base.price = "exec.price", num.trades = 5,
            analysis.title = "Trade Cost Analysis", batch.method = "unique",
            start.period = NULL, end.period = NULL)
}
\arguments{
  \item{dynamic}{ The  \code{dynamic} argument is  for dynamic descriptive
    data of securities and should include some notion of time period and
    price.}
  \item{trade}{ The  \code{trade} argument should be a data
    set with trading data including at least one type of price related
    to the execution of a trade as well as a notion of period. }
   \item{static}{ The \code{static} argument is an optional argument
     with defaults \code{NULL} that  provides descriptive data for
     securities, should include basic information for each security.}
   \item{decisions}{The \code{decisions} argument is an optional
     argument that can provide decision data for calculating
     implementation shortfall.}
   \item{benchmark.price}{ The \code{benchmark.price} argument should be the
    name of a column in \code{dyna} and is the price that will be used
    for benchmarking and calculating trade cost statistics such as
    slippage.}
   \item{base.price}{ The \code{base.price} argument should be the
     name of a column in \code{dyna} and is the price that will be used
     as the base price for calculating trade cost statistics such as
    slippage.}
  \item{num.trades}{The \code{num.trades} argument is a numeric
    argument that specifies how many trades should be shown in the
    resulting summaries. The default is 5 trades, which means for each
    category, the summary shows the 5 best and worst trades.}
  \item{analysis.title}{ User-specified character string which serves as
    the title of the report for aesthetic purposes. }
  \item{batch.method}{Option for selecting method for grouping
    trades.}
  \item{start.period}{Object of the same class as the \code{period}
    column in the \code{trade} and \code{static} data frames that
    specifies the first period of the analysis.}
  \item{end.period}{Object of the same class as the \code{period}
    column in the \code{trade} and \code{static} data frames that
    specifies the last period of the analysis.}
}
\value{
  A  tradeCostsResults object is returned.
}
\details{
  This version of \code{trade.costs} provides a single multi-period
  sample batch method, \code{same.sided}, which groups all consecutive
  same-sided orders into a single batch.  The default
  \code{batch.method}, \code{unique}, places each trade in its own
  batch.
}  
\author{Aaron Schwartz and Luyi Zhao }
\examples{
data(dynamic.mar.2007)
data(trade.mar.2007)
data(static.mar.2007)
trade.costs(trade.mar.2007, dynamic.mar.2007,
                    static.mar.2007, num.trades = 3)
}

\keyword{}
