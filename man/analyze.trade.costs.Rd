\name{analyze.trade.costs}
\alias{analyze.trade.costs}
\title{User-level function to perform basic trade cost analysis.}
\description{
  This function requires 3 data sets (dynamic descriptive, static
  descriptive, and trading data).  It also requires the names of
  important columns related to the trade cost analysis.
}
\usage{
analyze.trade.costs(trade, dynamic, static = NULL,
benchmark.price = "vwap", num.trades = 5,
analysis.title = "Trade Cost Analysis",  out.fmt = "pdf", pdf.file = NULL)
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
   \item{benchmark.price}{ The \code{benchmark.price} argument should be the
    name of a column in \code{dyna} and is the price that will be used
    for benchmarking and calculating trade cost statistics such as
    slippage. }
  \item{num.trades}{The \code{num.trades} argument is a numeric
    argument that specifies how many trades should be shown in the
    resulting summaries. The default is 5 trades, which means for each
    category, the summary shows the 5 best and worst trades.}
  \item{analysis.title}{ User-specified character string which serves as
    the title of the report for aesthetic purposes. }
  \item{out.fmt}{Defaults to \code{pdf}, allows user to choose what type of output
    file the user receives. \code{text} will print to console and
    \code{pdf-verbose} will print a verbosre pdf report.}
  \item{pdf.file}{ Optional user-specified relative file path and file
    name to store pdf report at for saving.}
}
\value{
  A summary of the trade cost analysis is output and a tradeCostsResults
  object is returned.
}
\author{Aaron Schwartz and Luyi Zhao }

\examples{
data(dynamic.mar.2007)
data(trade.mar.2007)
data(static.mar.2007)
analyze.trade.costs(trade.mar.2007, dynamic.mar.2007,
                    static.mar.2007, num.trades = 3, out.fmt = "text")
}

\keyword{}
