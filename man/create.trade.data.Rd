\name{create.trade.data}
\alias{create.trade.data}
\title{User-level function to create a trade data.frame for use in trade.costs}
\description{
  This function requires five vectors: period, id, side, exec.qty, and
  exec.price. It combines these five vectors and optional vectors into a
  single data.frame ready for use in the trade.costs function.
}
\usage{
create.trade.data(period, id, side, exec.qty, exec.price, ...)
}
\arguments{
  \item{period}{The column of periods in which trades occured.}
  \item{id}{The column of security identifiers traded.}
  \item{side}{What side the security was traded in.}
  \item{exec.qty}{How much was traded.}
  \item{exec.price}{The price per share of the trade.}
  \item{...}{Additional vector arguments of the same length to be
  incorporated into the resulting data frame}
}
\value{
  A data.frame is returned for use in the trade.costs function.
}
\details{
  This helper function makes creating a trade data.frame easier.
}  
\author{Daniel Suo}
\examples{
data(trade.mar.2007)
create.trade.data(trade.mar.2007$period, trade.mar.2007$id,
trade.mar.2007$side, trade.mar.2007$exec.qty, trade.mar.2007$exec.price)
}

\keyword{file}
