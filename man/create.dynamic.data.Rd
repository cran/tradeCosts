\name{create.dynamic.data}
\alias{create.dynamic.data}
\title{User-level function to create a dynamic data.frame for use in trade.costs}
\description{
  This function requires two vectors: period, id. It combines these two
  vectors and optional vectors into a single data.frame ready for use in the trade.costs function.
}
\usage{
create.dynamic.data(period, id, ...)
}
\arguments{
  \item{period}{The column of periods in which trades occured.}
  \item{id}{The column of security identifiers traded.}
  \item{...}{Additional vector arguments of the same length to be
  incorporated into the resulting data frame}
}
\value{
  A data.frame is returned for use in the trade.costs function.
}
\details{
  This helper function makes creating a dynamic data.frame easier.
}  
\author{Daniel Suo}
\examples{
data(dynamic.mar.2007)
create.dynamic.data(dynamic.mar.2007$period, dynamic.mar.2007$id,
dynamic.mar.2007$vwap)
}

\keyword{file}
