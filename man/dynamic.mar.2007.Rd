\name{dynamic.mar.2007}
\alias{dynamic.mar.2007}
\docType{data}
\title{Sample dynamic descriptive data}
\description{
  Sample dynamic descriptive data, or data that changes over time such
  as price, for a set of securities.
}
\usage{data(dynamic.mar.2007)}
\format{
  A data frame with 1688 observations on the following 6 variables.
  \describe{
    \item{\code{period}}{a Date}
    \item{\code{id}}{a character vector containing a unique security id}
    \item{\code{price}}{a numeric vector containing the period's price}
    \item{\code{vwap}}{a numeric vector containing the volume-weighted
      average price for the period}
    \item{\code{prior.close}}{a numeric vector specifying the prior
      closing price for the period}
    \item{\code{adjustment.factor}}{a numeric vector}
  }
}
\examples{
data(dynamic.mar.2007)
}
\keyword{datasets}
