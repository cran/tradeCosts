\name{trade.mar.2007}
\alias{trade.mar.2007}
\docType{data}
\title{Sample trading data}
\description{
  Sample trading data from March, 2007.
}
\usage{data(trade.mar.2007)}
\format{
  A data frame with 413 observations on the following 5 variables.
  \describe{
    \item{\code{period}}{a Date specifying the period of the trade}
    \item{\code{id}}{a character vector containing a unique security id}
    \item{\code{side}}{a character vector specifying type of trade.
      Takes on values \code{B} (buy), \code{S} (sell), \code{C} (cover)
      and \code{X} (short sell)}
    \item{\code{exec.qty}}{a numeric vector specifying the size of the trade}
    \item{\code{exec.price}}{a numeric vector specifying the execution price}
  }
}
\examples{
data(trade.mar.2007)
}
\keyword{datasets}
