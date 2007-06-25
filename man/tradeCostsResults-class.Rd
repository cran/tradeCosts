\name{tradeCostsResults-class}
\docType{class}

\alias{tradeCostsResults-class}
\alias{extremeTrades,tradeCostsResults,numeric,character-method}
\alias{summary,tradeCostsResults-method}
\alias{pdfSummary,tradeCostsResults-method}
\alias{calcAll,data.frame,numeric-method}
\alias{calcID,data.frame,numeric-method}
\alias{calcPeriod,data.frame,numeric-method}
\alias{calcStats,data.frame-method}


\alias{calcAll}
\alias{calcID}
\alias{calcPeriod}
\alias{calcStats}

\title{Class "tradeCostsResults"}
\description{ This function constructs a basic trade cost analysis
  results object.}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} containing
      the name of this trade cost analysis}
    \item{\code{results}:}{Object of class \code{"data.frame"} that
      contains the raw trade cost results}
    \item{\code{na.counts}:}{Objects of class \code{"data.frame"} that
      contains the raw trade cost results}
    \item{\code{top.trades}:}{Objects of class \code{"vector"} that
      contain summary information on a variety of levels.}
     }
}
\section{Methods}{
  \describe{
    \item{extremeTrades}{\code{signature(object = "tradeCostsResults",
	num.trades = "numeric",  group.by = "character")}:
      Calculates the best and worst  \code{num.trades} of trades in terms of cost.}
    \item{summary}{\code{signature(object = "tradeCostsResults")}:
      Provides a printout summarizing the \code{tradeCostsResults}
      object.}
    \item{pdfSummary}{\code{signature(object = "tradeCostsResults")}:
      Provides a pdf summary of the \code{tradeCostsResults} object.}
    \item{calcAll}{\code{signature(results = "data.frame",
      num.trades = "numeric")}: Calculates best/worst trades for all
      id/date pairs.}
    \item{calcID}{\code{signature(results = "data.frame",
	num.trades = "numeric")}: Calculates best/worst trades for
	ID's.}
    \item{calcPeriod}{\code{signature(results = "data.frame",
	num.trades = "numeric")}: Calculates best/worst trades over
      periods.}
    \item{calcStats}{\code{signature(results = "data.frame")}:
      Calculates summary statistics for the tradeCostsResults object.}
    
  }
}


\author{Aaron Schwartz and Luyi Zhao}
\keyword{classes}
