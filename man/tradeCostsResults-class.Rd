\name{tradeCostsResults-class}
\docType{class}

\alias{tradeCostsResults-class}
\alias{extremeTrades,tradeCostsResults,numeric,character-method}
\alias{summary,tradeCostsResults-method}
\alias{pdfReport,tradeCostsResults-method}
\alias{calcAll,data.frame,numeric-method}
\alias{calcID,data.frame,numeric-method}
\alias{calcPeriod,data.frame,numeric-method}
\alias{calcStats,data.frame-method}
\alias{calc,tradeCostsResults,character-method}
\alias{show,tradeCostsResults-method}
\alias{plot,tradeCostsResults,character-method}

\alias{calc}
\alias{calcAll}
\alias{calcID}
\alias{calcPeriod}
\alias{calcStats}
\alias{pdfReport}


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
     \item{\code{base.price}:}{Object of class \code{"character"} that
      explains what price is being used as the base price.}
    \item{\code{benchmark.price}:}{Object of class \code{"character"} that
      explains what price is being used as the benchmark price.}
    \item{\code{batch.method}:}{Object of class \code{"character"} that
      explains in what way to group the executions for analysis.}
  }
  
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "tradeCostsResults")}:
      Provides a printout summarizing the \code{tradeCostsResults}
      object.}
    \item{pdfReport}{\code{signature(object = "tradeCostsResults")}:
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
    \item{plot}{\code{signature(x = "tradeCostsResults")}:
      Plots the tradeCostsResults object with the argument 'type' as one
      of "time.series" (this is the default), "time.series.bps",
      "cumulative", or any column name in the static data frame}
    
  }
}

\author{Aaron Schwartz and Luyi Zhao and Arjun Ravi Narayan}
\keyword{classes}
