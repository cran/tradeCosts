\name{tradeCosts-class}
\docType{class}

\alias{tradeCosts-class}
\alias{analyzeData,tradeCosts,character,character-method}


\title{Class "tradeCosts"}
\description{ This is an object of tradeCosts class constructed so trade
  cost analysis can be performed.}

\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} containing
      the name of this \code{"tradeCosts"} object.}
    \item{\code{trade.data}:}{Object of class \code{"data.frame"}
      with trade information}
    \item{\code{static.desc}:}{Object of class \code{"data.frame"}
      with static descriptive security information.}
    \item{\code{dynamic.desc}:}{Object of class \code{"data.frame"}
      with dynamic descriptive security information.}
  }
}

\section{Methods}{
  \describe{
    \item{analyzeData}{\code{signature(object = "tradeCosts",
	base.price = "character", benchmark.price = "character")}:
      Merges the data, does NA reporting, and calls private functions
      that calculate trade costs.  }
  }
}
  
\author{Aaron Schwartz and Luyi Zhao}
\keyword{classes}
