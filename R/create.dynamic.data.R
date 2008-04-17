################################################################################
##
## $Id: create.trade.data.R 1253 2008-02-15 20:48:04Z suo $
##
## function for creating a properly formed trade data
## frame for use in the trade.costs function.
##
################################################################################

create.dynamic.data <- function(period,
                                id,
                                ...){

  if(!all(is.vector(unclass(period)), is.vector(id))){
    stop("All arguments must be vectors.")
  }
  
  if(!all.equal(length(period), length(id)))
    stop("All arguments must have the same length.")
  
   if(length(list(...)) > 0) {
     for(i in 1:length(list(...))){
       if(!is.vector(unclass(list(...)[[i]]))){
        stop("All arguments must be vectors.")
      }
      if(length(list(...)[[i]]) != length(period)) {
        stop("All arguments must have the same length.")
      }
    }
  }
   
  return(data.frame(period     = period,
                    id         = id,
                    ...))
}

