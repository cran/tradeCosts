################################################################################
##
## $Id: create.trade.data.R 1255 2008-02-15 22:49:59Z suo $ $Revision: 1255 $ $Date: 2008-02-15 17:49:59 -0500 (Fri, 15 Feb 2008) $ $Author: suo $
##
## function for creating a properly formed trade data
## frame for use in the trade.costs function.
##
################################################################################

create.trade.data <- function(period,
                              id,
                              side,
                              exec.qty,
                              exec.price,
                              ...){

  if(!all(is.vector(unclass(period)), is.vector(id), is.vector(side),
          is.vector(exec.qty), is.vector(exec.price))) {
    stop("All arguments must be vectors.")
  }
  
  if(!all.equal(length(period), length(id), length(side),
                length(exec.qty), length(exec.price))) {
    stop("All arguments must have the same length.")
  }

  if(!all(side %in% c("B", "S", "C", "X", NA))) {
    stop("All values in side must be one of B, S, C, or X")
  }

  if(length(list(...)) > 0) {

    for(i in 1:length(list(...))){
      if(!is.vector(unclass(list(...)[i]))){
        stop("All arguments must be vectors.")
      }
      
      if(length(list(...)[i]) != length(period)) {
        stop("All arguments must have the same length.")
      }
    }
  }
      

  return(data.frame(period     = period,
                    id         = id,
                    side       = side,
                    exec.qty   = exec.qty,
                    exec.price = exec.price,
                    ...))
}
