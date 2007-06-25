#################################################################################
##
## $Id: AllClasses.R 427 2007-06-25 19:13:42Z enos $
##
## Class definitions for the tradeCosts package.
##
################################################################################


## Load hook for methods

.onLoad <- function(lib, pkg) require(methods)

## tradeCostsResults class contains the data for working
## with the finalize  transaction cost results


setClass("tradeCosts",
         representation(
                        name         = "character",
                        trade.data   = "data.frame",
                        dynamic.desc = "data.frame",
                        static.desc  = "data.frame"
                        ),
         
         prototype = prototype(
           name          = "Trade Cost Analysis",
           trade.data    = data.frame(),
           dynamic.desc  = data.frame(),
           static.desc   = data.frame()
         ),
         validity = function(object){
           
           ## The Id vars must all be of the same type.
           
           if(!isTRUE(all.equal(class(object@dynamic.desc$id),
                                class(object@trade.data$id)))){
             return("Id vars must be of the same type")
           }

           ## The period vars must be of the same type in your data sets
           
           if(!isTRUE(all.equal(class(object@dynamic.desc$period),
                                class(object@trade.data$period)))){
             return("Period vars must be of the same type")
           }
           
           ## Only one trade per period per id allowed
           
           if(any(table(data.frame(object@trade.data$id,
                                     object@trade.data$period)) > 1)){
             return("Only one trade per period per id allowed.")
           }
           
           return(TRUE)
         }
       )
 
## tradeCostsResults class contains the data for working
## with the finalized transaction cost results

setClass("tradeCostsResults",
         representation(
                        name       = "character",
                        results    = "data.frame",
                        na.counts  = "data.frame"
                        ),
         
         prototype = prototype(
           name       = "Trade Cost Analysis",
           results    = data.frame(),
           na.counts  = data.frame()
           )
         )
         
         
         
