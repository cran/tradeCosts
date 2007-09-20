#################################################################################
##
## $Id: AllClasses.R 1171 2007-09-13 15:08:31Z zhao $
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
                        name           = "character",
                        trade.data     = "data.frame",
                        dynamic.desc   = "data.frame",
                        static.desc    = "data.frame",
                        decisions.data = "data.frame"
                        ),
         
         prototype = prototype(
           name           = "Trade Cost Analysis",
           trade.data     = data.frame(),
           dynamic.desc   = data.frame(),
           static.desc    = data.frame(),
           decisions.data = data.frame()
         ),
         validity = function(object){

           ## columns for trade.data

           if(!all(c("id", "period", "side","exec.qty", "exec.price") %in% names(object@trade.data))){
             stop("Columns of trade.data must include id, period, side, exec.qty, and exec.price.")
           }

           ## columns for dynamic.desc

           if(!all(c("id", "period") %in% names(object@dynamic.desc))){
             stop("Columns of dynamic. data must include id and period.")
           }

           ## columns for static.desc

           if(!(nrow(object@static.desc) == 0) && !all(c("id", "symbol") %in% names(object@static.desc))){
             stop("Static descriptive data is not required but if used must contain id and symbol columns")
           }

           ## columns for decisions.data

           if(!(nrow(object@decisions.data) == 0) && !all(c("id", "side", "period", "qty", "price"))){
             stop("Decisions is not required but if used must contain id, side, period, qty, and price")
           }
             
           
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
                        name             = "character",
                        executions       = "data.frame",
                        batches          = "data.frame",
                        na.counts        = "data.frame",
                        base.price       = "character",
                        benchmark.price  = "character",
                        batch.method     = "character"
                        ),
         
         prototype = prototype(
           name            = "Trade Cost Analysis",
           executions      = data.frame(),
           batches         = data.frame(),
           na.counts       = data.frame(),
           base.price      = "exec.qty",
           benchmark.price = "vwap",
           batch.method    = "unique"
          )
         )
         
         
         
