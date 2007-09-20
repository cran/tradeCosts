################################################################################
##
## $Id: get.is.R 1203 2007-09-20 11:01:22Z enos $
##
## Formally calculate implementation shortfall
##
################################################################################

## is stands for implementation shortfall

get.is <- function(x, base.price, benchmark.price){

  if(nrow(x) < 1){
    return(data.frame())
  }
  
  ## no notion of delay cost
  ## Check to see if required columns are present

  stopifnot(c("decision.shares", "decision", "id",
              "side", "period") %in% names(x))

  ## Calculate implementation shortfall for each decision
  
  shortfall  <-
    do.call(rbind,
            lapply(split(x, x$decision),
                   function(x){    

                     ## A lot of data is located in the first row, so
                     ## rather than call the first element of each
                     ## column, we create a new variable holding this
                     ## first row.
                     
                     data <- x[1,]
                     
                     ## Calculate tc = trade cost, and adjust for side.
                     
                     tc <- sum(x$exec.qty * (x[[base.price]] - x[[benchmark.price]]))
                     tc <- ifelse(x$side %in% c("X","S"), -1, 1) * tc

                     ## Note that below "decision" becomes "batch".
                     ## We should use the latter throughout for
                     ## consistency.
                     
                     cost.data <- data.frame(id               = data$id,
                                             batch            = data$decision,
                                             start.period     = data$period,
                                             end.period       = tail(x$period, 1),
                                             side             = data$side,
                                             batch.shares     = data$decision.shares,
                                             exec.qty         = sum(x$exec.qty),
                                             exec.mkt.val     = sum(x$exec.qty *
                                               x[[base.price]]),
                                             execution.cost   = tc[1])

                     cost.data[[base.price]] <- data[[base.price]]
                     cost.data[[benchmark.price]] <- data[[benchmark.price]]
                     
                     ## Rerrange columns in sensible order.
                     
                     cost.data <- cost.data[c("id","batch","start.period","end.period",
                                              "side","batch.shares","exec.qty", base.price,
                                              "exec.mkt.val", benchmark.price, "execution.cost")]

                     ## Add a descriptive (unique) batch name.  In
                     ## future versions of the package there could be
                     ## algorithms for calculating batches that make
                     ## this descriptive name non-unique.  Add a stop
                     ## to check for this.

                     cost.data$batch.name <- paste(cost.data$id, " (",
                                                   cost.data$start.period, " - ",
                                                   cost.data$end.period, ")", sep = "")
                     stopifnot(!any(duplicated(cost.data$batch.name)))
                     
                     ## Add a column named benchmark.price to cost.data
                     
                     cost.data[[benchmark.price]] <- data[[benchmark.price]]
                     
                     ## If the decision.price is the benchmark.price
                     ## add notion of opportunity cost and i.s
                     
                     if(benchmark.price %in% "decision.price"){
                       
                       ## Calculate oc = opportunity costs This
                       ## assumes that decision.price is already
                       ## adjusted
                       
                       oc <- (cost.data$batch.shares - sum(x$exec.qty)) *
                         (tail(x[[base.price]], 1) - tail(x$decision.price, 1))
                       
                       ## Adjust oc for side
                       
                       oc <- ifelse(x$side %in% c("X","S"), -1, 1) * oc
                       
                       ## Add oc and is to cost.data
                       
                       cost.data$opportunity.cost <- oc[1]
                       cost.data$cost             <- tc[1] + oc[1]
                     }
                     else{
                       cost.data$opportunity.cost <- 0
                       cost.data$cost               <- cost.data$execution.cost
                     }
                     
                     return(cost.data)
                     
                   }))
  
  
  invisible(shortfall)
}
                       
