################################################################################
##
## $Id: trade.costs.R 1220 2007-09-21 03:05:18Z enos $
##
## function for performing a basic trade cost analysis
##
################################################################################

trade.costs <- function(trade,
                        dynamic,
                        static          = NULL,
                        decisions       = NULL,
                        benchmark.price = "vwap",
                        base.price      = "exec.price",
                        num.trades      = 5,
                        analysis.title  = "Trade Cost Analysis",
                        batch.method    = "unique",
                        start.period    = NULL,
                        end.period      = NULL
                        ){

  if(is.null(start.period)){
    start.period <- min(trade$period)
  }
  
  if(is.null(end.period)){
    end.period <- max(trade$period)
  }
  
  if(start.period > end.period){
    stop("Start period must come before end period.")
  }

  trade   <- subset(trade, period >= start.period & period <= end.period)
  dynamic <- subset(dynamic, period >= start.period & period <= end.period)
  
  ## add some stop?
  
  if(is.null(static)){
    static <- data.frame()
  }
  
  if(is.null(decisions)){
    decisions <- data.frame()
  }
  
  ## create tradeCosts and tradeCostsResults objects
  
  tc <- new("tradeCosts", name = analysis.title,
            trade.data = trade, static.desc = static,
            dynamic.desc = dynamic, decisions.data = decisions)
  
  results <- analyzeData(tc,
                         base.price = base.price,
                         benchmark.price = benchmark.price,
                         batch.method = batch.method)
  
  ## return tradeCostsResults object
  
  invisible(results)
}
