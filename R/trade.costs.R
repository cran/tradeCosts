################################################################################
##
## $Id: trade.costs.R 1256 2008-04-20 23:10:31Z enos $
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

  trade   <- trade[!is.na(trade$period) & trade$period >= start.period & trade$period <= end.period,]
  dynamic <- dynamic[!is.na(dynamic$period) & dynamic$period >= start.period & dynamic$period <= end.period,]
  
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
