## function for performing a basic trade cost analysis

analyze.trade.costs <- function(trade,
                                dynamic,
                                static          = NULL,
                                benchmark.price = "vwap",
                                num.trades      = 5,
                                analysis.title  = "Trade Cost Analysis",
                                out.fmt         = "pdf",
                                pdf.file        = NULL){

  ## build tradeCosts object (parses the data)
  
  if(is.null(static)){
    static <- data.frame()
  }
     
  tc <- new("tradeCosts", name = analysis.title, trade.data = trade,
            static.desc = static, dynamic.desc = dynamic)
  results <- analyzeData(tc, base.price = "exec.price", benchmark.price)

  ## summary output
  
  if(isTRUE(all.equal(out.fmt, "pdf"))){
    pdfSummary(results, num.trades = num.trades, pdf.file = pdf.file, out.fmt = out.fmt)
  }
  else if(isTRUE(all.equal(out.fmt, "pdf-verbose"))){
    pdfSummary(results, num.trades = num.trades, pdf.file = pdf.file, out.fmt = out.fmt)
  }
  else if(isTRUE(all.equal(out.fmt, "text"))){
    summary(results, num.trades = num.trades)
  }
  else{
    stop("Invalid out.fmt.  Please enter pdf, pdf-verbose, or text")
  }
  
  ## return tradeCostsResults object
  
  invisible(results)
}
