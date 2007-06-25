setMethod("summary",
          signature(object = "tradeCostsResults"),
          function(object, num.trades = 5){
            
            cat("\n")
            
            cat("Trade Cost Analysis\n\n\n")

            ## Summary Stats
            sumstats <- calcStats(object@results)
            cat("Summary Statistics\n")
            cat(
                sprintf(" Total Market Value: %17s", format(sumstats$total.mkt.value, big.m = ",")), "\n",
                sprintf("Total Percent Cost:    %17.2f", sumstats$total.percent.cost), "\n",
                sprintf("Max Percent Cost:      %17.2f", sumstats$max.percent.cost), "\n", 
                sprintf("Min Percent Cost:      %17.2f", sumstats$min.percent.cost), "\n",
                sprintf("First Period:       %17s", sumstats$first.period), "\n",
                sprintf("Last Period:        %17s", sumstats$last.period), "\n"
                )
                        
            ## filler space
            
            cat("\n\n")
            
            ## print out the best trades by id
            
            cat("Best and worst ID's over all periods\n")
            show(calcID(object@results, num.trades))
            
            ## filler space
            
            cat("\n\n")
            
            ## print out the best trades by period
            
            cat("Best and worst periods over all ID's\n")
            show(calcPeriod(object@results, num.trades))

            ## filler space
            
            cat("\n\n")

            ## Best/worst
            
            cat("Best and Worst trades\n")
            show(calcAll(object@results, num.trades))

            ## filler space
            
            cat("\n\n")
            
            ## NA reporting
            
            cat("NA REPORT\n")
            na.counts <- data.frame(t(object@na.counts))
            colnames(na.counts) <- "count"
            show(na.counts)
            
            ## filler space
            
            cat("\n\n")

                        

          })




setMethod("pdfSummary",
          signature(object = "tradeCostsResults"),
          function(object, num.trades = 5, pdf.file = NULL, out.fmt = "pdf"){
            if("trade.costs.object" %in% ls(.GlobalEnv)){
              stop("Object trade.costs.object exists in the global env.")
            }
            if("trade.num.trades" %in% ls(.GlobalEnv)){
              stop("Object trade.num.trades exists in the global env.")
            }
           
            assign("trade.costs.object", object, envir = .GlobalEnv)
            assign("trade.num.trades", num.trades, envir = .GlobalEnv)
            
            if(isTRUE(all.equal(out.fmt, "pdf"))){
              sweave.file <- system.file("template/tradeCostsReport.Rnw",
                                         package = "tradeCosts")
            }
            else if(isTRUE(all.equal(out.fmt, "pdf-verbose"))){
              sweave.file <- system.file("template/tradeCostsReport-verbose.Rnw",
                                         package = "tradeCosts")
            }
            
            ## save the working directory
            
            wkdir <- getwd()
            
            ## generate a temp. directory
            
            tdir <- tempdir()
            
            ## set the wd to temp dir.
            
            setwd(tdir)

            ## Sweave the file
            
            Sweave(sweave.file, quiet = TRUE)
            
            if(isTRUE(all.equal(out.fmt, "pdf"))){
              texi2dvi("tradeCostsReport.tex", pdf = TRUE)
            }
            else if(isTRUE(all.equal(out.fmt, "pdf-verbose"))){
              texi2dvi("tradeCostsReport-verbose.tex", pdf = TRUE)
            }
            
            if(!is.null(pdf.file)){
              if(isTRUE(all.equal(out.fmt, "pdf"))){
                file.copy("tradeCostsReport.pdf",
                          to = pdf.file, overwrite = TRUE)
              }
              else if(isTRUE(all.equal(out.fmt, "pdf-verbose"))){
                file.copy("tradeCostsReport-verbose.pdf",
                          to = pdf.file, overwrite = TRUE)
              }
            }
            else{
              if(isTRUE(all.equal(out.fmt, "pdf"))){
                pdf.file <- file.path(tdir, "tradeCostsReport.pdf")
              }
              else if(isTRUE(all.equal(out.fmt, "pdf-verbose"))){
                pdf.file <- file.path(tdir, "tradeCostsReport-verbose.pdf")
              }
              
            }
            
            ##open PDF file path
            
            .pdfOpen(pdf.file)
            
            ## reset working directory
            
            setwd(wkdir) 
            rm(trade.costs.object, trade.num.trades, envir = .GlobalEnv)
           
          })


setMethod("calcAll",
          signature(results = "data.frame", num.trades = "numeric"),
          function(results, num.trades){
            
            out <- results[c("id", if("symbol" %in% colnames(results)){"symbol"},
                             "period", "side", "exec.qty",
                             "exec.mkt.val", "percent.cost", "cost")]
            
            ## convert to character as a workaround xtable's limitations
            
            out$period <- as.character(out$period)
            
            out <- .getHeadTail(out, num.trades)
            
            return(out)
          })
          
setMethod("calcID",
          signature(results = "data.frame", num.trades = "numeric"),          
          function(results, num.trades){
            
            out <- .calcCulmCosts(results, "id")
            
            if("symbol" %in% colnames(results)){
              out$symbol <- results$symbol[order(unique(results$id))]
            }
            
            out <- out[c("id", if("symbol" %in% colnames(results)){"symbol"},
                         "exec.qty", "exec.mkt.val","percent.cost", "cost")]
            
            out <- .getHeadTail(out, num.trades)
            
            return(.prettify.df(out))
          })

setMethod("calcPeriod",
          signature(results = "data.frame", num.trades = "numeric"),
           function(results, num.trades){
             
             out <- .calcCulmCosts(results, "period")
             
             out <- out[c("period", "exec.qty", "exec.mkt.val",
                          "percent.cost", "cost")]
             
             ## convert to character as a workaround xtable's limitations
             
             out$period <- as.character(out$period)
             
             out <- .getHeadTail(out, num.trades)
             
             return(.prettify.df(out))
             
           })

.calcCulmCosts <- function(results, calc.by){
  
  out <- rowsum(group   = results[[calc.by]],
                x       = data.frame(exec.qty = results$exec.qty,
                          exec.mkt.val = results$exec.mkt.val,
                          cost = results$cost),
                na.rm   = TRUE,
                reorder = TRUE)
  
  out$percent.cost <- (out$cost / out$exec.mkt.val) * 100
  
  out[[calc.by]] <- sort(unique(results[[calc.by]]))
    
  return(out)
}

.getHeadTail <- function(out, num.trades, order.by = "percent.cost"){
  
  ## sort data frame by percent cost
  
  out <- out[order(out[[order.by]], na.last = NA),]

  ## Re-order the row names
  
  rownames(out) <- NULL

  out <- rbind(head(out, n = num.trades), tail(out, n = num.trades))

  return(out)
}


setMethod("calcStats",
          signature(results = "data.frame"),
          function(results){
            
            out <- data.frame(total.mkt.value =
                              sum(results$exec.qty*results$exec.price))
            
            out$total.percent.cost <- 100*(sum(results$cost)/out$total.mkt.value)
            out$max.percent.cost   <- max(results$percent.cost, na.rm = TRUE)
            out$min.percent.cost   <- min(results$percent.cost, na.rm = TRUE)
            
            period.list      <- range(list(unique(as.character(results$period))))
            out$first.period <- period.list[1]
            out$last.period  <- period.list[2]
            
            return(out)
          })
          

## inspired by vignette print code -> opens pdf

.pdfOpen <- function(file.path){
  
  if(.Platform$OS.type == "windows"){
    shell.exec(file.path)
  }
  else{
    system(paste(shQuote(getOption("pdfviewer")), shQuote(file.path)),
           wait = FALSE)
  }
}

.prettify.df <- function(x){
 stopifnot(is.data.frame(x))
 for(n in names(x)){
   if(is.numeric(x[[n]]) && !inherits(x[[n]], "Date")){
     if(isTRUE(mean(abs(x[[n]]), na.rm = TRUE) < 10)){
       x[[n]] <- round(x[[n]], digits = 2)
     }
     else{
       x[[n]] <- round(x[[n]], digits = 0)
     }
     x[[n]] <- format(x[[n]], big.mark = ",")
   }
 }
 invisible(x)
}
