################################################################################
##
## $Id: tradeCostsResults.R 1256 2008-04-20 23:10:31Z enos $
##
## Methods for tradeCostsResults object
## 
################################################################################

setMethod("show",
          signature(object = "tradeCostsResults"),
          function(object){
            
            cat("\nTrade cost analysis with benchmark price:", object@benchmark.price, "\n")
            
            ## Summary Stats
            sumstats <- calcStats(object@batches)
            cost.bps <- (sumstats$total.exec.cost / sumstats$total.mkt.value) * 100 * 100
            
            cat(
                sprintf(" Total Market Value: %17s",
                        format(sumstats$total.mkt.value, big.m = ",")), "\n",
                sprintf("First Period:       %17s", sumstats$first.period), "\n",
                sprintf("Last Period:        %17s", sumstats$last.period), "\n",
                sprintf("Trade Costs:        %17s",
                        format(round(sumstats$total.exec.cost), big.m = ","), digits = 2), "\n",
                sprintf("Total Cost (bps):   %17s",
                        format(round(cost.bps), big.mark = ",")), "\n"
                )   
          }
          )

setMethod("summary",
          signature(object = "tradeCostsResults"),
          function(object, num.trades = 3){
                        
            cat("\nTrade Cost Analysis\n\n")

            cat("Benchmark Price:", object@benchmark.price, "\n\n")
           
            ## Summary Stats
            sumstats <- calcStats(object@batches)
            cost.bps <- (sumstats$total.exec.cost / sumstats$total.mkt.value) * 100 * 100
            
            cat("Summary statistics:\n")
            cat(
                sprintf(" Total Market Value: %17s",
                        format(sumstats$total.mkt.value, big.m = ",")), "\n",
                sprintf("First Period:       %17s", sumstats$first.period), "\n",
                sprintf("Last Period:        %17s", sumstats$last.period), "\n",
                sprintf("Total Cost:         %17s",
                        format(round(sumstats$total.exec.cost), big.mark = ","), digits = 2), "\n",
                sprintf("Total Cost (bps):   %17s",
                        format(round(cost.bps), big.mark = ",")), "\n"
                
                )   
            
            ## filler space
            
            cat("\n")

            ## Best and Worst Batches

            cat("Best and worst batches over all periods:\n")

            out <- .getHeadTail(calc(object, "batch.name"), num.trades)
            show(.prettify.df(out))

            ## filler space
            
            cat("\n")

            ## Best/worst
            
            cat("Best and worst securities over all periods:\n")
            
            out <- .getHeadTail(calc(object, "id"), num.trades)
            names(out) <- c("id", "exec.qty", "cost")
            show(.prettify.df(out))

            ## filler space
            
            cat("\n")
            
            ## NA reporting
            
            cat("NA report:\n")
            na.counts <- data.frame(t(object@na.counts))
            colnames(na.counts) <- "count"
            show(na.counts)
            
            ## filler space
            
            cat("\n")

                        

          })


setMethod("plot",
          signature(x = "tradeCostsResults", y = "character"),
          function(x, y, ...) {
            
            sub = NULL
            
            if(isTRUE(all.equal(y, "time.series"))) {
              
              ## This is valid only for "unique" batched data.
              
              periods <- unique(x@executions$period)
              costs <- NULL
              periods <- periods[order(periods, decreasing = FALSE)]
              
              for(i in 1:length(periods)) {
                costs[i] <- sum(subset(x@executions, period %in% periods[i])$execution.cost)
              }
              mp <- barplot(costs,
                            main = "Trade costs by period",
                            sub = "",
                            xlab = "",
                            ylab = "Cost",
                            las = 2
                            )
              text(x = mp, y = par("usr")[3] - 2.8, labels = periods,
                   srt = 45, adj = 1, xpd = TRUE)
              
            } else if(isTRUE(all.equal(y, "time.series.bps"))) {
              
              periods <- unique(x@executions$period)
              costs <- NULL
              mktvl <- NULL
              periods <- periods[order(periods, decreasing = FALSE)]
              
              for(i in 1:length(periods)) {
                costs[i] <- sum(subset(x@executions, period %in% periods[i])$execution.cost)
                mktvl[i] <- sum(subset(x@executions, period %in% periods[i])$exec.mkt.val)
              }
              
              bps <- costs*10000/mktvl
              mp <- barplot(bps,
                            main = "Trade costs by period",
                            sub = "",
                            xlab = "",
                            ylab = "Basis points",
                            las = 2
                            )
              text(x = mp, y = par("usr")[3] - 2.8, labels = periods,
                   srt = 45, adj = 1, xpd = TRUE)
              
            } else if(isTRUE(all.equal(y, "cumulative"))) {
              
              ## This method only works with same.sided data

              ## Its purpose is to take the batches, and add up the
              ## costs (in bps) for each "day" of trading. So there is
              ## a sum of all the first day of trades, all the second
              ## day of trades, etc.
              
              stopifnot(isTRUE(all.equal(x@batch.method, "same.sided")))
              
              ## Create empty lists
              
              costs <- as.list(1:nrow(x@batches))
              mktvl <- as.list(1:nrow(x@batches))
              days <- vector(mode = "numeric", length = 1)
              
              for(i in 1:nrow(x@batches)) {
                
                ## This creates vectors of the execution costs and
                ## market values in each list element
                
                costs[[i]] <-
                  x@executions[
                               x@executions$period <= x@batches$end.period[i] &
                               x@executions$period >= x@batches$start.period[i] &
                               x@executions$id %in% x@batches$id[i],
                               ]$execution.cost
                mktvl[[i]] <- 
                  x@executions[
                               x@executions$period <= x@batches$end.period[i] &
                               x@executions$period >= x@batches$start.period[i] &
                               x@executions$id %in% x@batches$id[i],
                               ]$exec.mkt.val
                
                ## This gets days to have the maximum length
                
                
                if(length(costs[[i]]) > length(days)) {
                  days <- vector(mode = "numeric", length = (length(costs[[i]])))
                }
                
              }
              
              daysmktvl <- days
              
              for(i in 1:length(days)) {
                
                for(j in 1:nrow(x@batches)) {
                  
                  sumc <- costs[[j]][i]
                  summ <- mktvl[[j]][i]
                  days[i] <- days[i] + replace(sumc, is.na(sumc), 0)
                  daysmktvl[i] <- daysmktvl[i] + replace(summ, is.na(summ), 0)
                }
                
              }
              
              bps <- days * 10000 / daysmktvl
              
              mp <- barplot(bps,
                            main = "Trade costs by batch period",
                            sub = "",
                            xlab = "Period of batch",
                            ylab = "Basis points",
                            names.arg = 1:length(bps)
                            )
              
            } else if (y %in% names(x@executions)) {
              
              
              categories <- unique(x@executions[[y]])
              costs <- NULL
              mktvl <- NULL
              
              ## This is guaranteed to work because if it DID come
              ## from the static data, the column would be identical
              ## for all identical ids
              
              for(i in 1:nrow(x@batches)) {
                x@batches[i,y] <-
                  subset(x@executions, id %in% as.character(x@batches$id[i]))[y][1,]
              }
              
              
              for(i in 1:length(categories)) {
                costs[i] <- sum(x@batches[x@batches[[y]] %in% categories[i],]$execution.cost)
                mktvl[i] <- sum(x@batches[x@batches[[y]] %in% categories[i],]$exec.mkt.val)
              }
              
              bps <- costs*10000/mktvl
              
              ## Truncate category names to be shorter to make chart neater
              
              categories <- substring(categories, 1, 6)
              
              mp <- barplot(bps,
                            main = paste("Trade costs by",toString(y)),
                            ylab = "Basis points",
                            las=2
                            )
              text(x = mp, y = par("usr")[3] - 2.6, labels = categories,
                   srt = 45, adj = 1, xpd = TRUE)
            }
            else {
              warning(paste(y,
                            " not found in names(static.desc). It should",
                            "be one of time.series, cumulative, or in the",
                            "static data slot of the tradeCosts object"))
            }
            
          }
          )



setMethod("pdfReport",
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


setMethod("calc",
          signature(object = "tradeCostsResults", by = "character"),
          function(object, by){
            
            if(nrow(object@batches) < 1){
              return(data.frame())
            }
            
            out <- .calcCulmCosts(object@batches, by)

            ##out <- merge(batches, out, by)
            
            ##out <- out[c("id", if("symbol" %in% colnames(batches)){"symbol"},
              ##               "period", "side", "exec.qty",
                ##             "exec.mkt.val"")]

            ## add exec.mkt.val and percent.cost back?
            
            ## Re-order the row names
  
            rownames(out) <- NULL
            
            return(out)

          })


setMethod("calcID",
          signature(batches = "data.frame", num.trades = "numeric"),          
          function(batches, num.trades = 5){

            ## save symbols
            
            if("symbol" %in% colnames(batches)){
              id.symbol <- unique(data.frame("id" = batches$id, "symbol" = batches$symbol))
            }
            out <- .calcCulmCosts(batches, "id")
            
            ## add symbols, by id
            
            if("symbol" %in% colnames(batches)){
              out <- merge(out, id.symbol, by = "id", all.x = TRUE)
            }
            
            out <- .getHeadTail(out, num.trades)
            
          
          })

.calcCulmCosts <- function(results, calc.by){


  results <- subset(results, !is.na(results[[calc.by]]))

  out <- rowsum(group   = results[[calc.by]],
                x       = data.frame(exec.qty = results$exec.qty,
                          cost = results$cost),
                na.rm   = TRUE,
                reorder = TRUE)

  ## The below is dangerous, and requires that reorder = TRUE above.
  
  out[[calc.by]] <- sort(unique(results[[calc.by]]))

  ## Ensure that the calc.by column appears first.

  out <- out[c(calc.by, "exec.qty", "cost")]
  
  return(out)
}

.getHeadTail <- function(out, num.trades, order.by = "cost"){
  
  ## sort data frame by is
  
  out <- out[order(out[[order.by]], na.last = NA),]

  ## Re-order the row names
  
  rownames(out) <- NULL

  out <- rbind(head(out, n = num.trades), tail(out, n = num.trades))

  return(out)
}


setMethod("calcStats",
          signature(batches = "data.frame"),
          function(batches){

            if(nrow(batches) > 0){
              out <- data.frame(total.mkt.value =
                                sum(batches$exec.mkt.val))
              
              out$first.period <- min(batches$start.period)
              out$last.period  <- max(batches$end.period)
              
              out$total.exec.cost <- sum(batches$execution.cost)
              out$total.opp.cost  <- sum(batches$opportunity.cost)
              out$total.is        <- sum(batches$cost)
              
              return(out)
            }
            else{
              out <- data.frame(total.mkt.value = 0,
                                first.period    = NA,
                                last.period     = NA,
                                total.exec.cost = 0,
                                total.opp.cost  = 0,
                                total.is        = 0)
            }
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
