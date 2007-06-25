
setMethod("analyzeData",
          signature(object          = "tradeCosts",
                    base.price      = "character",
                    benchmark.price = "character"),
          function(object, base.price, benchmark.price){
            num.trades <- nrow(object@trade.data)
          
            ## merge data sets
            
            merged <- merge(object@trade.data,
                            object@dynamic.desc,
                            by = c("id", "period"),
                            all.x = TRUE)

            ## add static descriptive data
            
            if(!(nrow(object@static.desc) == 0)){
              merged <- merge(merged, object@static.desc,
                              by = "id", all.x = TRUE)
            }
            
            ## NA reporting
            
            na.num                 <-  data.frame(side = .na.count(merged$side))
            na.num$base.price      <- .na.count(merged[[base.price]])
            na.num$benchmark.price <- .na.count(merged[[benchmark.price]])
            na.num$exec.qty        <- .na.count(merged$exec.qty)
            
            ## create tradeCostsResults object
            
            results <- new("tradeCostsResults", name = object@name,
                           results = merged, na.counts = na.num)
            
            ## return object with calculation performed
            
            invisible(.updateCost(results, base.price, benchmark.price))

            ## check for existance of exec.qty
        })
            
.updateCost <-  function(object, base.price, benchmark.price){

  ## start column names vector

  cols <- c("id", if("symbol" %in% names(object@results)){"symbol"}, "period",
            "side", "exec.qty", base.price,  benchmark.price,
            "exec.mkt.val", "percent.cost", "cost")
            
  ## check for adjustment factor

  if("adjustment.factor" %in% names(object@results)){
    object@results[[base.price]]      <- object@results[[base.price]] /
                                         object@results$adjustment.factor
    object@results[[benchmark.price]] <- object@results[[benchmark.price]] /
                                         object@results$adjustment.factor
    object@results$exec.qty           <- object@results$exec.qty *
                                         object@results$adjustment.factor
  }
  
  ## create new column of spread
  
  object@results$cost  <- object@results[[base.price]] -
                          object@results[[benchmark.price]]
  
  ## remove NAs for analysis
  
  object@results <- object@results[!is.na(object@results$side),]
  #### remove more NAs?

  
  ## calculate trade cost
  
  object@results$cost <- ifelse(object@results$side %in%
                                c("X","S"),-1,1) * object@results$cost
  object@results$cost <- object@results$cost *  object@results$exec.qty


  ## calculate market value
  
  object@results$exec.mkt.val <- object@results$exec.qty*object@results$exec.price
  
  ## calculate percent cost

  object@results$percent.cost <- (object@results$cost /
                                 object@results$exec.mkt.val)*100
  
  
  ## pare down data

  object@results <- object@results[cols]
 
  invisible(object)
}

.na.count <- function(x){
  return(sum(is.na(x)))
}



