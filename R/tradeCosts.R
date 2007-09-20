################################################################################
##
## $Id: tradeCosts.R 1220 2007-09-21 03:05:18Z enos $
##
## Methods for tradeCosts class
##
################################################################################

setMethod("analyzeData",
          signature(object = "tradeCosts"),

          function(object, base.price = "exec.price", 
                   benchmark.price, batch.method = "unique"){

            ## merge data sets
            ## we make sure to sort by id and period so that
            ## decision columns can be added on in the right order
            
            merged <- merge(object@trade.data,
                            object@dynamic.desc,
                            by    = c("id", "period"),
                            all.x = TRUE,
                            sort  = TRUE)

            ## A special check is needed if benchmark price == decision.price
            ## To check it just make - sure prior.close data is not NA
            ## since decision price is based off prior.close
            ## Also NA counting should be performed with prior.close data
            ## subsituting as the benchmark.price
            
            if(nrow(object@decisions.data) == 0 &&
               isTRUE(all.equal(benchmark.price,"decision.price"))){
              merged <- merged[!is.na(merged$prior.close),]
              na.num <- .na.calc(merged, base.price, "prior.close")
            }
            else{
              merged <- merged[!is.na(merged[[benchmark.price]]),]
              na.num <- .na.calc(merged, base.price, benchmark.price)
            }
          
            ## remove bad NA's, remove rows with NAs in:
            ## side, id, exec.qty, base.price, benchmark.price, period
            
            merged <- merged[!is.na(merged$side),]
            merged <- merged[!is.na(merged$id),]
            merged <- merged[!is.na(merged$exec.qty),]
            merged <- merged[!is.na(merged[[base.price]]),]
            merged <- merged[!is.na(merged$period),]
             
            ## Add static descriptive data
            
            if(!(nrow(object@static.desc) == 0)){
              merged <- merge(merged, object@static.desc,
                              by = "id", all.x = TRUE)
            }
                    
            ## If there is no decisions data slot then build decisions slots
            ## Note that the data must be sorted by id and period
            
            if(nrow(object@decisions.data) == 0){


              ## add decision column
              
              merged$decision <- .get.decision(merged, batch.method)
              
              ## calculate decision shares
              
              merged$decision.shares <- .get.decision.shares(merged)
              
              if(isTRUE(all.equal(benchmark.price,"decision.price"))){
                
                ## find decision price

                merged$decision.price  <- .get.decision.price(merged,
                                                              "prior.close")
              }
            }
            else{
              
              ## Merge object@decision.data with merged data
              ## this needs to be implemented/or taken care of
                            
            }
                        
            ## calculate individual trade costs
           
            merged$execution.cost <- ifelse(merged$side %in% c("X","S"), -1, 1) *
              (merged$exec.qty * merged[[base.price]] -
              merged$exec.qty * merged[[benchmark.price]])

            merged$pct.exe.cost <- 100 * merged$execution.cost / (merged$exec.qty * merged[[base.price]])
            
            ## calculate trade batches
            
            batches <- get.is(merged, base.price, benchmark.price)
            
            ## create tradeCostsResults object
            
            results <- new("tradeCostsResults",
                           name            = object@name,
                           executions      = merged,
                           batches         = batches,
                           na.counts       = na.num,
                           base.price      = base.price,
                           benchmark.price = benchmark.price,
                           batch.method    = batch.method)
         
            ## return object with calculation performed
            
            invisible(.updateCost(results,
                                  base.price,
                                  benchmark.price,
                                  batch.method))
          })

## Private function to create decisions IDs

.get.decision <- function(x, batch.method){
  
  if(nrow(x) < 1 ){
    return(numeric())
  }
  
  ## check for required columns.
  
  stopifnot(c("id", "side", "period") %in% names(x))
  
  ## Set initial decision IDs
  
  x$decision <- 1:dim(x)[1]
  
  if(isTRUE(all.equal(batch.method, "same.sided"))){
    
    ## Split data by ID
    
    by.id <- split(x, x$id)
    
    if(nrow(x) > 1){
      
      ## Adjust decision IDs for consecutive same-sided, same-ID trades
      
      x <- do.call(rbind, lapply(by.id, function(x){
        if(length(x$decision) > 1 && !all(is.na(x$decision))){
          for(i in 2:length(x$decision)){
            if(isTRUE(all.equal(x$side[i], x$side[i-1]))){ 
              x$decision[i] <- x$decision[i-1]
            }
          }
        }
        return(x)
      }))
      
      ## Create vectors of the unique decisions and their ranks
      
      unique.decision <- unique(x$decision)
      unique.order    <- rank(unique.decision)
      
      ## Re order the decision numbers so they are consecutive
      
      for(i in 1:length(x$decision)){
        x$decision[x$decision == unique.decision[i]] <- unique.order[i]
      }
    }
  }
  
  return(x$decision)
  
}

## Private function to find the number of shares per decision

.get.decision.shares <- function(x){
  
  if(nrow(x)<1){
    return(numeric())
  }

  ## check for required columns
  
  stopifnot(c("id", "side", "period", "exec.qty") %in% names(x))

  ## Split data by decision ID
  
  by.id <- split(x, x$decision)
  
  ## Find number of decision shares
  
  dec.shares <- do.call(rbind, lapply(by.id, function(x){
    data.frame(id = x$id[1],
               side = x$side[1],
               decision.shares = sum(x$exec.qty,
                 na.rm = TRUE))}))
  
  dec.shares$decision <- rownames(dec.shares)
  
  ## merge with original data
  
  x <- merge(x, dec.shares, by = c("id", "decision"))
  
  return(x$decision.shares)
  
}

## Private function to find the price per decision

.get.decision.price <- function(x, d.price = "prior.close"){
  
  if(nrow(x)<1){
    return(numeric())
  }
  
  ## check for required columns
  
  stopifnot(c(d.price, "id", "side", "period", "decision") %in% names(x))

  ## Split data by decision ID

  by.id <- split(x, x$decision)

  ## Get prices for every decision ID

  dec.prices <- do.call(rbind,
                        lapply(by.id,
                               function(x){
                                 data.frame(id  = x$id[1],
                                            side           = x$side[1],
                                            decision.price = x[[d.price]][1],
                                            decision.adj   = ifelse("adjustment.factor" %in%
                                              names(x), x$adjustment.factor[1], NA))
                               }))
  
  ## Set the dec.prices rownames as decision IDs
  
  dec.prices$decision <- rownames(dec.prices)
  
  ## Merge decision prices with original data
  
  x <- merge(x, dec.prices, by = c("id", "side", "decision"))

  ## adjustment factor used to adjust decision price this will divide
  ## the decision price by the ratio between the decision price's
  ## adjustment factor and each executions adjustment factor, allowing
  ## for multiple splits
  
  if("adjustment.factor" %in% names(x)){
    
    ## At this point decision.price = the decision.price for the first
    ## period.  Divide the decision price of the first period by the
    ## ratio of the adjustment factor for the decision's first period
    ## by the adjustment factor for each period.
    
    x$decision.price <- x$decision.price / (x$decision.adj / x$adjustment.factor)
  }
  
  return(x$decision.price)
}

.updateCost <-  function(object, base.price, benchmark.price, batch.method){

  ## Right now this vector of names is pointless. remove later?
  ## start column names vector

  cols <- c("id", "decision",  
            if("symbol" %in% names(object@executions)){"symbol"}, 
            "period", "side", "exec.qty", base.price,  benchmark.price,
            "exec.mkt.val", "decision.shares",
            if("adjustment.factor" 
               %in% names(object@executions)){"adjustment.factor"})
            
  
  ## calculate market value
  
  object@executions$exec.mkt.val <- object@executions$exec.qty*
    object@executions[[base.price]]
  
  ## pare down data - for now, dont. 
  
  ## object@executions <- object@executions[cols]
  
  invisible(object)
}

## calculate NA statistics

.na.calc <- function(x, base.price, benchmark.price){

  na.num                    <- data.frame(id = .na.count(x$id))
  na.num$period             <- .na.count(x$period)
  na.num$side               <- .na.count(x$side)
  na.num[[base.price]]      <- .na.count(x[[base.price]])
  na.num$exec.qty           <- .na.count(x$exec.qty)
  na.num[[benchmark.price]] <- .na.count(x[[benchmark.price]])
  
  invisible(na.num)
}

## utility function for calculating number of NAs in a vector

.na.count <- function(x){
  return(sum(is.na(x)))
}


