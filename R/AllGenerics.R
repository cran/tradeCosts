
## tradeCostsResults class methods

if(!isGeneric("pdfSummary"))
  setGeneric("pdfSummary",              
             function(object,
                      ...) standardGeneric("pdfSummary"))


if(!isGeneric("calcAll"))
  setGeneric("calcAll",              
             function(results,
                      num.trades,
                      ...) standardGeneric("calcAll"))

if(!isGeneric("calcID"))
  setGeneric("calcID",              
             function(results,
                      num.trades,
                      ...) standardGeneric("calcID"))

if(!isGeneric("calcPeriod"))
  setGeneric("calcPeriod",              
             function(results,
                      num.trades,
                      ...) standardGeneric("calcPeriod"))

if(!isGeneric("calcStats"))
  setGeneric("calcStats",              
             function(results,
                      ...) standardGeneric("calcStats"))

## tradeCosts class methods

if(!isGeneric("analyzeData"))
  setGeneric("analyzeData",
             function(object,
                      base.price,
                      benchmark.price,
                      ...) standardGeneric("analyzeData"))
