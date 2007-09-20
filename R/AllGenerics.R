################################################################################
##
## $Id: AllGenerics.R 1150 2007-08-24 15:15:32Z suo $
##
## Generic Methods
##
################################################################################

## tradeCostsResults class methods



if(!isGeneric("pdf"))
  setGeneric("pdf",              
             function(object,
                      ...) standardGeneric("pdf"))

if(!isGeneric("calc"))
  setGeneric("calc",              
             function(object,
                      by,
                      ...) standardGeneric("calc"))

if(!isGeneric("calcAll"))
  setGeneric("calcAll",              
             function(batches,
                      num.trades,
                      ...) standardGeneric("calcAll"))

if(!isGeneric("calcID"))
  setGeneric("calcID",              
             function(batches,
                      num.trades,
                      ...) standardGeneric("calcID"))

if(!isGeneric("calcPeriod"))
  setGeneric("calcPeriod",              
             function(batches,
                      num.trades,
                      ...) standardGeneric("calcPeriod"))

if(!isGeneric("calcStats"))
  setGeneric("calcStats",              
             function(batches,
                      ...) standardGeneric("calcStats"))

## tradeCosts class methods

if(!isGeneric("analyzeData"))
  setGeneric("analyzeData",
             function(object,
                      ...) standardGeneric("analyzeData"))

