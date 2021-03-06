################################################################################
##
## $Id: AllGenerics.R 1256 2008-04-20 23:10:31Z enos $
##
## Generic Methods
##
################################################################################

## tradeCostsResults class methods



if(!isGeneric("pdfReport"))
  setGeneric("pdfReport",              
             function(object,
                      ...) standardGeneric("pdfReport"))

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

