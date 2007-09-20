################################################################################
##
## $Id: tradeCostsResults.test.R 1204 2007-09-20 11:02:25Z enos $
##
## Test for tradeCostResults object
##
################################################################################

library(tradeCosts)

load("tradeCostsResults.test.RData")
load("tradeCostsResults.test2.RData")
load("tradeCostsResults.test3.RData")

## save(trade.example, dynamic.example, static.example, batches.truth, file = "tradeCostsResults.test.RData")
## save(trade.empty, dynamic.empty, empty.truth, file = "tradeCostsResults.test2.RData")
## save(trade.na, dynamic.na, na.truth, file = "tradeCostsResults.test3.RData")

tc <- new("tradeCosts", name         = "Test",
                        trade.data   = trade.example, 
                        dynamic.desc = dynamic.example,
                        static.desc  = static.example)

tc.empty <- new("tradeCosts", name         = "Empty",
                              trade.data   = trade.empty,
                              dynamic.desc = dynamic.empty)

tc.na <- new("tradeCosts", name         = "NA",
                           trade.data   = trade.na,
                           dynamic.desc = dynamic.na)

tr <- tradeCosts:::analyzeData(tc,
                               base.price      = "exec.price",
                               benchmark.price = "decision.price")

tr.empty <- tradeCosts:::analyzeData(tc.empty,
                                     base.price      = "exec.price",
                                     benchmark.price = "vwap")


tr.na <- tradeCosts:::analyzeData(tc.na,
                                  base.price      = "exec.price",
                                  benchmark.price = "vwap")
batches.test  <- tr@batches
batches.empty <- tr.empty@batches
batches.na    <- tr.na@batches

stopifnot(
          isTRUE(all.equal(batches.truth, batches.test)),
          isTRUE(all.equal(empty.truth, batches.empty)),
          isTRUE(all.equal(na.truth, batches.na))
          )
