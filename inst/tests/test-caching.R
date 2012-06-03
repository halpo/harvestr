{###############################################################################
# test-caching.R
# This file is part of the R package harvestr.
# 
# Copyright 2012 Andrew Redd
# Date: 6/2/2012
# 
# DESCRIPTION
# ===========
# test the caching facilities.
# 
# LICENSE
# ========
# harvestr is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# dostats is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# dostats. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################
library(harvestr)
library(testthat)
library(dostats)
context("Caching")

cache.dir <- normalizePath(file.path(tempdir(), "harvestr-cache"), mustWork=F)
options(harvestr.cache.dir=cache.dir)
reg.finalizer(emptyenv(), function(...){unlink(cache.dir, TRUE)}, onexit=TRUE)

long_function <- function(wait=15){
  # allow ne to read the ether 
  Sys.sleep(wait)
  paste("Your luck lotto numbers are:", 
    paste(sample(56, 5), collapse=" "),
    '|', sample(46, 1), sep=' ')
}
takes_at_least <-function (amount) {
    function(expr) {
        duration <- system.time(force(expr))["elapsed"]
        expectation(duration > amount, 
          sprintf("took %s seconds, which is more than %s", duration, amount))
    }
}
test_that("caching", {
  seed <- gather(1)[[1]]
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  t1 <- system.time(run1 <- withseed(seed, long_function(10), cache=T))
  t2 <- system.time(run2 <- withseed(seed, long_function(10), cache=T))
  expect_true(all(t2['elapsed'] <= t1['elapsed']))
  expect_identical(run1, run2)
  
  seeds <- gather(10)
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  t1 <- system.time(run1 <- farm(seeds, mean(rnorm(1e6)), cache=T))
  t2 <- system.time(run2 <- farm(seeds, mean(rnorm(1e6)), cache=T))
  expect_true(all(t2['elapsed'] <= t1['elapsed']))
  expect_identical(run1, run2)
  
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  long_sample <- compose(head, sample)
  x <- plant( list(1:1e7), list(seed))[[1]]
  t1 <- system.time(run1 <- reap(x, long_sample, cache=T))
  t2 <- system.time(run2 <- reap(x, long_sample, cache=T))
  expect_true(all(t2['elapsed'] <= t1['elapsed']))
  expect_identical(run1, run2)    
  unlink(cache.dir, recursive=TRUE, force=TRUE)
})
