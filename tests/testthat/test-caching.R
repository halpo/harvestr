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
# This file is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this file If not, see http://www.gnu.org/licenses/.
#
}###############################################################################
library(harvestr)
library(testthat)
library(boot)
foreach::registerDoSEQ()
context("Caching")


cache.dir <- normalizePath(file.path(tempdir(), "harvestr-cache"), mustWork=FALSE)
oo <- options( harvestr.cache.dir=cache.dir
             )
reg.finalizer(emptyenv(), function(...){unlink(cache.dir, TRUE)}, onexit=TRUE)

long_function <- function(){
    # should take about a second
    rnorm(5e6)
    paste("Your luck lotto numbers are:"
      , paste(sample(56, 5), collapse=" ")
      , '|'
      , sample(46, 1), sep=' ')
}
takes_at_least <-function (amount) {
    function(expr) {
        duration <- system.time(force(expr))["elapsed"]
        expectation(duration > amount,
            sprintf("took %s seconds, which is more than %s", duration, amount))
    }
}
test_that("caching in with seed with long_function", {
    seed <- gather(1)[[1]]
    unlink(cache.dir, recursive=TRUE, force=TRUE)
    t1 <- system.time(run1 <- withseed(seed, long_function(), cache=TRUE))
    t2 <- system.time(run2 <- withseed(seed, long_function(), cache=TRUE))
    expect_true(all(t2[1] <= t1[1]))
    expect_identical(run1, run2)
})
test_that("caching in farm with mean of rnorm", {
    seeds <- gather(10)
    unlink(cache.dir, recursive=TRUE, force=TRUE)
    t1 <- system.time(run1 <- farm(seeds, mean(rnorm(5e6)), cache=TRUE, .progress='none', .parallel=FALSE))
    t2 <- system.time(run2 <- farm(seeds, mean(rnorm(5e6)), cache=TRUE, .progress='none', .parallel=FALSE))
    expect_true(all(t2[1] <= t1[1]))
    expect_identical(run1, run2)
})
test_that("caching in reap with long sample", {
    seed <- gather(1)
    unlink(cache.dir, recursive=TRUE, force=TRUE)
    dir.create(cache.dir, recursive=TRUE)
    expect_true(dir.exists(cache.dir))
    long_sample <- function(...)head(sample(...))
    x <- plant( list(5e7), seed)[[1]]
    hash <- digest(list(x, long_sample, source="harvestr::reap"), algo='md5')

    t1 <- system.time(run1 <- reap(x, long_sample, hash=hash, cache=TRUE, time=FALSE))
    t2 <- system.time(run2 <- reap(x, long_sample, hash=hash, cache=TRUE, time=FALSE))
    expect_true(all(t2['user.self'] <= t1['user.self']), info="Rum time is faster in second call.")
    expect_identical(run1, run2, info="Runs are identical.")

    unlink(cache.dir, recursive=TRUE, force=TRUE)
})
test_that('caching in harvest using boot', {
    unlink(cache.dir, recursive=TRUE, force=TRUE)
    x <- farm(10, rnorm(1e4), cache=TRUE, .progress='none', .parallel=FALSE)
    t1 <- system.time(run1 <- harvest(x, boot, var, 100, cache=TRUE, .progress='none', .parallel=FALSE))
    t2 <- system.time(run2 <- harvest(x, boot, var, 100, cache=TRUE, .progress='none', .parallel=FALSE))
    expect_equal(run1, run2)
    expect_true(noattr(t2[1] <= t1[1]))
})
test_that('caching with timing turned on', {
    unlink(cache.dir, recursive=TRUE, force=TRUE)
    x <- farm(10, rnorm(1e4), cache=TRUE, .progress='none', .parallel=FALSE)
    t1 <- system.time(run1 <- harvest(x, boot, var, 100, cache=TRUE, time=TRUE, .progress='none', .parallel=FALSE))
    t2 <- system.time(run2 <- harvest(x, boot, var, 100, cache=TRUE, time=TRUE, .progress='none', .parallel=FALSE))
    expect_equal(run1, run2)
    expect_true(noattr(t2[1] <= t1[1]))
})

options(oo)
