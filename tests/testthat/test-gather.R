{###############################################################################
# test=gather.R
# This file is part of the R package harvestr.
# 
# Copyright 2012 Andrew Redd
# Date: 6/2/2012
# 
# DESCRIPTION
# ===========
# unit testing for gather and other process flow functions.
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
library(testthat)
library(plyr)
foreach::registerDoSEQ()
context("main functions")
options(harvestr.time=FALSE)

test_that("gather is replicable", {
    seeds  <- gather(10, seed=1234)
    seeds0 <- gather(10, seed=1234)
    expect_identical(seeds, seeds0)
})
test_that("gather gives the appropriate kinds of seed.", {
    .l <- gather(1)
    seed <- .l[[1]]
    replace.seed(seed)
    expect_identical(RNGkind(), c("L'Ecuyer-CMRG", "Inversion"))
    
    x <- plant(list(1), seeds=.l)
    expect_equivalent(gather(x), .l)
})
test_that("gather errors", {
    expect_error(gather(list(a=1,b=2)))
    expect_error(gather('seeds'))
})
test_that("gather replaces seed.", {
    set.seed(123, "Mersenne-Twister", "Box-Muller")
    l <- get.seed()
    seeds <- gather(10)
    k <- get.seed()
    expect_identical(l,k)
    expect_identical(RNGkind(), c("Mersenne-Twister", "Box-Muller"))
})
test_that("farm is replicable", {
    seeds <- gather(10)
    e <- farm(seeds, rnorm(10), cache=FALSE, time=FALSE, .progress='none', .parallel=FALSE)
    f <- farm(seeds, rnorm(10), cache=FALSE, time=FALSE, .progress='none', .parallel=FALSE)
    expect_identical(e,f)
    expect_equivalent(e,f)
})
test_that('farm is indifferent to order.', {
    seeds <- gather(10)
    o <- sample(seq_along(seeds))
    e <- farm(seeds   , rnorm(10), .progress='none', .parallel=FALSE)
    g <- farm(seeds[o], rnorm(10), .progress='none', .parallel=FALSE)[order(o)]
    expect_equivalent(e,g)
})
test_that("reap is reproducible", {
    seed <- gather(1)
    x <- plant(list(1:10), seed)[[1]]
    a <- reap(x, sample)
    b <- withseed(seed[[1]], sample(1:10))
    expect_identical(a,b)
    expect_identical(reap(a, sample), reap(a, sample))
})
test_that("harvest", {
    seeds <- gather(10)
    e <- farm(seeds, rnorm(10), .progress='none', .parallel=FALSE)
    x <- harvest(e, sample, replace=TRUE, .progress='none', .parallel=FALSE)
    y <- harvest(e, sample, replace=TRUE, .progress='none', .parallel=FALSE)
    expect_equivalent(x,y)
})
test_that("Permutation", {
    seeds <- gather(10)
    e <- farm(seeds, rnorm(10), .progress='none', .parallel=FALSE)
    x <- harvest(e, sample, replace=TRUE, .progress='none', .parallel=FALSE)
    o <- sample(seq_along(e))
    y <- harvest(e[o], sample, replace=TRUE, .progress='none', .parallel=FALSE)[order(o)]
    expect_equivalent(x,y)
})
test_that("using with", {
  data <- farm(gather(3), data.frame(x123=runif(100), y456=rnorm(100)), .progress='none', .parallel=FALSE)
  m1 <- harvest(data, with, mean(x123), .progress='none', .parallel=FALSE)
  m2 <- lapply(data, with, mean(x123))
  expect_equivalent(m1, m2)
})
test_that("plow & bale", {
    df <- data.frame(rep=1:10, n=100)
    rngs <- plow(df[-1], rnorm, .parallel=FALSE)
    
    expect_is(rngs, "harvestr-results")
    
    means <- harvest(rngs, mean, .parallel=FALSE, .progress='none')
    expect_is(means, "harvestr-results")
    
    df.means <- bale(means)
    expect_is(df.means, "data.frame")    
})
test_that("bale errors", {
    expect_error(bale(list(1, TRUE)))
    expect_error(bale(1:10))
    expect_error(bale(list()))
    expect_warning(bale(list(a=1, b=2)))
})
test_that("sprout warning", {
    set.seed(20170823)
    seed <- gather(1)[[1]]
    seed2 <- sprout(seed, 10)
    
    expect_warning( sprout(seed2[[1]], 10)
                  , "RNG seed provided is already a substream seed, independence of streams not guaranteed."
                  )
})
test_that("reap parameter use.try", {
    oo <- options(harvestr.try.silent = TRUE)
    seed <- gather(1)
    x <- plant(list(rnorm), seed)[[1]]
    expect_silent(a <- reap(x, sample, use.try=TRUE))
    expect_is(a, "try-error")
    
    expect_error(a <- reap(x, sample, use.try=FALSE))

    options(oo)
})
test_that("farm checks function arguments", {
    seeds <- gather(10, seed = 20170823)
    expect_error(x <- farm(seeds, rnorm))
})
test_that("try_summary", {
    oo <- options( warn=2
                 , harvestr.use.try     = TRUE
                 , harvestr.try.silent  = TRUE
                 , harvestr.try.summary = FALSE
                 )
    seeds <- gather(10, seed = 20170824)
    a <- farm(seeds, rnorm(1))
    b <- harvest(a, rnorm, n=10, mean=0, use.try=TRUE)
    
    expect_output( try_summary(b)
                 , "6 of 10 \\( 60%\\) calls produced errors"
                 )

    options(oo)
})
test_that("plant something besides seeds", {
    expect_error( plant(as.list(letters), LETTERS)
                , 'inherits\\(seeds, "numeric"\\) is not TRUE'
                )
})
test_that("branch", {
    x <- farm(gather(3, seed=20170825), rnorm(1))
    y <- 1:3
    
    results <- branch(rnorm, x[[1]], y, n=100, time=TRUE)
    expect_is(results, "harvestr-results")
})

