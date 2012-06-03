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
library(harvestr)
library(testthat)
library(plyr)

test_that("testing", {
# message("Ignore printed warnings they are expected, and impossible to supress.")
{ context("gather")
  suppressWarnings(suppressMessages({
    seeds  <- gather(10, seed=1234)
    seeds0 <- gather(10, seed=1234)
  }))
  expect_identical(seeds, seeds0)
}
{ context("withseed")
  seed <- seeds[[1]]
  noattr(a <- withseed(seed, rnorm(10)))
  noattr(b <- withseed(seed, rnorm(10)))
  c <- rnorm(10)
  expect_identical(a, b)
  expect_false(identical(a, c))  # FALSE
}
{ context("farm")
  seeds <- gather(10)
  e <- farm(seeds, rnorm(10))
  f <- farm(seeds, rnorm(10))
  expect_equivalent(e,f)
  
  seeds <- gather(10)
  o <- sample(seq_along(seeds))
  g <- farm(seeds[o], rnorm(10))[order(o)]
  expect_equivalent(e,g)
}
{ context("reap")
  expect_equivalent(reap(a, sample), reap(a, sample))
  local({
    seed <- gather(1)[[1]]
    x <- plant(list(1:10), list(seed))[[1]]
    a <- reap(x, sample)
    b <- withseed(seed, sample(1:10))
    expect_identical(a,b)
  })
}
{ context("harvest")
  x <- harvest(e, sample, replace=T)
  y <- harvest(e, sample, replace=T)
  expect_equivalent(x,y)
}
{ context("Permutation")
  x <- harvest(e, sample, replace=T)
  o <- sample(seq_along(e))
  y <- harvest(e[o], sample, replace=T)[order(o)]
  expect_equivalent(x,y)
}
{ context("using with")
  data <- farm(gather(3), data.frame(x123=runif(100), y456=rnorm(100)))
  m1 <- harvest(data, with, mean(x123))
  m2 <- lapply(data, with, mean(x123))
  expect_equivalent(m1, m2)
}
})
