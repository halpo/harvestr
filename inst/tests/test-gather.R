library(harvestr)
library(testthat)
library(plyr)

test_that("testing",{
message("Ignore printed warnings they are expected, and impossible to supress.")
{ context("gather")
  suppressWarnings(suppressMessages({
    seeds  <- gather(10, seed=1234)
    seeds0 <- gather(10, seed=1234)
  }))
  expect_identical(seeds, seeds0)
}
{ context("withpseed")
  seed <- seeds[[1]]
  noattr(a <- withpseed(seed, rnorm(10)))
  noattr(b <- withpseed(seed, rnorm(10)))
  c <- rnorm(10)
  expect_identical(a, b)
  expect_false(identical(a, c))  # FALSE
}
{ context("farm")
  e <- farm(seeds, rnorm(10))
  f <- farm(seeds, rnorm(10))
  expect_equivalent(e,f)
  
  o <- sample(seq_along(seeds))
  g <- farm(seeds[o], rnorm(10))[order(o)]
  expect_equivalent(e,g)
}
{ context("reap")
  expect_equivalent(reap(a, sample), reap(a, sample))
}
{ context("harvest")
  x <- harvest(e, sample, replace=T)
  y <- harvest(e, sample, replace=T)
  expect_equivalent(x,y)
}
{ context("using with")
  data <- farm(gather(3, seed=1234), data.frame(x123=runif(100), y456=rnorm(100)))
  m1 <- harvest(data, with, mean(x123))
  m2 <- lapply(data, with, mean(x123))
  expect_equivalent(m1, m2)
}
{ context("caching")
  cache.dir <- file.path(tempdir(), "cache")
  options(cache.dir=cache.dir)
  seed <- gather(1)[[1]]
  t1 <- system.time(run1 <- withpseed(seed, mean(rnorm(1e7)), cache=T))
  t2 <- system.time(run2 <- withpseed(seed, mean(rnorm(1e7)), cache=T))
  expect_true(all((t2 < t1)[1:3]))
  expect_identical(run1, run2)
  unlink(cache.dir, TRUE)
}
})
