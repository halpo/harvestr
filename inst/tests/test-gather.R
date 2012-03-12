library(harvestr)
library(testthat)
library(plyr)

test_that("testing",{
message("Ignore printedwarnings they are expected, and impossible to supress.")
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
  expect_identical(e,f)
}
{ context("reap")
  expect_identical(reap(a, sample), reap(a, sample))
}
{ context("harvest")
  x <- harvest(e, sample, replace=T)
  y <- harvest(e, sample, replace=T)
  expect_identical(x,y)
}
{ context("using with")
  data <- farm(gather(3, seed=1234), data.frame(x123=runif(100), y456=rnorm(100)))
  m1 <- harvest(data, with, mean(x123))
  m2 <- lapply(data, with, mean(x123))
  expect_equivalent(noattr(m1), noattr(m2))
}
})
