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
{ context("withpseed")
  seed <- seeds[[1]]
  noattr(a <- withpseed(seed, rnorm(10)))
  noattr(b <- withpseed(seed, rnorm(10)))
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
    b <- withpseed(seed, sample(1:10))
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
