library(harvestr)
library(testthat)
cache.dir <- normalizePath(file.path(tempdir(), "harvestr-cache"), mustWork=F)
options(harvestr.cache.dir=cache.dir)
reg.finalizer(environment(), function(){unlink(cache.dir, TRUE)}, onexit=TRUE)

long_function <- function(wait=15){
  # allow ne to read the ether 
  Sys.sleep(wait)
  paste("Your luck lotto numbers are:", 
    paste(sample(56, 5), collapse=" "),
    '|', sample(46, 1), sep=' ')
}
{ context("timings")
  expect_that(withpseed(gather(1)[[1]], long_function(9)), takes_less_than(10))
  expect_that(withpseed(gather(1)[[1]], long_function(9)), takes_less_than(11))
}
{ context("caching")
  seed <- gather(1)[[1]]
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  t1 <- system.time(run1 <- withpseed(seed, long_function(10), cache=T))
  t2 <- system.time(run2 <- withpseed(seed, long_function(10), cache=T))
  expect_true(all((t2 <= t1)[1:3]))
  expect_identical(run1, run2)
  
  seeds <- gather(100)
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  t1 <- system.time(run1 <- farm(seeds, mean(rnorm(1e6)), cache=T))
  t2 <- system.time(run2 <- farm(seeds, mean(rnorm(1e6)), cache=T))
  expect_true(all((t1 >= t2)[1:3]))
  expect_that(run1, is_identical_to(run2))
  
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  x <- plant( list(1:1e7), list(seed))[[1]]
  t1 <- system.time(run1 <- reap(x, sample, cache=T))
  t2 <- system.time(run2 <- reap(x, sample, cache=T))
  expect_true(all((t2 <= t1)[1:3]))
  expect_identical(run1, run2)    
  unlink(cache.dir, recursive=TRUE, force=TRUE)
}
