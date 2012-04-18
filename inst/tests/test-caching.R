library(harvestr)
library(testthat)
library(dostats)
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
takes_at_least <-function (amount) 
{
    function(expr) {
        duration <- system.time(force(expr))["elapsed"]
        expectation(duration > amount, 
          sprintf("took %s seconds, which is more than %s", duration, amount))
    }
}
{ context("timings")
  if (file.exists(cache.dir)) unlink(cache.dir, recursive=TRUE, force=TRUE)
  expect_that(withpseed(gather(1)[[1]], long_function(9), cache=T), takes_at_least(9))
  expect_that(withpseed(gather(1)[[1]], long_function(9), cache=T), takes_less_than(9))
}
{ context("caching")
  seed <- gather(1)[[1]]
  unlink(cache.dir, recursive=TRUE, force=TRUE)
  t1 <- system.time(run1 <- withpseed(seed, long_function(10), cache=T))
  t2 <- system.time(run2 <- withpseed(seed, long_function(10), cache=T))
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
}
