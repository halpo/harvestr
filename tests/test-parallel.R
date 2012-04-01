library(harvestr)
library(testthat)
library(doParallel)
library(plyr)

{ context("parallel")
  if(require(doParallel)) {
    cl=makeCluster(2)
    clusterEvalQ(cl, quote(library(harvestr)))
    registerDoParallel(cl)
    seed=gather(1, seed=1234)[[1]]
    r <- foreach(replicate(12, seed, simplify=F)) %do% withpseed(rnorn(1e5))
    s <- foreach(replicate(12, seed, simplify=F)) %dopar% withpseed(rnorn(1e5))

    r <- llply(replicate(12, seed, simplify=F), withpseed, function(...)rnorm(10000))
    s <- llply(replicate(12, seed, simplify=F), withpseed, function(...)rnorm(10000), .parallel=T)
    expect_true(all(laply(r, identical, r[[1]])))
    expect_true(all(laply(s, identical, s[[1]])))
    seeds <- gather(100, seed=1234)
    e <- farm(seeds, rnorm(10))
    x <- harvest(e, sample, replace=T)
    z <- harvest(e, sample, replace=T, .parallel=T)
    expect_identical(x,z)
    stopCluster(cl)
  }
}
