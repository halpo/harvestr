library(harvestr)
library(testthat)
library(plyr)
if(require(doParallel)) {
  cl=makeCluster(2)
  clusterEvalQ(cl, (library(harvestr)))
  registerDoParallel(cl)
}
{ context("parallel")
  if(require(doParallel)) {
    seed=gather(1, seed=1234)[[1]]
    r <- foreach(seed = replicate(4, seed, simplify=F)) %do%  
      withpseed(seed, rnorm(1e5))
    s <- foreach(seed = replicate(4, seed, simplify=F)) %dopar%
      withpseed(seed, rnorm(1e5))

    # r <- llply(replicate(12, seed, simplify=F), withpseed, function(...)rnorm(10000))
    # s <- llply(replicate(12, seed, simplify=F), withpseed, function(...)rnorm(10000), .parallel=T)
    expect_true(all(laply(r, all.equal, r[[1]], check.attributes=F)))
    expect_true(all(laply(s, all.equal, s[[1]], check.attributes=F)))
    seeds <- gather(100, seed=1234)
    e <- farm(seeds, rnorm(10))
    x <- harvest(e, sample, replace=T)
    z <- harvest(e, sample, replace=T, .parallel=T)
    expect_equivalent(x,z)
  }
}
