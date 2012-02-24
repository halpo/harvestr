
suppressWarnings(suppressMessages({
seeds  <- gather(10, seed=1234)
seeds0 <- gather(10, seed=1234)
}))
isTRUE(all.equal(seeds, seeds0))

seed <- seeds[[1]]

a <- noattr(withpseed(seed, rnorm(10)))
b <- noattr(withpseed(seed, rnorm(10)))
c <- rnorm(10)
isTRUE(all.equal(a,b))
isTRUE(all.equal(a,c))  # FALSE

a <- farm(seeds, rnorm(10))
b <- farm(seeds, rnorm(10))
isTRUE(all.equal(a,b))

x <- harvest(a, sample, replace=T)
y <- harvest(a, sample, replace=T)
identical(x,y)


# test parallel
require(doMC)
registerDoMC()
z <- harvest(a, sample, replace=T, .parallel=T)
identical(x,z)

# test parallel
r <- llply(replicate(100, seed, simplify=F), withpseed, function()rnorm(10000))
s <- llply(replicate(100, seed, simplify=F), withpseed, function()rnorm(10000), .parallel=T)
all(laply(r, identical, r[[1]]))
all(laply(s, identical, s[[1]]))

# test with use


