

RNGkind("L'Ecuyer-CMRG")
set.seed(123)
(s <- .Random.seed)

r<-nextRNGStream(s)
replicate(10, nextRNGSubStream(r), simplify=F)

n <- 10


l

a <-
llply(l, withseed, runif(10), environment())
b <-
llply(l, withseed, runif(10), environment())
identical(a,b)

a <- replicate(3, {
    r <<- nextRNGStream(s)
    replicate(3, r <<- nextRNGSubStream(r), T)
}, F)

b <- replicate(3, {
    r <<- nextRNGStream(s)
    runif(10)
    replicate(3, r <<- nextRNGSubStream(r), T)
}, F)




nextRNGSubStream(s)
set.seed(r)
a <- runif(10)

nextRNGStream(s)
nextRNGSubStream(s)

set.seed(r)
b <- runif(10)

set.seed(1)
a <- runif(10)

set.seed(1)
b <- runif(10)




