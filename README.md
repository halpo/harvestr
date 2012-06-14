# The `harvestr` Parallel Simulation Framework.
The `harvestr` package is a framework for conducting replicable parallel 
simulations in [R](http://www.r-project.org).  It builds off the 
the popular [plyr](http://cran.r-project.org/?pacakge=plyr) 
package for split apply combine framework, and the parallel combined 
multiple-recursive generator’ from L'Ecuyer (1999).

Due to the replicable simulations being based off seed values,this package takes a theme of seeds and farming.  The principal functions are as follows:

  * `gather` - Creates a list of parallel rng seeds.
  * `farm` - Uses seeds from `gather` to evaluate expressions after each seed has been set.  This is usefull for generating data.
  * `harvest` - This will take the results from `farm` and continue evaluation with the random number generation where farm left off.  This is useful for the evaluating data generated with farm, through stochastic methods such as Markov Chain Monte Carlo.
  * `reap` - is the single version of harvest for a single element that has appropriately structured seed attributes.
  * `plant` - takes a list of objects, assumed to be of the same class, and gives each element a parallel seed value to use with `harvest` for evaluation.
  * `graft` - splits RNG sub-streams from a main object.
  * `sprout` -  gets the seeds for use in `graft`.

##Lists##
All of the functions work off lists, They expect and return lists, which can be easily converted to data frames.  I would do this with `ldply(list, I)`. 

##Parallel##
The advantage of setting the seeds like this is that parallelization is seamless and transparent, similar to the `plyr` framework each function has a `.parallel` argument, which defaults to `FALSE`, but when set to true will evaluate and run in parallel.  An appropriate parallel backend must be specified.  For example, with a multicore backend you would run the following code.

```r
library(doMC)
regiserDoMC()
```

See the `plyr` and `foreach` packages documentation for what backends are currently supported.

## Operating Systems ##
`harvestr` is limited in it's capabilities by the packages that it depends on, mainly 
[foreach](http://cran.r-project.org/package=foreach)
and [plyr](http://cran.r-project.org/package=plyr)
The Parallel backends are platform limited read the individual packages documentation:
 
  * [doMC](http://cran.r-project.org/?package=doMC)
  * [doSMP](http://cran.r-project.org/?package=doSMP)
  * [doParallel](http://cran.r-project.org/?package=doParallel)
  * [doMPI](http://cran.r-project.org/?package=doMPI)
  * [doRedis](http://cran.r-project.org/?package=doRedis)
  * [doSNOW](http://cran.r-project.org/?package=doSNOW)
