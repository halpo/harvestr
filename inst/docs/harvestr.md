# Using `harvestr` for replicable simulations

```knitr global-setup, echo=F, results='hide', message=F, warning=F
render_gfm()
library(markdown)
print.data.frame <- function(x, ...){
  print(ascii(df, include.rownames = FALSE), type = 'rest')
}
```

## Introduction
The `harvestr` package is a new approach to simulation studies that facilitates 
parallel execution.  It builds off the structures available in the `plyr`, `foreach`
and `rsprng` packages.  What `harvestr` brings to the picture is abstractions of 
the process of performing simulation.

## Process
The theme of `harvestr` is that of gardening, stemming from the idea that
the pseudo-random numbers generated (RNG) in replicable simulation come from 
initial states called seeds.  Figure 1 shows the basic process for `harvestr`.

![The basic `harvestr` process](http://yuml.me/activity/draw/
(start)->(gather(n))->[parallel seeds]->(farm(seeds, expr))->[data sets]->(harvest(data, fun))->(end))

The ideas are simple.

#. `gather(n, [seed])` takes an integer for the number of seeds to generate.  
    Optionally, the seed can be set for replicable simulations.  This uses the 
    `rsprng` library to initialize independent parallel random number streams.
#. The seeds that are returned from `gather` are then fed into the `farm` 
    function along with an expression to be generate data.  `farm` returns
    a list of data frames each independently generated under each of the rng 
    streams.
#. The final step is to apply a function to analyze the data.  This is done
   with the `harvest` command, which takes the data from `farm` and applies
   an analysis function to the dataset.  In the case that the analysis is
   deterministic `harvest` is equivalant to `llply` from the `plyr` package.
   The difference is with stochastic analysis, such as Markov Chain Monte
   Carlo (MCMC), where `harvest` resumes the RNG stream where `farm` left
   off when generating the data.
   
The effect is the results can be taken in any order and independently, and 
the final results are the same as if each analysis was taken from start to 
end with setting a single seed for each stream.

## Example 1 - Basic Example
Some learn best by example.  Here I will show a simple example for the basic
process.  Here we will perform simple linear regression for 100 data sets.
First off we gather the seeds.  This step is separate to facilitate storing
the seeds to be distributed along with research if necessary.

```knitr ex1-setup
library(harvestr)
library(plyr)
library(dostats)
seeds <- gather(100, seed=12345)
```

Second, we generate the data.

```knitr ex1-generate_data
datasets <- farm(seeds, {
  x <- rnorm(100)
  y <- rnorm(100, mean=x)
  data.frame(y,x)
})
```

Then we analyze the data.

```kntir ex1-analysis
analyses <-  harvest(datasets, lm)
```

So what do we have in `analyses`?  We have whatever `lm` returned.  In this 
case we have a list of `lm` objects containg the results of a linear regression.
Ussually we will want to do more to summarize the results.

```knitr ex1-summarize, results='hide'
coefs <- t(sapply(analyses, coef))
adply(coefs,2, dostats, mean, sd)
```

## Example 2 - Stochastic Analysis

That is very nice, but rather simple as far ananalyses go.  What might be more interesting is to
perform an analysis with a random component to the analysis. 

## Example 3 - Caching