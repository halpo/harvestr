library(lme4)
library(dostats)
library(plyr)

context("Grafting")
test_that("graft replicability", {
results <- 
replicate(2, simplify=F, {
    datasets <- farm(gather(3, seed = 20120604), {
        x1 <- rnorm(100)
        x2 <- rnorm(100)
        g <- rep(rnorm(10), each=10)
        y <- rbinom(100, 1, p=plogis(x1 + x2 + g))
        data.frame(y, x1, x2, g = seq_consecutive(g))
    })

    analyses   <- harvest(datasets, lmer, formula = y~x1 + x2 + (1|g))
    substreams <- llply(analyses, graft, 10)
    subchains <- harvest(substreams[[1]], mcmcsamp, n=1000)
})
expect_identical(noattr(results[[1]]), noattr(results[[2]]))
})

