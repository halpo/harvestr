foreach::registerDoSEQ()

context("Grafting")
test_that("graft is reproducible", if (requireNamespace('MCMCpack')) {
    results <-
    replicate(2, simplify=F, {
        datasets <- farm(gather(3, seed = 20120604), {
            x1 <- rnorm(100)
            x2 <- rnorm(100)
            y <- rbinom(100, 1, p=plogis(x1 + x2))
            data.frame(y, x1, x2)
        }, .progress='none', .parallel=FALSE)

        substreams <- plyr::llply(datasets, graft, 10)
        subchains <- harvest(substreams[[1]], MCMCpack::MCMCregress
                            , formula=y~x1+x2, n=100, .progress='none', .parallel=FALSE)
    })
    expect_identical(noattr(results[[1]]), noattr(results[[2]]))
})

