{###############################################################################
 # test-timing.R
 # This file is part of the R package harvestr.
 #
 # Copyright 2012 Andrew Redd
 # Date: 8/14/2012
 #
 # DESCRIPTION
 # ===========
 # unit testing for function to tests correct timing functionality.
 #
 # LICENSE
 # ========
 # harvestr is free software: you can redistribute it and/or modify it under the
 # terms of the GNU General Public License as published by the Free Software
 # Foundation, either version 3 of the License, or (at your option) any later
 # version.
 #
 # This file is distributed in the hope that it will be useful, but WITHOUT ANY
 # WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 # FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License along with
 # this file If not, see http://www.gnu.org/licenses/.
 #
}###############################################################################
library(testthat)
library(harvestr)
foreach::registerDoSEQ()
context("Timing")

make_has_attr <- function(name, class='any'){
    function(x){
        a <- attributes(x)

        if (!(name %in% names(a)))
            structure(FALSE, msg = sprintf("Does not have '%s' attribute", name)) else
        if (class != 'any' && ! inherits(a[[name]], class))
            structure( FALSE
                     , msg = sprintf("has '%s' attribute but it is not of class '%s'."
                               , name, class)) else
        TRUE
    }
}
has_time <- make_has_attr('time', 'proc_time')
with_option <- function (code, ...) {
    old <- options(...)
    on.exit(options(old))
    force(code)
}


test_that("withseed times results", {
    seed <- gather(1)[[1]]
    expect_true(has_time(withseed(seed, runif(1), time=TRUE)))
    expect_true(has_time(with_option(withseed(seed, runif(1)), harvestr.time=TRUE)))
})
test_that('farm times results', {
    seeds <- gather(3)
    expect_true(has_time(farm(seeds, runif(10), time=TRUE, .progress='none', .parallel=FALSE)))
    expect_true(has_time(with_option(farm(seeds, runif(10), .progress='none', .parallel=FALSE), harvestr.time=T)))
})
test_that('reap times results', {
    x <- farm(1, runif(100), .progress='none', .parallel=FALSE)[[1]]
    expect_true(has_time(reap(x, mean, time=TRUE)))
    expect_true(has_time(with_option(reap(x, mean), harvestr.time=T)))
})
test_that('harvest times results', {
    x <- farm(3, runif(100), .progress='none', .parallel=FALSE)
    expect_true(has_time(harvest(x, mean, time=TRUE, .progress='none', .parallel=FALSE)))
    expect_true(has_time(with_option(harvest(x, mean, .progress='none', .parallel=FALSE), harvestr.time=T)))
})
