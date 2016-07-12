{###############################################################################
# test-Interactive.R
# This file is part of the R package harvestr.
# 
# Copyright 2012 Andrew Redd
# Date: 6/2/2012
# 
# DESCRIPTION
# ===========
# test the caching facilities.
# 
# LICENSE
# ========
# harvestr is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# dostats is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# dostats. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################
library(harvestr)
library(testthat)
context("Interactive")
test_that("testing is_knitting", {
    expect_false(is_knitting())
})
test_that("called from", {
    expect_false(called_from(stats::rnorm))
    expect_true(called_from(test_that))
})
test_that("Interactive", {
    expect_equal(Interactive(), interactive())
})

