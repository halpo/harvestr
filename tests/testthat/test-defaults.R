{###############################################################################
# test-defaults.R
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
context("defaults")

test_that("parallel", {
    foreach::registerDoSEQ()
    expect_true(getDoParRegistered())
    expect_true(dflt_harvestr_parallel())
})
test_that("others", {
    expect_false(dflt_harvestr_time())
    expect_false(dflt_harvestr_cache())
    expect_equal(dflt_harvestr_cache_dir(), "harvestr-cache")
    expect_equal(dflt_harvestr_use_try(), !interactive())
})


