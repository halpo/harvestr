{###############################################################################
# utils.R
# This file is part of the R package harvestr.
# 
# Copyright 2012 Andrew Redd
# Date: 6/2/2012
# 
# DESCRIPTION
# ===========
# Helper utilities for working with harvestr functions.
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

#' Use a reference class method
#' @param method name of the method to call
#' @param ... additional arguments to pass along
#' 
#' @seealso \link{ReferenceClasses}
#' @return a function that calls the designated meethod
#' @example inst/examples/use_method.R
#' @export
use_method <- function(method, ...){
  method <- as.character(substitute(method))
  function(x){
    fun <- do.call(`$`, list(x, method))
    fun(...)
  }
}

#' retrieve the total time for a simulation
#' 
#' @param x a list from harvest
#' @export
total_time <- function(x){
    times <- sapply(x, attr, 'time')
    sum(times['elapsed',])
}
