{###############################################################################
# withseed.R
# This file is part of the R package harvestr.
#
# Copyright 2012 Andrew Redd
# Date: 6/2/2012
#
# DESCRIPTION
# ===========
# functions for working with random number seeds.
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
# this file. If not, see http://www.gnu.org/licenses/.
#
}###############################################################################

#' Do a computation with a given seed.
#' @rdname seed_funs
#' @param seed      a valid seed value
#' @param expr      expression to evaluate.
#' @param envir     the \code{\link{environment}} to evaluate the code in.
#' @param cache     should results be cached or retrieved from cache.
#' @param cache.dir Where should cached results be saved to/retrieve from.
#' @param time      should results be timed?
#'
#' @details
#' Compute the expr with the given seed, replacing the global seed after compuatations
#' are finished.
#'
#' does not replace the global .Random.seed
#' @note Not parallel compatible, this modifies the global environment, while processing.
#' @importFrom digest digest
#' @seealso \code{\link{set.seed}}
#' @export
withseed <- function(seed, expr, envir=parent.frame()
                    , cache     = getOption('harvestr.use.cache', defaults$cache()    )
                    , cache.dir = getOption("harvestr.cache.dir", defaults$cache.dir())
                    , time      = getOption('harvestr.time'     , defaults$time()     )
                    ){
    oldseed <- get.seed()
    on.exit(replace.seed(oldseed))
    se <- substitute(expr)
    if(cache){
        expr.md5 <- attr(cache, 'expr.md5')
        parent.call <- sys.call(-1)[[1]]
        if(is.null(expr.md5)){
        if(is.call(se))
            expr.md5 <- digest(se , 'md5')
        else
            expr.md5 <- digest(expr, 'md5')
        }
        seed.md5 <- digest(seed, 'md5')
        filename <- paste(expr.md5, '-', seed.md5, ".RData", sep='')
        cache.file <- file.path(cache.dir, filename)
        if(file.exists(cache.file)){
        load(cache.file)
        return(result)
        }
    }
    replace.seed(seed)
    fun <- if(is.name(se)){
        if(is.function(expr)){
            stopifnot(is.null(formals(expr)))
            expr
        } else if(is.call(expr)) {
            as.function(list(expr), envir=envir)
        } else {
            eval(substitute(function()expr), envir=envir)
        }
    } else {
        eval(substitute(function()expr), envir=envir)
    }
    if(time) start.time <- proc.time()
    result <- fun()
    ending.seed <- get.seed()
    attributes(ending.seed) <- attributes(seed) # will carry forward things like RNGStream level.
    result <- structure(result,
        starting.seed = seed,
        ending.seed   = ending.seed,
        time=if(time)structure(proc.time() - start.time, class = "proc_time"))
    if(cache && !inherits(result, 'try-error')){
        if(!file.exists(cache.dir)) dir.create(cache.dir)
        save(result, file=cache.file)
    }
    result
}


#' safe version of retrieving the .Random.seed
#' @rdname seed_funs
#' @return the .Random.seed if defined, otherwise NULL
#' @export
get.seed <- function(){
  if(exists(".Random.seed", envir=.GlobalEnv, mode="numeric")) {
    seed <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
    class(seed) <- c("rng-seed", "integer")
    return(seed)
  } else {
    return(NULL)
  }
}

#' @rdname seed_funs
#' @param delete logical to delete if \code{seed} is null.
#' @details
#' Replaces the .Random.seed with seed unless seed is null, then it will
#' delete the .Random.seed if delete=T
#' @export
replace.seed <- function(seed, delete=TRUE){
  if(is.null(seed)){
    if(delete && exists('.Random.seed', envir=.GlobalEnv, inherits=FALSE))
      remove('.Random.seed', envir=.GlobalEnv)
  } else {
    assign('.Random.seed', noattr(seed), envir=.GlobalEnv)
  }
}

#' Get or Set Current Seed - Safe Version
#' @rdname seed_funs
#'
#' @details
#' Always returns a valid seed.
#' Useful for grabbing a seed used to generate a random object.
#'
#' @return a valid .Random.seed value.
#' @importFrom stats runif
GetOrSetSeed<-function(){
  if(is.null(get.seed())) stats::runif(1)
  seed <- .Random.seed
	seed
}


#' @export
#' @importFrom utils head tail
`format.rng-seed` <-
function( x
        , ...
        ){
    if (length(x) <= 7)
        paste0(RNGkind()[1], "<", paste(sprintf("%X", x[-1]), collapse="/"), ">")
    else
        paste0(RNGkind()[1], "<", digest(tail(head(x, -1), -1), "xxhash64"), "+", tail(x, 1), ">")
}

#' @export
`print.rng-seed` <-
function( x
        , ...
        ){
    print(format(x))
}

#' @export
`format.rng-seeds` <-
function( x
        , ...
        ){
    sapply(x, `format.rng-seed`)
}

#' @export
`print.rng-seeds` <-
function( x                 #< [rng-seeds] object
        , ...
        , max.length  = 10  #< maximum length to not truncate
        , show.length = 6   #< when truncating how many to show
        ){
    o <- format(x, ...)
    if(length(o)>max.length){
        print(head(o, show.length))
        cat("+", length(o) - 6, "more, ", length(o), "in total.\n")
    } else {
        print(o)
    }

}



