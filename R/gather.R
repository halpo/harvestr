#' Gather independent seeds.
#' @param x number of seeds, or an object with seeds to gather
#' @param ... passed on
#' @param .starting if TRUE starting seeds with be gathered rather than ending seeds.
#'
#' Equivalant to \code{\link[rsprng]{spawn.new.sprng}} when x is a number.
#' 
#' @importFrom rsprng spawn.new.sprng
#' @include withseed.R
#' @export
gather <- 
function(x, ..., .starting=F){
  if(is.list(x)){
    seeds <- lapply(x, attr, ifelse(.starting,"starting.seed", "ending.seed"))
    if(any(sapply(seeds, is.null)))
      stop("Malformed list.")
  } else if(is.numeric(x) && isTRUE(all.equal(x,ceiling(x)))){
    seeds <- as.list(as.data.frame(spawn.new.sprng(ceiling(x), ...)))
    seeds <- llply(seeds, structure, class='pseed')
    seeds
  } else {
    stop("x must be either a list or integer")
  }
}

#' @rdname seed_funs
#' @param cache use cache, see Caching in \link{harvestr}
#' @details
#' \code{withpseed} is the same as withseed, but assumes a parallel seed from 
#' \code{\link[rsprng]{spawn.sprng}}.  When evaluated the beginning and endind seeds are
#' saved in the attributes "starting.seed" and "ending.seed", for continued evaluation with 
#' \code{\link{harvest}}
#' 
#' @importFrom rsprng unpack.sprng free.sprng pack.sprng
#' @export
withpseed <- function(seed, expr, envir=parent.frame(), cache=FALSE) {
  stopifnot(inherits(seed, "pseed"))
  if(cache){
    cache.dir <- getOption("harvestr.cache.dir", "harvestr-cache")
    expr.md5 <- attr(cache, 'expr.md5')
    parent.call <- sys.call(-1)[[1]]
    if(is.null(expr.md5)){
      se <- substitute(expr)
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
  oldseed <- get.seed()
  RNGkind("user")
  unpack.sprng(seed)
  on.exit(free.sprng())
  on.exit(replace.seed(oldseed), add=T)
  fun <- if(is.name(substitute(expr)) && is.function(expr)){
    stopifnot(is.null(formals(expr)))
    expr
  } else {
    eval(substitute(function()expr), envir=envir)
  }
  start.time <- proc.time()
  result <- fun()
  result <- structure(result,
    starting.seed = seed,
    ending.seed   = structure(pack.sprng(), class="pseed"),
    time=structure(proc.time() - start.time, class = "proc_time"))
  if(cache){
    if(!file.exists(cache.dir)) dir.create(cache.dir)
    save(result, file=cache.file)
  }
  result
}

#' Call a function continuing the random number stream.
#' @param x an object
#' @param fun a function to call on object
#' @param ... passed onto function
#' @param cache use cache, see Caching in \code{link{harvestr}}
#' 
#' @seealso \code{\link{withpseed}}, \code{\link{harvest}}, and \code{\link{with}}
#' @export
reap <-
function(x, fun, ..., cache=FALSE){
  seed <- attr(x, "ending.seed")
  if(is.null(seed))
    stop("Could not find a seed value associated with x")
  if(cache){
    cache <- structure(cache, 
      expr.md5 = digest(list(x, fun, source="harvestr::reap"), "md5"))
  }
  withpseed(seed, fun(x,...), cache=cache)
}

#' Evaluate an expression for a set of seeds
#' @param seeds a list of seeds can be obtained though \code{\link{gather}}
#' @param expr an expression to evalutate with the different seeds.
#' @param envir an environment within which to evaluate \code{expr}.
#' @param .parallel should the computations be run in parallel?
#' @param cache use cache, see Caching in \code{link{harvestr}}
#' 
#' @importFrom plyr llply
#' @export
farm <-
function(seeds, expr, envir=parent.frame(), cache=FALSE, .parallel=FALSE){
  fun <- if(is.name(substitute(expr)) && is.function(expr)){
    stopifnot(is.null(formals(expr)))
    expr
  } else {
    if(cache){
      cache <- structure(cache, 
        expr.md5 = digest(substitute(expr), "md5"))
    }
    eval(substitute(function()expr), envir=envir)
  }
  llply(seeds, withpseed, fun, envir=environment(), cache=cache, .parallel=.parallel)
}


#' Harvest results
#' @param .list a list of \code{data.frame}s  See details.
#' @param fun a function to apply
#' @param ... passed to \code{fun}
#' @param cache use cache, see Caching in \code{link{harvestr}}
#' @param .parallel should the computations be run in parallel?
#' 
#' @details
#' harvest is functionaly equivalant to llply, but takes on additional capability when used
#' with the other functions from this package.  When an object comes from \code{\link{withpseed}}
#' the ending seed is extacted and used to continue evaluation.
#' 
#' @importFrom plyr mlply
#' @export
harvest <-
function(.list, fun, ..., cache=FALSE, .parallel=F){
  llply(.list, reap, fun, ..., cache=FALSE, .parallel=.parallel)
}

#' Strip attributes
#' @param x, any object
#' @export
noattr <- noattributes <- function(x){
  if(is.list(x)){
    x <- llply(x, noattributes)
  }
  attributes(x) <- NULL
  x
}

#' @S3method print pseed
#' @importFrom digest digest
print.pseed <- 
function(x, ...){
  cat(sprintf("<sprng parallel seed: MD5=%s>", digest(x)), ...)
  invisible(x)
}

#' Plant elements with seeds
#' @param .list a list to set seeds on
#' @param seeds to plant from \code{\link{gather}}
#'
#' For each element in list set an in dependent random seed.
#' This will replace and ending seeds values already set for the objects in the list.
#' 
#' @export
plant <-
function(.list, seeds=gather(length(.list))){
  stopifnot(is.list(.list))
  n <- length(.list)
  stopifnot(n == length(seeds))
  for(i in seq_len(n)){
    attr(.list[[i]],'ending.seed') <- seeds[[i]]
  }
  return(.list)
}

