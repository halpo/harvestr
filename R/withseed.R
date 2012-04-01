#' Do a computation with a given seed.
#' @rdname seed_funs
#' @param seed  a valid seed value
#' @param expr expression to evaluate.
#' @param envir the \code{\link{environment}} to evaluate the code in. 
#' 
#' @details
#' Compute the expr with the given seed, replacing the global seed after compuatations
#' are finished.
#' 
#' does not replace the global .Random.seed
#' @note Not parallel compatible, this modifies the global environment, while processing.
#' @seealso \code{\link{set.seed}}
#' @export
withseed <- function(seed, expr, envir=parent.frame()){
  oldseed <- get.seed()
  on.exit(replace.seed(oldseed))
  set.seed(seed)
  structure(eval(substitute(expr), envir=envir),
    starting.seed = seed,
    ending.seed   = .Random.seed)
}


#' safe version of retrieving the .Random.seed
#' @rdname seed_funs
#' @return the .Random.seed if defined, otherwise NULL
#' @export
get.seed <- function(){
  if(exists(".Random.seed", envir=.GlobalEnv, mode="numeric")) {
    seed <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
  } else {
    seed <- NULL
  }
  seed
}

#' @rdname seed_funs
#' @param delete logical to delete if \code{seed} is null.
#' @details 
#' Replaces the .Random.seed with seed unless seed is null, then it will 
#' delete the .Random.seed if delete=T
#' @export
replace.seed <- function(seed, delete=TRUE){
  if(is.null(seed)){
    remove(.Random.seed, envir=.GlobalEnv)
  } else {
    assign('.Random.seed', seed, envir=.GlobalEnv) 
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
#' @export
GetOrSetSeed<-function(){
  if(is.null(get.seed())) runif(1)
  seed <- .Random.seed
	seed
}

