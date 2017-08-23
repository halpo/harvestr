#' Test if a function was called from others.
#' 
#' @param ...   functions to pass
#' @param FUNS  functions as a list
#' 
#' @export
called_from <- 
function( ...  #< functions to consider
        , FUNS=list(...) #< passed as a list
        ){
    stopifnot( missing(FUNS) || length(list(...)) == 0 )
    if(any(.i <- sapply(FUNS, is.character))){
        FUNS[.i] <- lapply(FUNS[.i], match.fun)
    }
    stopifnot(all(sapply(FUNS, is.function)))
    n <- sys.nframe()
    for(i in 0:n){
        if(any(sapply(FUNS, identical, sys.function(i))))
            return(TRUE)
    }
    return(FALSE)
}

#' @describeIn called_from Determine if the function is called while knitting a document
#' @export
is_knitting <-
function(){
    "Determine if the function is called while knitting a document"
    if("knitr" %in% .packages(all.available = TRUE)){
        called_from(knitr::knit)
    } else FALSE
}


#' Smarter interactive test
#' 
#' This is a smarter version of \code{\link{interactive}},
#' but also excludes cases inside knit or in startup 
#' \code{\link{.First}}, or others specified in dots.
#' You can also specify functions to exclude in the option
#' \code{harvestr::Interactive::exclude}
#' 
#' @param exclude.calls functions to pass to \code{\link{called_from}}
#' 
#' @export
Interactive <-
function(exclude.calls=getOption("harvestr::Interactive::exclude")){
    Interactive_core(exclude.calls=exclude.calls)
}

Interactive_core <- 
function( is.interactive = interactive()
        , is.knitting    = is_knitting()
        , exclude.calls  = getOption("harvestr::Interactive::exclude")
        ){
    is.interactive &&
    !is.knitting && 
    !called_from(.First.sys) && 
    if(!is.null(exclude.calls)){
        !called_from(FUNS=exclude.calls)
    } else TRUE
}
