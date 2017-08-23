is_harvestr_frame <- function(env){
    par <- parent.env(env)
    if(!exists(".packageName", envir=par, inherits=FALSE)) return(FALSE)
    par$.packageName == "harvestr"
}

is_top_harvestr_call <-
function(n=0){
    frames <- head(sys.frames(), -1-n)
    sum(sapply(frames, is_harvestr_frame))==1
}

#' @importFrom foreach getDoParRegistered
dflt_harvestr_parallel <- 
function(){
    if(is_top_harvestr_call()) 
        stop("dflt_harvestr_parallel should not be called directly.")
    if(is_top_harvestr_call(1))
        return(getDoParRegistered())
    frames <- sys.frames()
    harvestr.frames <- Filter(is_harvestr_frame, frames)
    has.parallel <- sapply(harvestr.frames, exists, x=".parallel", where=-1, inherits=FALSE)
    if(!any(has.parallel)) return(FALSE)
    return(max(get(envir=harvestr.frames[has.parallel][[1]], ".parallel") - 1, 0))
}
test_dflt_harvestr_parallel <- function(.parallel=0, nest=.parallel){
    if(nest>0) return(Recall(.parallel=.parallel, nest= nest-1))
    dflt_harvestr_parallel()
}


dflt_harvestr_progress <-
function( is.interactive = Interactive()
        , OS             = .Platform$OS.type
        , is.top.call    = is_top_harvestr_call(1)
        ){
    if( is.interactive && is.top.call){
        if( OS == "windows") 
            return("win")
        else return("time")
    }
    return("none")
}
test_dflt_harvestr_progress <- function(...){dflt_harvestr_progress(...)}

dflt_harvestr_time      <- function(){ FALSE }
dflt_harvestr_cache     <- function(){ FALSE }
dflt_harvestr_cache_dir <- function(){"harvestr-cache"}
dflt_harvestr_use_try   <- function(){!interactive()}

defaults <- 
list( parallel  = dflt_harvestr_parallel
    , progress  = dflt_harvestr_progress
    , time      = dflt_harvestr_time
    , cache     = dflt_harvestr_cache
    , cache.dir = dflt_harvestr_cache_dir
    , use.try   = dflt_harvestr_use_try
    )
