is_harvestr_frame <- function(env){
    par <- parent.env(env)
    if(!exists(".packageName", envir=par, inherits=FALSE)) return(FALSE)
    par$.packageName == "harvestr"
}

is_top_harvestr_call <-
function(){
    frames <- sys.frames()
    any(!sapply(frames, is_harvestr_frame))
}

dflt_harvestr_parallel <- 
function(){
    if(is_top_harvestr_call())
        return(getDoParRegistered())
    frames <- sys.frames()
    harvestr.frames <- filter(is_harvestr_frame, frames)
    has.parallel <- sapply(harvestr.frames, exists, x=".parallel", where=-1, inherits=FALSE)
    return(get(envir=harvestr.frames[has.parallel][[1]], ".parallel") - 1)
}

dflt_harvestr_progress <-
function(){
    if(interactive() && is_top_harvestr_call()){
        if(.Platform$OS.type == "windows") return("win")
        else return("time")
    }
    return("none")
}

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
