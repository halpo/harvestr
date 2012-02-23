replace.seed <-
function(seed, delete=TRUE){
  if(is.null(seed)){
    remove(.Random.seed, envir=.GlobalEnv)
  } else {
    assign('.Random.seed', seed, envir=.GlobalEnv) 
  }
}
