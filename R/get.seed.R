get.seed <-
function(){
  if(exists(".Random.seed", envir=.GlobalEnv, mode="numeric")) {
    seed <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
  } else {
    seed <- NULL
  }
  seed
}
