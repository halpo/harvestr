GetOrSetSeed <-
function(){
  if(is.null(get.seed())) runif(1)
  seed <- .Random.seed
	seed
}
