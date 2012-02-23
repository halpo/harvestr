withpseed <-
function(seed, expr, envir=parent.frame()){
  stopifnot(require("rsprng"))
  oldseed <- get.seed()
  on.exit(free.sprng())
  on.exit(replace.seed(oldseed), add=T)
  unpack.sprng(seed)
  eval(substitute(expr), envir=envir)
}
