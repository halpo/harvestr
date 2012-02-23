withseed <-
function(seed, expr, envir=parent.frame()){
  oldseed <- get.seed()
  on.exit(replace.seed(oldseed))
  set.seed(seed)
  eval(substitute(expr), envir=envir)
}
