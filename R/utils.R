#' Use a reference class method
#' @param method name of the method to call
#' @param ... additional arguments to pass along
#' 
#' @seealso \link{ReferenceClasses}
#' @return a function that calls the designated meethod
#' @example inst/examples/use_method.R
#' @export
use_method <- function(method, ...){
  method <- as.character(substitute(method))
  function(x){
    fun <- do.call(`$`, list(x, method))
    fun(...)
  }
}
