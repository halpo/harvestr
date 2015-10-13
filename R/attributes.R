#' Strip attributes from an object.
#' 
#' @param x, any object
#' @seealso \link{attributes}
#' @export
noattr <- noattributes <- function(x) {
  if(is.list(x)){
    x <- llply(x, noattributes)
  }
  attributes(x) <- NULL
  x
}

#' Retrieve an attribute or a default if not present.
#' 
#' Behaves similar to \code{\link{getOption}}, but is a simple wrapper
#' for \code{\link{attr}}.
#' 
#' 
#' @param object    An R Object.
#' @param name      Name of the Attribute
#' @param default   The value if the attribute is not set or NULL
#' 
#' @export
getAttr <- function(object, name, default=NULL){
    a <- attr(object, name)
    if(is.null(a)) 
        return(default)
    else 
        return(a)
}
