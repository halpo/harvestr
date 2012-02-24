#' Gather independent seeds.
#'
#' Equivalant to \code{\link[rsprng]{spawn.new.sprng}}
#' 
#' @importFrom rsprng
#' @export
gather.seeds <- 
function(n, seed, ...){
	spawn.new.sprng(n, seed)
}
