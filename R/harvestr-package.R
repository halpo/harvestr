#' \code{harvestr} package
#' @name harvestr
#' @aliases package-harvestr harvestr
#' @docType package
#' @aliases harvestr package-harvestr
#' @title A Simple Reproducible Parallel Simulation Framework
#' @author Andrew Redd <amredd_at_gmail.com>
#' 
#' The harvestr package is a framework for parallel reproducible simulations based off the package
#' \code{\link[rsprng:spawn.new.sprng]{rsprng}}.
#' The functions to know about are:
#' \enumerate{
#'   \item \code{\link{gather}} - which gathers parallel seeds.
#'   \item \code{\link{farm}}  - which uses the saved seeds from gather to replicate an expression,
#'         once for each seed.
#'   \item \code{\link{harvest}} -  which uses objects from farm, that have saved seed attributes, 
#'         to continue evaluation from where farm finished.
#'   \item \code{\link{reap}} - is used by harvest for a single item
#'   \item \code{\link{plant}} - is used to set seeds for a list of predefined objects so that harvest
#'         can be used on it.
#' }
#' 
#' @section Caching:
#' The functions in \code{harvestr} can cache results for faster and
#' interuptible simulations.  This option defaults to \code{FALSE} but can be 
#' chosen by specifying the \code{cache} parameter in any of the functions
#' that produce results.
#' 
#' The caching is performed by saving a RData file in a specified caching
#' directory.  The default directory is named "harvestr-cache" and resides 
#' under the \link[base:getwd]{working directory}.  This can be specified by setting
#' the \code{harvestr.cache.dir} \code{\link{option}}.  Files in this directory 
#' use file names derived from hashes of the expression to evaluate.  Do not
#' modify the file names.
#' 
NULL