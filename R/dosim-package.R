#' \code{dosim} package
#' @name package-dosim
#' @title Simulation framework
#' @author Andrew Redd <amredd_at_gmail.com>
#' 
#' The dosim package is a framework for parallel replicable simulations based off the package
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
#'}
#' 
NULL