#' LoadPackage function
#'
#' Performs all steps in order to fit the BeretonHolt population growth model, show the figures, and return the asked for output
#'
#' @keywords Beverton Holt Bayesian parallel model libraries
#' @export
#' @import tidyverse
#' @import rstan
#' @import loo
#' @import deSolve
#' @import coda
#' @import parallel
#' @examples
#' LoadPackages()
loadPackages <- function(){
  # load libraries
  library(rstan)
  library(loo)
  library(deSolve)
  library(coda)
  library(parallel)
  library(tidyverse)
}