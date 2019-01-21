#' BevertonHolt function
#'
#' Performs all steps in order to fit the BeretonHolt population growth model, show the figures, and return the asked for output
#'
#' @param dd Dataframe with columns for population size data (colname=popsize), time data (colname=time) and unique identifiers for each population (colname=ident)
#' @param K.prior Prior value for the mean carrying capacity (K). Must be on log scale and numeric
#' @param Ksd.prior Prior value for the standard deviation on carrying capacity (K). Must be on log scale and numeric
#' @param r0.prior Prior value for the mean intrinsic rate of growth (r0). Must be on log scale and numeric
#' @param r0sd.prior Prior value for the standard deviation on intrinsic rate of growth (r0). Must be on log scale and numeric
#' @param d.prior Prior value for the mean death rate (d). Must be on log scale and numeric
#' @param d.prior Prior value for the mean death rate (d). Must be on log scale and numeric
#' @param N0.prior Prior value for the mean starting population size (N0). Must be on log scale and numeric
#' @param N0sd.prior Prior value for the standard deviation on starting population size (N0). Must be on log scale and numeric
#' @param sdev.prior Prior value for the standard deviation for the model fitting. Must be numeric. Defaults to 1.
#' @param cores Optional argument detailing the number of cores to use. Must be integer. Defaults to NA, in which case all available cores - 1 will be used
#' @param iter Number of iterations to run for the model, defaults to 1e4. Must be integer
#' @param warmup Number of iterations to run warmup. Defaults to 1e3. Must be integer and smaller than warmup
#' @param chains Number of chains to run for each fit. Must be integer. Defaults to 1
#' @param graphname Path with filename to save the figure showing the fits of the data with the posterior predictions. Defaults to NA, in which the plot will not be saved
#' @param outputtype Type of output that will be returned. Must be "summary", "full" or "both". summary return the summarised posteriors (mean and sd on log scale), full returns dataframe containing lists of all the posterior samples, both returns a list with both the summary and full output. Defaults to summary
#' @keywords Beverton Holt Bayesian parallel model fitting
#' @export
#' @import tidyverse
#' @import rstan
#' @import loo
#' @import deSolve
#' @import coda
#' @import parallel
#' @examples
#' BevertonHolt()
BevertonHolt <- function(dd, K.prior, Ksd.prior, r0.prior, r0sd.prior, d.prior, dsd.prior, N0.prior, N0sd.prior, sdev.prior=1,
                         cores=NA, iter=1e4, warmup=1e3, chains=1, graphname=NA, outputtype="summary"){
  #0) Check input, raise error if input is unsuitable and load packages
  error <- CheckInput(dd, K.prior, Ksd.prior, r0.prior, r0sd.prior, d.prior, dsd.prior, N0.prior, N0sd.prior, sdev.prior,
                      cores, iter, warmup, chains, graphname, outputtype)
  if (!is.na(error)){stop(error)}
  LoadPackages()

  #1) Generate the stan code to run Bayesian population growth model
  #) One needs to input prior information for all the parameters
  BH.code <- GenerateBHcode(K.prior, Ksd.prior, r0.prior, r0sd.prior, d.prior, dsd.prior, N0.prior, N0sd.prior, sdev.prior=sdev.prior)

  #2) Determine the number of cores that is available to run the code
  cores.to.use <- GetCores(cores)

  #3) Run the Bayesian model, parallellized for the different populations/cultures
  fulloutput <- BHfitting(BH.code=BH.code, dd=dd, K.prior, r0.prior, d.prior, N0.prior, sdev.prior=sdev.prior, cores.to.use, iter, warmup, chains)

  #4) Summarize the data to get parameter distributions (mean and sd) on a log scale
  sumoutput <- SummarizePosteriors(fulloutput)

  #5) Generate plot to check fits of data
  PlotFits(fulloutput, dd, filename=graphname)

  #6) Return output (summary, full, both)
  return(ReturnOutput(outputtype, fulloutput=fulloutput, sumoutput=sumoutput))
}
