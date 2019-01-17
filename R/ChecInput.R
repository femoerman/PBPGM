#' CheckInput function
#'
#' Checks the user input variables, and creates an error if it encounters a problem for fitting the models
#' @param dd Dataframe with columns for population size data (colname=popsize), time data (colname=time) and unique identifiers for each population (colname=ident)
#' @param K.prior Prior value for the mean carrying capacity (K). Must be on log scale and numeric
#' @param Ksd.prior Prior value for the standard deviation on carrying capacity (K). Must be on log scale and numeric
#' @param r0.prior Prior value for the mean intrinsic rate of growth (r0). Must be on log scale and numeric
#' @param r0sd.prior Prior value for the standard deviation on intrinsic rate of growth (r0). Must be on log scale and numeric
#' @param d.prior Prior value for the mean death rate (d). Must be on log scale and numeric
#' @param r0sd.prior Prior value for the standard deviation on death rate (d). Must be on log scale and numeric
#' @param N0.prior Prior value for the mean starting population size (N0). Must be on log scale and numeric
#' @param N0sd.prior Prior value for the standard deviation on starting population size (N0). Must be on log scale and numeric
#' @param sdev.prior Prior value for the standard deviation for the model fitting. Must be numeric. Defaults to 1.
#' @param cores Optional argument detailing the number of cores to use. Must be integer. Defaults to NA, in which case all available cores - 1 will be used
#' @param iter Number of iterations to run for the model, defaults to 1e4. Must be integer
#' @param warmup Number of iterations to run warmup. Defaults to 1e3. Must be integer and smaller than warmup
#' @param chains Number of chains to run for each fit. Must be integer. Defaults to 1
#' @param graphname Path with filename to save the figure showing the fits of the data with the posterior predictions. Defaults to NA, in which the plot will not be saved
#' @param outputtype Type of output that will be returned. Must be "summary", "full" or "both". summary return the summarised posteriors (mean and sd on log scale), full returns dataframe containing lists of all the posterior samples, both returns a list with both the summary and full output. Defaults to summary
#' @keywords Beverton Holt input check
#' @export
#' @examples
#' CheckInput()
CheckInput <- function(dd, K.prior, Ksd.prior, r0.prior, r0sd.prior, d.prior, dsd.prior, N0.prior, N0sd.prior, sdev.prior,
           cores, iter, warmup, chains, graphname, outputtype){
  error <- NA
  if(!is.data.frame(dd)){error <- "Faulty data input, data must be a data frame with the variables time, popsize and ident"}
  else if(!"time" %in% names(dd)){ error<- "Variable time missing in dataframe"}
  else if(!"popsize" %in% names(dd)){ error<- "Variable time missing in dataframe"}
  else if(!"time" %in% names(dd)){ error<- "Variable time missing in dataframe"}
  else if (!is.numeric(K.prior) | !is.integer(K.prior)){error <- "K.prior is not a numeric value"}
  else if (!is.numeric(Ksd.prior) | !is.integer(Ksd.prior)){error <- "Ksd.prior is not a numeric value"}
  else if (!is.numeric(r0.prior) | !is.integer(r0.prior)){error <- "r0.prior is not a numeric value"}
  else if (!is.numeric(r0sd.prior) | !is.integer(r0sd.prior)){error <- "r0sd.prior is not a numeric value"}
  else if (!is.numeric(d.prior) | !is.integer(d.prior)){error <- "d.prior is not a numeric value"}
  else if (!is.numeric(dsd.prior) | !is.integer(dsd.prior)){error <- "dsd.prior is not a numeric value"}
  else if (!is.numeric(N0.prior) | !is.integer(N0.prior)){error <- "N0.prior is not a numeric value"}
  else if (!is.numeric(N0sd.prior) | !is.integer(N0sd.prior)){error <- "N0sd.prior is not a numeric value"}
  else if (!is.numeric(sdev.prior) | !is.integer(sdev.prior)){error <- "sdev.prior is not a numeric value"}
  else if (!is.integer(cores)){error <- "cores must be an integer value"}
  else if (!is.integer(iter)){error <- "iter must be an integer value"}
  else if (!is.integer(warmup)){error <- "warmup must be an integer value"}
  else if (warmup>iter){error <- "warmup must be smaller than iter"}
  else if (!is.integer(chains)){error <- "chains must be an integer value"}
  else if (!is.character(graphname)){error <- "graphname must be avalid path (character)"}
  else if (!is.character(outputtype)){error <- "outputtype must be either 'full', 'summary' or 'both' character value"}
  else if (!outputtype  %in% c("both", "full", "summary")){error <- "outputtype must be either 'full', 'summary' or 'both character value"}
  return(error)
}
