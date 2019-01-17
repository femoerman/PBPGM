#'RetrunOutput function
#'
#'Returns the requested output to the user
#'
#' @param outputtype Type of output that will be returned. Must be "summary", "full" or "both". summary return the summarised posteriors (mean and sd on log scale), full returns dataframe containing lists of all the posterior samples, both returns a list with both the summary and full output. Defaults to summary
#' @param fulloutput Output generated in the BHfitting function, containing all the posterior data from the model fitting
#' @param sumoutput summarised output generated in the SummarizePosteriors function
#' @keywords Beverton Holt return output
#' @export
#' @examples
#' ReturnOutput()
ReturnOutput <- function(outputtype, fulloutput, sumoutput){
  if(output=="summary"){
    return(sumoutput)
  }
  else if (output=="full"){
    return(fulloutput)
  }
  else if (output=="both"){
    return(list(sumoutput=sumoutput, fulloutput=fulloutput))
  }
  else {
    stop("Invalid output format, choose summary, full or both")
  }
}