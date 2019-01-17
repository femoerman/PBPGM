#' GetCores function
#'
#' Calculates the available cores based on the user input and machine available cores
#'
#' @param cores Optional argument detailing the number of cores to use. Must be integer. Defaults to NA, in which case all available cores - 1 will be used
#' @keywords Beverton Holt Stan code
#' @export
#' @examples
#' GetCores()
GetCores <- function(cores){
  if (is.na(cores)){
    cores.to.use <- detectCores()-1
    return(cores.to.use)
  }
  else if (is.integer(cores)){
    cores.to.use <- min(detectCores()-1, cores)
    return(cores.to.use)
  }
  else{
    stop("cores object must be an integer")
  }
}
