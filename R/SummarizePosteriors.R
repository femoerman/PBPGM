#' SumarizePosteriors function
#'
#' Function to summarize the posterior data
#'
#' @param fulloutput Output generated in the BHfitting function, containing all the posterior data from the model fitting
#' @keywords Beverton Holt output summary posteriors
#' @export
#' @examples
#' SummarizePosteriors()
SummarizePosteriors <- function(fulloutput){
  #1) Summarize the posterior data by calculating mean and standard deviations of all the parameters (on log scale)
  {
    sumoutput <- select(fulloutput, ident)
    sumoutput <- mutate(sumoutput, logr0.mean =NA, logr0.sd=NA, logK.mean=NA, logK.sd=NA, logalfa.mean=NA, logalfa.sd=NA, logd.mean=NA, logd.sd=NA)
    for (i in sumoutput$ident){
      temp <- filter(fulloutput, name==i)
      sumoutput[which(sumoutput$ident==i), ]$logr0.mean <- mean(log(unlist(temp2$all_r0_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logr0.sd <- sd(log(unlist(temp2$all_r0_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logK.mean <- mean(log(unlist(temp2$all_K_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logK.sd <- sd(log(unlist(temp2$all_K_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logalfa.mean <- mean(log(unlist(temp2$all_alfa_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logalfa.sd <- sd(log(unlist(temp2$all_alfa_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logd.mean <- mean(log(unlist(temp2$all_d_dist)))
      sumoutput[which(sumoutput$ident==i), ]$logd.sd <- sd(log(unlist(temp2$all_d_dist)))
    }
  }
  return(sumoutput)
}