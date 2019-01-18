#' PlotFits function
#'
#' Plots the posterior predictions on the actual data so model fit can be checked visually
#'
#' @param fulloutput Output generated in the BHfitting function, containing all the posterior data from the model fitting
#' @param dd Dataframe with columns for population size data (colname=popsize), time data (colname=time) and unique identifiers for each population (colname=ident)
#' @param graphname Path with filename to save the figure showing the fits of the data with the posterior predictions. Defaults to NA, in which the plot will not be saved
#' @keywords Beverton Holt plot model fit
#' @export
#' @examples
#' PlotFits()
PlotFits <- function(fulloutput, dd, filename){
  #1) determine dimensions of output graph
  all <- length(unique(fulloutput$ident))
  height=ceiling(sqrt(all))
  width=ceiling(all/height)
  frames <- width*height

  #2) general plot settings
  x11(width=width,height=height)
  par(mfrow=c(width,height),bty="l",mar=rep(0.5,4),oma=c(4,4,0.5,0.5))

  ##############################################################################
  #3) Plot all figures
  for (i in fulloutput$ident){
    temp <- fulloutput[which(fulloutput$ident==i), ]
    if(!is.na(temp$timesim)){
      # plot current growth dynamics
      plot(unlist(temp$act_densities) ~ unlist(temp$act_times), pch=16, type="n", lty=3, ylab="Population density, N (indiv./mL)", xlab="Time, t (h)",ylim=c(0,max(dd$popsize)), xlim=c(0,max(dd$time)) ,col="darkblue",xaxt="n",yaxt="n")

      if(is.element(temp$cnt,seq(1,frames,height))) axis(side=2)
      if(temp$cnt >= frames) axis(side=1)

      figure <- points(unlist(temp$act_densities) ~ unlist(temp$act_times), pch=16, type="b", lty=3, col="black")
      polygon(x = c(unlist(temp$timesim),rev(unlist(temp$timesim))), y= c(unlist(temp$act_2.5), rev(unlist(temp$act_97.5))),border=F,col=adjustcolor("black",alpha.f=0.3))
      lines(unlist(temp$Nsim)~unlist(temp$timesim),col="grey40",lwd=2)
      title(main=temp$ident)
    } else {
      figure <- plot(unlist(temp$act_densities) ~ unlist(temp$act_times), pch=16, type="n", lty=3, ylab="Population density, N (indiv./mL)", xlab="Time, t (h)",ylim=c(0,max(dd$popsize)), xlim=c(0,max(dd$time)) ,col="darkblue",xaxt="n",yaxt="n")
      if(is.element(temp$cnt,seq(1,frames,height))) axis(side=2)
      if(temp$cnt >= frames) axis(side=1)
      title(main=temp$ident)

    }
  }

  #4) afulloutput$ident axis labels
  mtext(side=1,at=0.5,outer=T,"Time, t (h)",line=2)
  mtext(side=2,at=0.5,outer=T,"Population density, N (indiv./mL)",line=2)
  ###############################################################################
  # save plot 1
  if(!is.na(filename)){
    dev.copy2pdf(file=filename)
  }

}
