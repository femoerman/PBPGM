% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReturnOutput.R
\name{ReturnOutput}
\alias{ReturnOutput}
\title{RetrunOutput function}
\usage{
ReturnOutput(outputtype, fulloutput, sumoutput)
}
\arguments{
\item{outputtype}{Type of output that will be returned. Must be "summary", "full" or "both". summary return the summarised posteriors (mean and sd on log scale), full returns dataframe containing lists of all the posterior samples, both returns a list with both the summary and full output. Defaults to summary}

\item{fulloutput}{Output generated in the BHfitting function, containing all the posterior data from the model fitting}

\item{sumoutput}{summarised output generated in the SummarizePosteriors function}
}
\description{
Returns the requested output to the user
}
\examples{
ReturnOutput()
}
\keyword{Model}
\keyword{Population}
\keyword{growth}
\keyword{output}
\keyword{return}
