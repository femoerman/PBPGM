#' GenerateBHcode function
#'
#' Generates the stan code to be used for the models, basd on the prior information provided. This allows to only compile one model for all data, saving a lot of computing time.
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
#' @keywords Beverton Holt Stan code
#' @export
#' @examples
#' GenerateBHcode()
GenerateBHcode <- function(K.prior, Ksd.prior, r0.prior, r0sd.prior, d.prior, dsd.prior, N0.prior, N0sd.prior, sdev.prior){
  part1 <- '
  // function that caclulates BH population growth
  functions{
  real[] odemodel(real t, real[] N, real[] p, real[] x_r, int[] x_i){
  // p[1]=r0, p[2]=d, p[3]=K
  real dNdt[1];
  dNdt[1] = ((p[1] + p[2])/(1 + ((p[1]/(p[3]*p[2])) * N[1])) -
  p[2])*N[1];
  return dNdt;
  }
  }

  data{
  int n;
  real log_N0;
  real log_N[n];
  real t0;
  real t[n];
  }

  transformed data {
  // not used here
  real x_r[0];
  int x_i[0];
  }

  parameters{
  real log_r;
  real log_d;
  real log_K;
  real log_N0sim;
  real<lower=0> sdev;
  }


  transformed parameters{
  // all of this was in the model section previously
  // I moved it here to be able to get waic because Nsim needs to be accessible in "generated quantities {}"

  real p[3];
  real Nsim[n,1]; // simulated values, matrix. dim1 = time, dim2 = dim_ODE = 1
  real N0sim_dummy[1]; // just a dummy because the ODE solver requires real[] instead of real

  // parameters for integrator
  p[1] = exp(log_r);
  p[2] = exp(log_d);
  p[3] = exp(log_K);
  N0sim_dummy[1] = exp(log_N0sim);  // see above

  // integrate ODE (maybe try: integrate_ode_bdf()??)
  Nsim = integrate_ode_rk45(odemodel,N0sim_dummy,t0,t,p,x_r,x_i);
  }

  model{
  // priors
  // note: it can be VERY helpful to estimate parameters on logscale,
  // especially if they have different orders of magnitude. here it works on regular scale, though.'
  part2 <- paste("log_r ~ normal(", r0.prior, ", ", r0sd.prior,");", sep="")
  part3 <- paste("log_K ~ normal(", K.prior, ", ", Ksd.prior,");", sep="")
  part4 <- paste("log_d ~ normal(", d.prior, ", ", dsd.prior,");", sep="")
  part5 <- paste("log_N0sim ~ normal(", N0.prior, ", ", N0sd.prior,");", sep="")
  part6 <- paste("sdev ~ cauchy(0, ", sdev.prior,");", sep="")
  part7 <-
  'sdev ~ cauchy(0,1);

  // likelihood, normal (maybe lognormal helpful)
  log_N0 ~ normal(log_N0sim,sdev);
  for (i in 1:n){
  log_N[i] ~ normal(log(Nsim[i,1]),sdev);
  }
  }

  // cacluate log lik to get waic
  // from loo R package description
  generated quantities {
  real log_lik[n];
  for (nn in 1:n){
  log_lik[nn] = normal_lpdf(log_N[nn] | log(Nsim[nn,1]), sdev);
  }
  }
  '

  BH.code <- paste(part1, part2, part3, part4, part5, part6, part7, sep="\n")
  return(BH.code)
}
