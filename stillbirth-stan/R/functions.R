#' Write covariate matrices for jags input
#'
#' Write covariate matrices for jags input
#'
#' @param covar Covariate to be turned into matrix
#' @param estyears Estimation year. Default from 2000 to 2015.
#' @param dataset Dataset containing the covariate. Default using national_covar dataset.
#' @export
#' @return A matrix with column year and row value
#' @examples
#' covarMatrix("mean_edu")
#'
covarMatrix <-function (covar,
                        estyears=seq(2000,2015),
                        dataset=national_covar){
  yearLength <- length(estyears)
  countryRegionList <- dataset[,c(1,8)] %>% distinct()
  CountryRegionList <- mutate(countryRegionList,country_idx=as.numeric(factor(countryRegionList$iso3)))
  numcoun<- length(countryRegionList$iso3)
  cMatrix<- matrix(ncol=yearLength,nrow=numcoun)
  dataset$ln_lbw = log(dataset$lbw_final)
  for (i in 1:numcoun){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(dataset[dataset$iso3 == countryRegionList$iso3[i] & dataset$year== estyears[j],covar])
    }
  }
  return (cMatrix)
}


#' Add credible intervals to plots
#'
#' Add credible intervals to plots
#' @param CI.low.t Lower bound for seq.years.t
#' @param CI.up.t Upper bound for seq.years.t
#' @param seq.years.t Sequence of years.
#' @param col Default = 1.
#' @export
#' @return A matrix with column year and row value
#' @examples
#' GetCIs(dataset_list, estimates_name, lower = 0.025, upper = 0.975)
#'
AddCIs <- function(CI.low.t, # lower bound for seq.years.t
                   CI.up.t, # upper bound for seq.years.t
                   seq.years.t, col = 1){
  # add CIs to a plot.
  col = adjustcolor(col, alpha.f = 0.1)
  for (t in 2:length(seq.years.t))
    polygon(c(seq.years.t[t-1], seq.years.t[t-1], seq.years.t[t], seq.years.t[t],seq.years.t[t-1]),
            c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t], CI.low.t[t], CI.low.t[t-1]),
            col=col, border = NA)
}

#' Create trace plot for a parameter
#'
#' Create trace plot for one parameter and add loess smoother for each chain
#' @param parname Parameter name
#' @param mcmc.array Needs to be a 3-dimensional array
#' @param n.chains Default = NULL
#' @param n.sims Default = NULL
#' @param main Default = NULL
#' @export
#' @return A trace plot for specified parameter
#'
#'
PlotTrace <- function(
  parname, mcmc.array,
  n.chains= NULL, n.sim= NULL, main = NULL){
  if (is.null(main)) main <- parname
  if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
  if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
  plot(c(mcmc.array[,1,parname]), type = "l", ylab = parname,  main = main,
       ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname])))
  for (chain in 1:n.chains){
    lines(c(mcmc.array[,chain,parname]), type = "l", col = chain)
  }
  for (chain in 1:n.chains){
    curve(predict(loess(c(mcmc.array[,chain,parname])~seq(1,n.sim)),x), lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
  }
}

#' Plot histogram of posterior sample
#'
#' Plot histogram of posterior sample
#' @param post.same Posterior sample
#' @param parname Parameter name. Default = NULL
#' @export
#' @return A histogram of posterior sample
#'
#'
PlotPostOnly <- function(
  post.samp, parname = NULL##<<used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
}

#' Plot histogram of posterior sample
#'
#' Plot histogram of posterior sample and add normal prior
#' @param post.same Posterior sample
#' @param priormean Prior's mean
#' @param priorsd Prior's standard deviation
#' @param parname Parameter name. Default = NULL
#' @export
#' @return Posterior sample plot with prior
#' @example
#' PlotPostWithNormalPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "bla")
#'
PlotPostWithNormalPrior <- function(
  post.samp, priormean, priorsd, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  curve(dnorm(x, mean = priormean, sd = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}


#' Plot histogram of posterior sample with  prior
#'
#' Plot histogram of posterior sample and add uniform prior
#' @param post.same Posterior sample
#' @param priorlow Prior's lower bound
#' @param priorup Prior's upper bound
#' @param parname Parameter name. Default = NULL
#' @export
#' @return Posterior sample plot with prior
#' @example
#' PlotPostWithUnifPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "bla")
#'
PlotPostWithUnifPrior <- function(
  post.samp, priorlow, priorup, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "",xlim = c(minx, maxx))
  h <- 1/(priorup-priorlow)
  segments(priorlow, h, priorup, h, col = 2)
}

#' Create directory
#'
#' Create output directory
#' @param runname Name
#' @export
#' @return Output directory
#'
MakeDirs <- function(runname){
  output.dir <- outputdir <- paste0(getwd(), "/output/", runname, "/")
  dir.create("output/", showWarnings = FALSE)
  dir.create(outputdir, showWarnings = FALSE)
  return(output.dir)
}

#' Create logit function
#'
#' @param x Input
#' @export
#' @return logit transformation on input
#'
logit <- function(x){
  log(x/(1-x))
}

#' Create inverse logit function
#'
#' @param y Input
#' @export
#' @return inverse logit transformation on input
#'
inverselogit <- function(y) 1/(1+exp(-y))

#' Create Gett.i
#'
#' @param years.i
#' @param years.t
#' @export
#' @return getti.i
#'
Gett.i <- function(years.i, year.t ){
  gett.i <- rep(NA, length(years.i))
  for (i in 1:length(years.i)){
    gett.i[i] <- which(year.t == years.i[i])
  }
  return(gett.i)
}


#' Create Getc.i
#'
#' @param iso.i
#' @param iso.t
#' @export
#' @return getc.i
#'
Getc.i <- function(iso.i, iso.c){
  getc.i <- rep(NA, length(iso.i))
  for (i in 1:length(iso.i)){
    getc.i[i] <- which(iso.c == iso.i[i])
  }
  return(getc.i)
}


#-------------
# The End!
