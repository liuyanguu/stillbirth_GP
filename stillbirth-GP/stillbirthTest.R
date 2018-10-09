setwd("E:/doc/research/birth rate/New folder/stillbirths-master/")
library(haven)
SBR_input<-read_dta("data/SBR_input.dta",encoding='latin1')
library(tidyverse)
library(distortr)
library(R2jags)  
library(fields)
library(splines)
library(boot)
library(RColorBrewer)


library(dplyr)
rjags::load.module('dic')
national_covar <- read_dta("data/national_covariates.dta",encoding='latin1')

reported_loess <- read_dta("data/reported_loess.dta",encoding='latin1')

SBR_results<-read_dta("data/SBR_results.dta",encoding='latin1')


#nationalCovar_countries<-unique (national_covar$iso3)
#nationalCovar_countries
#SBR_countries<-unique (SBR_input$iso3)
#SBR_countries
#loess_countries<-unique (reported_loess$iso3)
#loess_countries

#withoutLoess<-SBR_input %>% 
#  filter(!iso3 %in% loess_countries)

#withoutLoess_countries <-unique(withoutLoess$iso3)

#all.equal(withoutLoess_countries,SBR_countries)

#test <-cbind(withoutLoess_countries,SBR_countries)



# to see the source mean of each country
#avg_source<-aggregate(withoutLoess[,13], list(withoutLoess$iso3), mean)
#source1 <- arrange(avg_source, avg_source$n_context5)
##################

# filter SBR_input. Keep the data of year 2000-2015
SBR_input<-filter(SBR_input, year>=2000 & year<=2015)

# get a list of country and its matching region use iso3

countryRegionList <- national_covar[,c(1,8)]

countryRegionList<-distinct(countryRegionList)
countryRegionList

##get the numeric representation of the country at index i
countryRegionList$country_fac <- as.numeric(factor(countryRegionList$iso3))
countryRegionList<-countryRegionList[order(countryRegionList$country_fac),]
# connect SBR_input countries with national_covar countries 

totalIndex_SBR = length(SBR_input$iso3)
country_SBR = as.vector(SBR_input$iso3)

totalIndex_covar = length(countryRegionList$iso3)
totalIndex_covar
country_covar= as.vector(countryRegionList$iso3)
country_num_covar = as.vector(countryRegionList$country_fac)
getc.i <- rep (NA, totalIndex_SBR)


for (i in 1: totalIndex_SBR){
  for (j in 1:totalIndex_covar){
    if (country_SBR[i] == country_covar[j]){   
      getc.i[i]<-country_num_covar[j]
    }
  }
}

## get the year at index i, get numeric 1,2,3,4
estyears<-seq(2000,2015)
year_SBR<-as.vector(SBR_input$year)

gett.i<-rep(NA, totalIndex_SBR)
for (i in 1: totalIndex_SBR){
  gett.i[i]<- year_SBR[i]-estyears[1]+1
}


## get the region of a specif country

getr.c <- countryRegionList$shmdg2

# create a c by t matrix for each covariate 
yearStart = min(national_covar$year)
yearStart
yearEnd = max(national_covar$year)
yearEnd
yearLength = yearEnd-yearStart +1
## for edu
edu_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
# for (i in 1:totalIndex_covar){
#   for ( j in 1: yearLength){
#     edu_matrix[i,j] <- as.numeric(national_covar[national_covar$country == country_covar[i] & national_covar$year== seq(yearStart,yearEnd)[j],"mean_edu"])
#   }
# }
totalIndex_covar
covarMatrix <-function (cMatrix, covar){
  for (i in 1:totalIndex_covar){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(national_covar[national_covar$iso3 == country_covar[i] & national_covar$year== seq(yearStart,yearEnd)[j],covar])
    }
  }
  return (cMatrix)
}

edu_matrix = covarMatrix(edu_matrix,"mean_edu")

## for data type
#dataType_matrix =matrix(nrow=totalIndex_covar,ncol=yearLength)
#dataType_matrix= covarMatrix(dataType_matrix,"n_context5")

## it has different levels of data type (I used sbr_input data for the model.)


## for ANC
anc_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
anc_matrix = covarMatrix(anc_matrix,"anc4")

## for NMR
nmr_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
nmr_matrix = covarMatrix(nmr_matrix,"ln_nmr")

## for GNI
gni_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
gni_matrix = covarMatrix(gni_matrix,"ln_gni")

## for LBW
national_covar$ln_lbw = log(national_covar$lbw_final)
lbw_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
lbw_matrix = covarMatrix(lbw_matrix,"ln_lbw")

## for region

#region_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
#region_matrix = covarMatrix(region_matrix,"shmdg2")



# fit the model
totalObs_SBR = nrow(SBR_input)

totalRegion = length(unique(countryRegionList$shmdg2))

y.i<- SBR_input$ln_sbr


#creating dummies for datatype
datatype.nm<-as.integer(SBR_input$n_context5)

N=totalIndex_SBR
dummy_test <- function(SBR_context5,context) {
  N<-length(SBR_context5)
  dummy_datatype<-rep(0, N)
  for(i in 1:N){
    if (SBR_context5[i]==context){
      dummy_datatype[i]<-1
    } 
  }
  return(dummy_datatype)
}


dummy_datatype2.i <- dummy_test(SBR_input$n_context5,2)
dummy_datatype3.i <- dummy_test(SBR_input$n_context5,3)
dummy_datatype4.i<-dummy_test(SBR_input$n_context5,4)
dummy_datatype5.i<-dummy_test(SBR_input$n_context5,5)


getj.i <-SBR_input$n_context5

jags.data<- list(  y.i = y.i, getj.i=getj.i, getc.i = getc.i, getr.c = getr.c, gett.i= gett.i, dummy_datatype5.i = dummy_datatype5.i, dummy_datatype2.i=dummy_datatype2.i, dummy_datatype3.i=dummy_datatype3.i,dummy_datatype4.i=dummy_datatype4.i,
                   totalObs_SBR = totalObs_SBR, totalIndex_covar=totalIndex_covar, totalRegion= totalRegion, yearLength= yearLength, edu_matrix= edu_matrix, gni_matrix=gni_matrix, lbw_matrix= lbw_matrix, nmr_matrix=nmr_matrix, anc_matrix= anc_matrix)

parnames <- c("mu.ct","a.r","z.i","tauy.j","beta0","beta2","beta3","beta4","beta5","beta6","beta_datatype2","beta_datatype3","beta_datatype4","beta_datatype5")
set.seed(1234)

## mod0 & mod 1 adding non-constant variance using jags.parallel and jags. (different outputs as well)
##use two variances tauy.i
## mod2 constant variance

mod0 <-jags.parallel(data = jags.data, 
                     parameters.to.save=parnames, 
                     model.file = "model/stillbirth_model test.txt", n.chains = 3, n.iter = 12000, n.burnin = 3000, n.thin=9)


#mod1 <-jags(data = jags.data, 
#            parameters.to.save=parnames, 
#            model.file = "/Users/Dingdang/Desktop/wd/stillbirth_dingdang/stillbirth_repo/model/stillbirth_model test.txt", n.chains = 3, n.iter = 12000, n.burnin = 3000, n.thin=9)


mod2 <-jags.parallel(data = jags.data, 
                     parameters.to.save=parnames, 
                     model.file = "model/stillbirth_model.txt", n.chains = 3, n.iter = 3000+3000, n.burnin = 3000)

#max(mod0$BUGSoutput$summary[, c("Rhat")])
#min(mod0$BUGSoutput$summary[, c("n.eff")])
#mcmc.array <- mod0$BUGSoutput$sims.array
output3<-round(mod0$BUGSoutput$summary[, c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat")],5)
output1<-round(mod1$BUGSoutput$summary[, c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat")],5)
output2<-round(mod2$BUGSoutput$summary[, c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat")],5)

View(output3) 
View(output1)## either way get better a.r and better betas

mcmc.array<- mod0$BUGSoutput$sims.array

#mcmc.array<- mod2$BUGSoutput$sims.array

#max(mod0$BUGSoutput$summary[, c("Rhat")])
#min(mod0$BUGSoutput$summary[, c("n.eff")])
#mcmc.array <- mod0$BUGSoutput$sims.array
#round(mod0$BUGSoutput$summary[, c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat")],5)

##### graphs 

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

percentiles = c(0.025, 0.5, 0.975)
#construct CIs for all countries (of all of the observations in 2000-2015)
CIs.cqt <- array(NA, c(totalIndex_covar,3, yearLength))
for (t in 1:yearLength){
  #  CIs.cqt[,,t] <- InvLogit((mod$BUGSoutput$summary[paste("logitpi.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]))
  CIs.cqt[,,t] <- mod0$BUGSoutput$summary[paste("mu.ct[", seq(1, totalIndex_covar), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
}  
CIs.cqt<- exp(CIs.cqt)
y.i=exp(y.i)
pdf_name <- paste0("fig/graphsCT.pdf")
pdf(pdf_name, width = 7, height = 5)

select_c <- order(countryRegionList$iso3)
sbr_c <- which(unique(countryRegionList$iso3) %in% sbr_countries)


#select_c <- order(unique(getc.i))
estyears <- seq(yearStart, yearEnd)
for (c in sbr_c){
  par(cex.axis=0.5, cex.main = 1.5, cex.lab=1.5, cex.sub=1.5)
  plot(1, type="n",
       #xlim = range(estyears),
       xlim = c(2000,2015),
       xaxp  = c(2000, 2015, 15),
       ylab = "Stillbirth Rate", xlab = "Time", 
       main = country_covar[c], 
       #ylim = range(y.i[getc.i==c], CIs.cqt[c,,]), 
       ylim = range(0,60),
       lwd = 5, pch = 19, cex = 1.5)
  AddCIs(CI.up.t = CIs.cqt[c,3,], CI.low.t = CIs.cqt[c,1,], seq.years.t = estyears, col = 2)
  lines(CIs.cqt[c,2,] ~ estyears, col = 2, lwd = 3)
  axis(1, at = 2000:2015, tck = 1, lty = 2, col = "grey", labels = NA)
  if (sum(getc.i==c)>0){
    points((y.i[getc.i==c]) ~ estyears[gett.i[getc.i==c]], lwd = 3, pch = 19,
           col = ifelse(dummy_datatype2.i[getc.i==c]==1,'light green',ifelse(dummy_datatype3.i[getc.i==c]==1,'orange',ifelse(dummy_datatype4.i[getc.i==c]==1,'blue',ifelse(dummy_datatype5.i[getc.i==c]==1,'maroon','dark green')))), cex=0.8)
    
  }
  legend("topright", 
         legend = c("High National Subroutine", "Low National Subroutine","National Survey","Sub-national","Other"), 
         col = c('dark green','light green','orange','blue','maroon'), 
         pch = 19, 
         bty = "n", 
         pt.cex = .6, 
         cex = .6, 
         text.col = "black", 
         horiz = F
         
  )   
}
dev.off()

mcmc.array <- mod0$BUGSoutput$sims.array

# hyperparameters
PlotTrace <- function(#Traceplot for one parameter
  ### Trace plot for one parameter and add loess smoother for each chain
  parname, mcmc.array,##<< needs to be 3-dimensional array!
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


pdf_name4 <- paste0("fig/convergence.pdf")
pdf(pdf_name4, width = 40, height = 20)
par(mfrow=c(3,3))
PlotTrace("a.r[1]", mcmc.array) 
PlotTrace("a.r[2]", mcmc.array) 
PlotTrace("a.r[3]", mcmc.array) 
PlotTrace("beta0", mcmc.array)
PlotTrace("beta2", mcmc.array)
PlotTrace("beta3", mcmc.array)
PlotTrace("beta4", mcmc.array)
PlotTrace("beta5", mcmc.array)  
PlotTrace("beta6", mcmc.array) 
PlotTrace("beta_datatype2", mcmc.array)
PlotTrace("beta_datatype3", mcmc.array)
PlotTrace("beta_datatype4", mcmc.array)
PlotTrace("beta_datatype5", mcmc.array)
PlotTrace("tauy.j[1]", mcmc.array) 
PlotTrace("tauy.j[2]", mcmc.array) 
PlotTrace("tauy.j[3]", mcmc.array)
PlotTrace("tauy.j[4]", mcmc.array)
PlotTrace("tauy.j[5]", mcmc.array)
dev.off()

# some example of region and country specific parameters
pdf_name1 <- paste0("fig/mu.ct[c,12].pdf")
pdf(pdf_name1, width = 40, height = 20)
par(mfrow=c(2,2))
for (country in 1:totalIndex_covar) PlotTrace(paste0("mu.ct[", country, ",12]"), mcmc.array)
dev.off()

pdf_name2 <- paste0("fig/z.i[1109].pdf")
pdf(pdf_name2, width = 40, height = 20)
#par(mfrow=c(2,2))
PlotTrace("z.i[1109]", mcmc.array)
dev.off()

# pdf_name2 <- paste0("mu.ct_Year2.pdf")
# pdf(pdf_name2, width = 40, height = 20)
# par(mfrow=c(2,2))
# for (country in 1:totalIndex_covar) PlotTrace(paste0("mu.ct[", country, ",2]"), mcmc.array)
#  dev.off()
# # 
# pdf_name3 <- paste0("mu.ct_Year3.pdf")
# pdf(pdf_name3, width = 40, height = 20)
# par(mfrow=c(2,2))
# for (country in 1:totalIndex_covar) PlotTrace(paste0("mu.ct[", country, ",3]"), mcmc.array)
# dev.off()


################## model diag

#----
# estimates of intercepts and slopes
## no checking for betak and a.c 

#----
# model fit, residuals
y.i <-log(y.i)
muhat.i <- rep(NA, totalIndex_SBR)
for (i in 1:totalIndex_SBR) muhat.i[i] <- quantile(c(mcmc.array[,,
                                                                paste0("mu.ct[", getc.i[i], ",", gett.i[i], "]")]), 0.5)
zhat.i <- rep(NA, totalIndex_SBR)
for (i in 1:totalIndex_SBR) zhat.i[i] <- quantile(c(mcmc.array[,,
                                                               paste0("z.i[",i,"]")]), 0.5)

res.i <- y.i - muhat.i-zhat.i
yhat.i<- muhat.i+zhat.i

##individual observation function
x.i<-function(varmatrix){
  varname.i<-rep(NA,totalIndex_SBR)
  for (i in 1:totalIndex_SBR) {varname.i[i]<-varmatrix[getc.i[i],gett.i[i]]}
  varname.i
}
edu.i<-x.i(edu_matrix)
anc.i<-x.i(anc_matrix)
lbw.i<-x.i(lbw_matrix)
nmr.i<-x.i(nmr_matrix)
gni.i<-x.i(gni_matrix)

##non-function writeup
# edu.i <- rep(NA, totalIndex_SBR)
# for (i in 1:totalIndex_SBR) edu.i[i] <- edu_matrix[getc.i[i], gett.i[i]]
# 
# anc.i <- rep(NA, totalIndex_SBR)
# for (i in 1:totalIndex_SBR) anc.i[i] <- anc_matrix[getc.i[i], gett.i[i]]
# 
# lbw.i <- rep(NA, totalIndex_SBR)
# for (i in 1:totalIndex_SBR) lbw.i[i] <- lbw_matrix[getc.i[i], gett.i[i]]
# 
# nmr.i <- rep(NA, totalIndex_SBR)
# for (i in 1:totalIndex_SBR) nmr.i[i] <- nmr_matrix[getc.i[i], gett.i[i]]
# 
# gni.i <- rep(NA, totalIndex_SBR)
# for (i in 1:totalIndex_SBR) gni.i[i] <- gni_matrix[getc.i[i], gett.i[i]]


getr.i <- getr.c[getc.i]
# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/residuals against predictors,yhat,time.pdf")
pdf(pdf_name3, width = 7, height = 4)
par(mfrow=c(1,2))
#plot(res.i~edu.i+anc.i+lbw.i+nmr.i+gni.i, col = getr.i)
plot(res.i~edu.i, col = getr.i, cex=1)
curve(predict(loess(res.i~edu.i),x), add = T, col = 2, lwd= 3)
abline(h=0)

plot(res.i~anc.i, col = getr.i,cex=1)
curve(predict(loess(res.i~anc.i),x), add = T, col = 2, lwd= 3)
abline(h=0)

plot(res.i~lbw.i, col = getr.i,cex=1)
curve(predict(loess(res.i~lbw.i),x), add = T, col = 2, lwd= 3)
abline(h=0)

plot(res.i~nmr.i, col = getr.i,cex=1)
curve(predict(loess(res.i~nmr.i),x), add = T, col = 2, lwd= 3)
abline(h=0)

plot(res.i~gni.i, col = getr.i,cex=1)
curve(predict(loess(res.i~gni.i),x), add = T, col = 2, lwd= 3)
abline(h=0)


plot(res.i~ yhat.i, col = getr.i,cex=1)
curve(predict(loess(res.i ~ yhat.i),x), add = T, col = 2, lwd= 3)
abline(h=0)

plot(res.i~seq(yearStart, yearEnd)[gett.i], col=getr.i,cex=1)
curve(predict(loess(res.i~seq(yearStart, yearEnd)[gett.i]),x), add = T, col = 2, lwd= 3)
abline(h=0)
dev.off()




### find r squred? (not sure if this is the correct method)  
##no improvement after adding variance dependency
sqrt_res.i<-rep(NA, totalIndex_SBR)
for (i in 1: totalIndex_SBR){
  sqrt_res.i[i] <- res.i[i]^2
}

RSS<- sum(sqrt_res.i)

ybar <- mean(y.i)

SS<-(y.i-ybar)^2
TSS<-sum(SS)

r2 <- 1-RSS/TSS

bayes_R2 <- function(fit) {
  y <- rstanarm::get_y(fit)
  ypred <- rstanarm::posterior_linpred(fit, transform = TRUE)
  if (family(fit)$family == "binomial" && NCOL(y) == 2) {
    trials <- rowSums(y)
    y <- y[, 1]
    ypred <- ypred %*% diag(trials)
  }
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}

library(rstanarm)
M1<-stan_glm(y.i ~ edu.i+anc.i+lbw.i+nmr.i+gni.i, data=SBR_input)
print(median(bayes_R2(M1)))

r2.bayes<-print(median(bayes_R2(M1)))



#' 
#' Compare priors and posteriors
#' 
## ------------------------------------------------------------------------
# again some functions
PlotPostWithNormalPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priormean, priorsd, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  #minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  #maxx<-0.5
  minx<- -0.5
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  curve(dnorm(x, mean = priormean, sd = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithNormalPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "bla")
pdf_name4 <- paste0("fig/nmrBetaPostAndPri.pdf")
pdf(pdf_name4, width = 7, height = 4)
PlotPostWithNormalPrior(post.samp = c(mcmc.array[,,"beta5"]), priormean = 0, priorsd = 1/sqrt(0.001), parname = "nmr_coef")
dev.off()
##not using rnorm, posterior samples are given by jags
#it's just a super flat prior lol

