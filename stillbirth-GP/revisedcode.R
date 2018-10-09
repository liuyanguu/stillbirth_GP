rm(list=ls())
setwd("E:/doc/research/birth rate/New folder/stillbirths-GP/")
library(haven)
library(tidyverse)
library(distortr)
library(R2jags)  
library(fields)
library(splines)
library(boot)
library(RColorBrewer)
library(dplyr)
rjags::load.module('dic')

#############################################################
##########            Data import           #################
#############################################################

SBR_input<-read_dta("data/SBR_input.dta",encoding='latin1')
national_covar <- read_dta("data/national_covariates.dta",encoding='latin1')
reported_loess <- read_dta("data/reported_loess.dta",encoding='latin1')


#############################################################
##########        Data clean for jags       #################
#############################################################

# filter SBR_input. Keep the data of year 2000-2015
SBR_input<-filter(SBR_input, year>=2000 & year<=2015)

#  list of country, its matching region, and a index 
countryRegionList <- national_covar[,c(1,8)] %>% distinct()  
CountryRegionList <- mutate(countryRegionList,country_idx=as.numeric(factor(countryRegionList$iso3)))

# getc.i: the country index of ith observation
getc.i<- merge(SBR_input,CountryRegionList,by="iso3")$country_idx

# gett.i: the time index of ith obervation
estyears<-seq(2000,2015)
gett.i<- SBR_input$year-estyears[1]+1

# the region given country_idx
getr.c <- countryRegionList$shmdg2

# Store each covariate infomration in c*t matrix 
yearLength = max(gett.i)+1
 #function for creating matrix
numcoun<- length(countryRegionList$iso3)
country_covar<- as.vector(countryRegionList$iso3)
covarMatrix <-function (covar){
  cMatrix<- matrix(ncol=yearLength,nrow=numcoun)
  for (i in 1:numcoun){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(national_covar[national_covar$iso3 == country_covar[i] & national_covar$year== estyears[j],covar])
    }
  }
  return (cMatrix)
}

## 5 covariates
edu_matrix = covarMatrix("mean_edu")
anc_matrix = covarMatrix("anc4")
nmr_matrix = covarMatrix("ln_nmr")
gni_matrix = covarMatrix("ln_gni")
national_covar$ln_lbw = log(national_covar$lbw_final)
lbw_matrix = covarMatrix("ln_lbw")


#creating dummies for datatype

dummy_datatype2.i <- ifelse(SBR_input$n_context5==2,1,0)
dummy_datatype3.i <- ifelse(SBR_input$n_context5==3,1,0)
dummy_datatype4.i <- ifelse(SBR_input$n_context5==4,1,0)
dummy_datatype5.i <- ifelse(SBR_input$n_context5==5,1,0)
getj.i <-SBR_input$n_context5

##
totalObs_SBR = nrow(SBR_input)
totalRegion = length(unique(countryRegionList$shmdg2))
totalIndex_covar = length(countryRegionList$iso3)

y.i<- SBR_input$ln_sbr

jags.data<- list(  y.i = y.i, getj.i=getj.i, getc.i = getc.i, getr.c = getr.c, gett.i= gett.i, 
                   dummy_datatype2.i=dummy_datatype2.i, dummy_datatype3.i=dummy_datatype3.i,dummy_datatype4.i=dummy_datatype4.i,dummy_datatype5.i = dummy_datatype5.i,
                   totalObs_SBR = totalObs_SBR, totalIndex_covar=totalIndex_covar, totalRegion= totalRegion,
                   yearLength= yearLength, edu_matrix= edu_matrix, gni_matrix=gni_matrix, lbw_matrix= lbw_matrix, nmr_matrix=nmr_matrix, anc_matrix= anc_matrix)

parnames <- c("mu.ct","a.r","z.i","tauy.j","beta0","beta2","beta3","beta4","beta5","beta6","beta_datatype2","beta_datatype3","beta_datatype4","beta_datatype5","mu.g","G.tc","sigma")
## mod0 & mod 1 adding non-constant variance using jags.parallel and jags. (different outputs as well)
##use two variances tauy.i
## mod2 constant variance

mod0 <-jags.parallel(data = jags.data, 
                     parameters.to.save=parnames, 
                     model.file = "model/stillbirth_ar.txt", n.chains = 3, n.iter = 12000, n.burnin = 3000, n.thin=9)

mod1 <-jags.parallel(data = jags.data, 
                     parameters.to.save=parnames, 
                     model.file = "model/stillbirth_gp.txt", n.chains = 3, n.iter = 12000, n.burnin = 3000, n.thin=9)
