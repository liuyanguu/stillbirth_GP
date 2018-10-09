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
dim(SBR_input)
se=rep(0,dim(SBR_input)[1])
SBR_input$se=se
new.data <- processData(d = SBR_input, iso.column = "iso3",
                        data.column = "ln_sbr",
                        se.column="se",
                        obsyear.column = "year",
                        region.column= "shmdg2",
                        source.column = "n_context5",
                        start.year = 2000,
                        end.year = 2015)

for(i in 1:length(new.data)){
  assign(names(new.data)[i], new.data[[i]])
}


cs.smoothing <- TRUE
nserror.estimated <- TRUE

# if non-sampling error is to be inputted, sigma.y must be a vector
# of values of non-sampling error for each source type.
# for example, there are three source types with ANC4 (survey, admin, other), so could put
# input.data$sigma.y <- c(0.2, 0.1, 0.5)
# if non-sampling error is to be estimated, sigma.y can just be NA
new.data$sigma.y <- NA

## JAGS parameters (can be changed if not converging - set to be relatively fast)
nchains = 3
nburnin = 1000
niter = 2000
nthin = 1

time.trend <- FALSE

# need extra data related to correlation matrix for GP model
Dist.c <- getGPData(nyears.c, niso, cov.method = "sqexp")
new.data$Dist.c <- Dist.c

mod_gpex <- runMCMC(method = "gp",
                    matern.cov = FALSE,
                    input.data = new.data,
                    cs.smoothing = cs.smoothing,
                    time.trend = time.trend,
                    nserror.estimated = nserror.estimated,
                    nchains = nchains,
                    nburnin = nburnin,
                    niter = niter,
                    nthin = nthin,
                    model.save.file.path = "model_gpex.txt")