#--------
# master.R 
# December 2018
#--------

# setup:
# main steps are in this master script
# project specific R functions are saved in the R subfolder 
# subscripts (that eventually are to be turned into R functions) are in sub_....R
# data are in data subfolder
# output per model in the output folder

#----
# user/run specific info
# define WD
workdir <- getwd()

# settings (default to FALSE to focus on fitting of the simplest model)

time.trend <- FALSE # do you want to add an AR(1) process? default is false (data were generated w/o it)



#-----
# setting the workdir, loading libraries sourcing functions
setwd(workdir)
# source the R functions in the functions subfolder 
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

source("sub_loadpackages.R")
source("sub_getjagsdata.R")

#---------------------------------------------
# read in data, process data to get jasgdata object 
SBR_input<-read_dta("input/SBR_input.dta",encoding='latin1')
SBR_input<-filter(SBR_input, year>=2000 & year<=2015)
national_covar <- read_dta("input/national_covariates.dta",encoding='latin1')
#reported_loess <- read_dta("input/reported_loess.dta",encoding='latin1')

# do.validation=FALSE, if true, most recent observation in each country is left out
jags.data <- get_jagsdata(SBR_input,national_covar,do.validation = FALSE)


# go to sub_exploratory for some overview plots

#---------------------------------------------------------------
# run a model

# save a model file (here using a function to allow for different options)
#writeModel(time.trend = F, file.name = "model/model_basic.txt")
#writeModel(time.trend = T, file.name = "model/model_ar1MVN.txt")

# parameters to save
#getParamsandOutput(time.trend = F, input.file.name = "model/model_basic.txt", output.file.name = "model/model_basic.R")
#getParamsandOutput(time.trend = T, input.file.name = "model/model_ar1MVN.txt", output.file.name = "model/model_ar1MVN.R")

# This is how to run the model, it takes a lot of time to get the result. And it will save 
# the output in "output/model_basic.rds" and "output/model_ar1MVN.rds"
# source("model/model_basic.R")
# source("model/model_ar1MVN.R")

# this is use to read the fake output
mod.basic <- readRDS(file ="output/mod.basic.rds")
mod.ar1MVN <- readRDS(file ="output/mod.ar1MVN.rds")
mod.gp <- readRDS(file ="output/mod.gp.rds")

# this is use to read the valdation jagsdata and fake output
jags.dataVAL <- get_jagsdata(SBR_input,national_covar,do.validation = TRUE)
mod.ar1MVNVAL <- readRDS(file ="output/mod.ar1MVNVAL.rds")


#-----------------------------------------------------------------
# go to sub_convergence for some convergenc checks
# PlotTrace() is learned from 730 course.


# check_est_table present the summary statistics for interested parameters(more than one)
est.list<-c("sdy.j[1]","sdy.j[2]","sdy.j[3]","sdy.j[4]","sdy.j[5]",
            "beta_datatype2","beta_datatype3","beta_datatype4","beta_datatype5")
check_est_table(mod=mod.ar1MVN, 
                est.list = est.list,
                want = c("mean", "sd", "2.5%","97.5%"),
                round=4)

# go to check_estimates for plots of the estimates

# go to check_biasadjustedandValidation for other plots and validations 

#------------------------------    Residual plots    --------------------------------------#

# It provides residual plots with loess line in a pdf file in "fig/residual"
#source("check_residual.R")
#------------------------------------------------------------------------------------------#


#-----
# The End!

