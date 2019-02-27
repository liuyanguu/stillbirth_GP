
#library(SBRgroup)
library("rjags")
library("R2jags")
library('dplyr')
library(tidyr)
#library(ggplot2) # we had issues using ggplot and bayesplot in combination

library(loo)
# use bayesplot for model checking etc
# note that just installing bayesplot from cran didn't work for me, I had to get it from github
# you need to have devtools installed for that
# and some other dependencies may not work, eg first install 
#install.packages(c("reshape2", "ggridges", "rstanarm"))
#devtools::install_github("stan-dev/bayesplot", dependencies = TRUE, build_vignettes = TRUE)
library(bayesplot)
library(rstantools) # needed for PIT_loo
library(haven)
library(tidyverse)
#library(distortr)
library(fields)
library(splines)
library(boot)
library(RColorBrewer)
library(dplyr)
rjags::load.module('dic')
