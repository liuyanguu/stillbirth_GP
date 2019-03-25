
library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

basic <- rstan::stan(file= "models/SBR_basic.stan",data=jags.data,chains = 4)
saveRDS(basic , file ="output/mod.basic.rds")

center_basic <- rstan::stan(file= "models/centerSBR_basic.stan",data=stan.data,chains = 4)
saveRDS(center_basic,file="output/mod.centerbasic.rds")

center_basic_hs <- rstan::stan(file= "models/centerSBR_basic_hs.stan",data=stan.data,chains = 4)
saveRDS(center_basic_hs,file="output/mod.centerbasic_hs.rds")

center_AR1_hs <- rstan::stan(file= "models/AR1_hs.stan",data=stan.data,chains = 4)
saveRDS(center_AR1_hs,file="output/mod.centerAR1_hs.rds")

ar <- rstan::stan(file= "models/SBR_AR1.stan",data=jags.data,chains = 4)
saveRDS(ar , file ="output/mod.ar.rds")

#util <- new.env()
#source('stan_utility.R', local=util)
#source('plot_utility.R', local=util)