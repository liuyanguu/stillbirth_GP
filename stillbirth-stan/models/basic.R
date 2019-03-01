
library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

basic <- rstan::stan(file= "models/SBR_basic.stan",data=jags.data,chains = 4)
saveRDS(basic , file ="output/mod.basic.rds")

center_basic <- rstan::stan(file= "models/centerSBR_basic.stan",data=jags.data,chains = 1,
                            iter = 100)
saveRDS(center_basic,file="output/mod.centerbasic.rds")

ar <- rstan::stan(file= "models/SBR_AR1.stan",data=jags.data,chains = 4)
saveRDS(ar , file ="output/mod.ar.rds")

