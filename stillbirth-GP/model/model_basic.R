parnames.to.save<-c( 
 "mu.ct","z.i","a.r","sdy.j","beta0","beta2","beta3","beta4","beta5","beta6","beta_datatype2","beta_datatype3",

        "beta_datatype4","beta_datatype5","yhat.i","yrep.i","loglike.i" 
)
mod.basic<-jags.parallel(data = jags.data,
                                  parameters.to.save=parnames.to.save, 
                                  model.file ="model/model_basic.txt", n.chains = 3, n.iter = 12000, n.burnin = 3000, n.thin=9)
    saveRDS(mod.basic, file ="output/mod.basic.rds")
 # end file 
