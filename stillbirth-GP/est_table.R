
est.list<- c("rho","sigma.g","a.r[1]","a.r[2]","a.r[3]","beta0","beta_datatype2","beta_datatype3",
              "beta_datatype4","beta_datatype5","sdy.j[1]","sdy.j[2]","sdy.j[3]","sdy.j[4]","sdy.j[5]")

check_est_table(mod=mod.basic, 
                est.list = est.list,
                want = c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat"),
                round=4)

check_est_table(mod=mod.ar1MVN, 
                est.list = est.list,
                want = c("mean", "sd", "2.5%","50%", "97.5%", "n.eff", "Rhat"),
                round=4)

check_est_table(mod=mod.gp, 
                est.list = est.list,
                want = c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat"),
                round=4)




ll.basic <- t(do.call("cbind", lapply(1:dim(mod.basic$BUGSoutput$sims.list[["loglike.i"]])[2],
                                      function(i) mod.basic$BUGSoutput$sims.list[["loglike.i"]][i,])))


ll.ar1MVN <- t(do.call("cbind", lapply(1:dim(mod.ar1MVN$BUGSoutput$sims.list[["loglike.i"]])[2],
                                       function(i) mod.ar1MVN$BUGSoutput$sims.list[["loglike.i"]][i,])))

ll.gp <-t(do.call("cbind", lapply(1:dim(mod.gp$BUGSoutput$sims.list[["loglike.i"]])[2],
                                  function(i) mod.gp$BUGSoutput$sims.list[["loglike.i"]][i,])))

waic_basic <- waic(log_lik = ll.basic)$total['waic']; waic_basic
waic_ar1MVN <- waic(log_lik = ll.ar1MVN)$total['waic']; waic_ar1MVN
waic_gp <-waic(log_lik = ll.gp)$total['waic']; waic_gp
