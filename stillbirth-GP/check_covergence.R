

mcmc.array.mod <- mod.gp$BUGSoutput$sims.array

pdf_name4 <- paste0("fig/convergence_gp.pdf")
pdf(pdf_name4, width = 40, height = 20)
par(mfrow=c(3,3))
PlotTrace("rho", mcmc.array.mod) 
PlotTrace("sigma.g", mcmc.array.mod)
PlotTrace("a.r[1]", mcmc.array.mod) 
PlotTrace("a.r[2]", mcmc.array.mod) 
PlotTrace("a.r[3]", mcmc.array.mod) 
PlotTrace("beta0", mcmc.array.mod)
PlotTrace("beta2", mcmc.array.mod)
PlotTrace("beta3", mcmc.array.mod)
PlotTrace("beta4", mcmc.array.mod)
PlotTrace("beta5", mcmc.array.mod)  
PlotTrace("beta6", mcmc.array.mod) 
PlotTrace("beta_datatype2", mcmc.array.mod)
PlotTrace("beta_datatype3", mcmc.array.mod)
PlotTrace("beta_datatype4", mcmc.array.mod)
PlotTrace("beta_datatype5", mcmc.array.mod)
PlotTrace("sdy.j[1]", mcmc.array.mod) 
PlotTrace("sdy.j[2]", mcmc.array.mod) 
PlotTrace("sdy.j[3]", mcmc.array.mod) 
PlotTrace("sdy.j[4]", mcmc.array.mod) 
PlotTrace("sdy.j[5]", mcmc.array.mod) 

dev.off()