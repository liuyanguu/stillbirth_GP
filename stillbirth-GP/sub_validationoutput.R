
#sub_validationoutput.R

# indices of test set
getitest <- setdiff(seq(1,jags.dataVAL$totalObs_SBR), jags.dataVAL$getitrain.k)

# errors
errors <- jags.dataVAL$y.i[getitest] - mod.ar1MVNVAL$BUGSoutput$summary[paste0("yrep.i[",getitest,"]"), "50%"]
hist(errors)
# summarize
mean(errors)  
median(errors)  
mean(abs(errors))
median(abs(errors))



ntest <- length(getitest)
# PIT
pit.j <- rep(NA, ntest)
for (j in 1:ntest){
  i <- getitest[j]
  yrepi.s <- mod.ar1MVNVAL$BUGSoutput$sims.matrix[,paste0("yrep.i[",i,"]")]
  pit.j[j] <- mean(yrepi.s <= jags.dataVAL$y.i[i])
} 
hist(pit.j, freq = F, xlab = "PIT-values", main = "Predicting last obs")  # should look uniform
#for (r in 1:30) lines(density(runif(ntest)), col = "grey")
abline(h=1)
# note that for this (simulated) dataset, % with pit > 0.9 is quite low (for other, it's a bit high)
mean(pit.j > 0.9)
# this is due to uncertainty in the outcome, based on number of points left out
# eg uncertainy in outcomes > 0.9
p <- 0.1
qbinom(c(0.1, 0.9), size = ntest, prob = p)/ntest # 80% PI for prop of left out obs in one bin of PIT values with range of p 
qbinom(c(0.25, 0.75), size = ntest, prob = p)/ntest # 50% PI for prop of left out obs in one bin of PIT values with range of p 
p <- 0.25
qbinom(c(0.1, 0.9), size = ntest, prob = p)/ntest # 80% PI for prop of left out obs in one bin of PIT values with range of p 
qbinom(c(0.25, 0.75), size = ntest, prob = p)/ntest # 50% PI for prop of left out obs in one bin of PIT values with range of p 

# coverage follows from pit
mean(pit.j < 0.025)# % below 95% PI
mean(pit.j < 0.05) # % below 90% PI
mean(pit.j < 0.1)
mean(pit.j > 0.975) # % above 95% PI 
mean(pit.j > 0.95) 
mean(pit.j > 0.9)



# plot data and estimates over time, and indicate which points were left out
percentiles = c(0.025, 0.5, 0.975)
CIs.cqt <- array(NA, c(C, 3, nyears))
for (t in 1:nyears){
  #  CIs.cqt[,,t] <- InvLogit((mod$BUGSoutput$summary[paste("logitpi.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]))
  CIs.cqt[,,t] <- mod$BUGSoutput$summary[paste("mu.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
}  
estyears <- seq(startyear, endyear)
pdf_name <- paste0("fig/modelval.pdf")
pdf(pdf_name, width = 7, height = 7)
# plot alphabetically
select_c <- order(name.c)
for (c in select_c){
  plot(1, type="n",
       xlim = range(estyears),
       ylab = "mu", xlab = "Time", 
       main = name.c[c], 
       ylim = range(y.i[getc.i==c], CIs.cqt[c,,]), 
       lwd = 5, pch = 19)
  AddCIs(CI.up.t = CIs.cqt[c,3,], CI.low.t = CIs.cqt[c,1,], seq.years.t = estyears, col = 2)
  lines(CIs.cqt[c,2,] ~ estyears, col = 2, lwd = 3)
  if (sum(getc.i==c)>0){
    points((y.i[getc.i==c]) ~ estyears[gett.i[getc.i==c]], lwd = 3, pch = 19, 
           col = ifelse(is.element(which(getc.i==c), getitest), "grey", 2))
  }
}
dev.off()
