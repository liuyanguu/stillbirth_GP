mod.basic <- readRDS(file ="output/mod.basic.rds")
mod.basic.array <- as.array(mod.basic)
mod.basic.bf <- as.data.frame(mod.basic)

plot.dat <- get_plot_dat(mod=mod.basic,
                         jags.data = jags.data,
                         alpha=0.05)

summary(mod.basic, pars=c("beta_r[1]"), probs = c(0.025, 0.25, 0.50, 0.75, 0.975),
        use_cache = F)$summary[1]


percentiles = c(0.025, 0.5, 0.975)
#construct CIs for all countries (of all of the observations in 2000-2015)
totalIndex_covar <- jags.data$totalIndex_covar
yearLength <- jags.data$yearLength
y_i <- jags.data$y_i
CIs.cqt <- array(NA, c(totalIndex_covar,3, yearLength))
for (t in 1:yearLength){
  #  CIs.cqt[,,t] <- InvLogit((mod$BUGSoutput$summary[paste("logitpi.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]))
  CIs.cqt[,,t] <- summary(mod.basic,pars=c(paste("mu_ct[", seq(1, totalIndex_covar), ",", t, "]", sep = "")))$summary[,c(4,6,8)]

}  
CIs.cqt<- exp(CIs.cqt)
y_i=exp(y_i)
pdf_name <- paste0("fig/graphsCT_basic.pdf")
pdf(pdf_name, width = 7, height = 5)


select_c <- order(national_covar$iso3)
SBR_countries<-unique (SBR_input$iso3)
sbr_c <- which(unique(national_covar$iso3) %in% SBR_countries)
estyears <- jags.data$estyears
getc.i <- jags.data$getc_i
gett.i <- jags.data$gett_i
dummy_datatype2.i <-jags.data$dummy_datatype2_i
dummy_datatype3.i <-jags.data$dummy_datatype3_i
dummy_datatype4.i <-jags.data$dummy_datatype4_i
dummy_datatype5.i <-jags.data$dummy_datatype5_i

for (c in sbr_c){
  par(cex.axis=0.5, cex.main = 1.5, cex.lab=1.5, cex.sub=1.5)
  plot(1, type="n",
       #xlim = range(estyears),
       xlim = c(2000,2015),
       xaxp  = c(2000, 2015, 15),
       ylab = "Stillbirth Rate", xlab = "Time", 
       main = jags.data$country_covar[c], 
       #ylim = range(y.i[getc.i==c], CIs.cqt[c,,]), 
       ylim = range(0,60),
       lwd = 5, pch = 19, cex = 1.5)
  AddCIs(CI.up.t = CIs.cqt[c,3,], CI.low.t = CIs.cqt[c,1,], seq.years.t = estyears, col = 2)
  lines(CIs.cqt[c,2,] ~ estyears, col = 2, lwd = 3)
  axis(1, at = 2000:2015, tck = 1, lty = 2, col = "grey", labels = NA)
  if (sum(getc.i==c)>0){
    points((y_i[getc.i==c]) ~ estyears[gett.i[getc.i==c]], lwd = 3, pch = 19,
           col = ifelse(dummy_datatype2.i[getc.i==c]==1,'light green',ifelse(dummy_datatype3.i[getc.i==c]==1,'orange',ifelse(dummy_datatype4.i[getc.i==c]==1,'blue',ifelse(dummy_datatype5.i[getc.i==c]==1,'maroon','dark green')))), cex=0.8)
    
  }
  legend("topright", 
         legend = c("High National Subroutine", "Low National Subroutine","National Survey","Sub-national","Other"), 
         col = c('dark green','light green','orange','blue','maroon'), 
         pch = 19, 
         bty = "n", 
         pt.cex = .6, 
         cex = .6, 
         text.col = "black", 
         horiz = F
  )   
}
dev.off()
